-- AudioDBTest - A test framework for the audioDB system
--
-- Copyright (C) 2015 Richard Lewis, Goldsmiths' College
-- Author: Richard Lewis <richard.lewis@gold.ac.uk>

-- This file is part of AudioDBTest.

-- AudioDBTest is free software: you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation, either version 3 of
-- the License, or (at your option) any later version.

-- AudioDBTest is distributed in the hope that it will be
-- useful, but WITHOUT ANY WARRANTY; without even the implied warranty
-- of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with AudioDBTest. If not, see <http://www.gnu.org/licenses/>.

module AudioDB.Test where

import Data.Yaml (ParseException)
import AudioDB.Test.Types
import AudioDB
import ADB
import Foreign.C.String
import Data.DateTime
import System.Info as Sys
import Data.List (sortBy)

configurePointQuery :: QueryConf -> ADBDatumPtr -> IO QueryAllocator
configurePointQuery conf qDatum = return $
  mkPointQuery qDatum (qc_npoints conf)

configureTrackQuery :: QueryConf -> ADBDatumPtr -> IO QueryAllocator
configureTrackQuery conf qDatum = return $
  mkTrackQuery qDatum (qc_npoints conf) (qc_ntracks conf)

configureSequenceQuery :: QueryConf -> ADBDatumPtr -> FeatureRate -> IO QueryAllocator
configureSequenceQuery conf qDatum secToFrames = return $
  mkSequenceQuery qDatum secToFrames (qc_npoints conf) (qc_ntracks conf) (qc_start conf) (qc_length conf) (mkDistance (qc_distance conf)) (mkAbsPower (qc_absoluteThreshold conf))

configureNSequenceQuery :: QueryConf -> ADBDatumPtr -> FeatureRate -> IO QueryAllocator
configureNSequenceQuery conf qDatum secToFrames = return $
  mkNSequenceQuery qDatum secToFrames (qc_npoints conf) (qc_ntracks conf) (qc_start conf) (qc_length conf) (mkDistance (qc_distance conf)) (mkAbsPower (qc_absoluteThreshold conf))

mkDistance :: Distance -> Maybe DistanceFlag
mkDistance DotProduct                = Just dotProductFlag
mkDistance EuclideanNormed           = Just euclideanNormedFlag
mkDistance Euclidean                 = Just euclideanFlag
mkDistance KullbackLeiblerDivergence = Just kullbackLeiblerDivergenceFlag

mkAbsPower :: Double -> Maybe Double
mkAbsPower 0.0 = Nothing
mkAbsPower x   = Just x

prepareQuery :: Query -> Maybe ADBDatumPtr -> IO QueryAllocator
prepareQuery _ Nothing = error "Query features not found."
prepareQuery q (Just qDatum)
  | qtype == PointQuery     = configurePointQuery conf qDatum
  | qtype == TrackQuery     = configureTrackQuery conf { qc_accumulation = AccumPerTrack } qDatum
  | qtype == SequenceQuery  = configureSequenceQuery conf { qc_accumulation = AccumPerTrack } qDatum featureRate
  | qtype == NSequenceQuery = configureNSequenceQuery conf { qc_accumulation = AccumOneToOne } qDatum featureRate
  where qtype = (qo_type . q_query) q
        featureRate = (db_featureRate . q_db) q
        qo = q_query q
        conf = QueryConf { qc_key = (qo_key qo)
                         , qc_start = (qo_start qo)
                         , qc_length = (qo_length qo)
                         , qc_exhaustive = False
                         , qc_allowFalsePositives = False
                         , qc_accumulation = AccumDB
                         , qc_distance = (qo_distance qo)
                         , qc_npoints = (qo_pointNN qo)
                         , qc_ntracks = (qo_resultLen qo)
                         , qc_includeKeys = []
                         , qc_excludeKeys = []
                         , qc_radius = (qo_radius qo)
                         , qc_absoluteThreshold = (qo_absoluteThreshold qo) // 0.0
                         , qc_relativeThreshold = (qo_relativeThreshold qo) // 0.0
                         , qc_durationRatio = 1.0
                         , qc_queryHopSize = (qo_sequenceHop qo)
                         , qc_datumHopSize = (qo_sequenceHop qo) }

runQuery :: Query -> IO QueryResult
runQuery q = withExistingROAudioDB dbFileName withDB
  where
    dbFileName = (db_fileName . q_db) q
    key = (qo_key . q_query) q
    withDB (Just adb) = do
      let frameSize = (db_frameSize . q_db) q

      -- FIXME withMaybeDatumPtr is giving me an IO (IO
      -- QueryAllocator) so I'm unwrapping its return value
      -- twice. This is silly and should be fixed.
      qFunc   <- withMaybeDatumPtr adb key (\qDatum -> prepareQuery q qDatum)
      qf      <- qFunc
      results <- execQuery adb qf

      let rankings        = sortBy cmpDistance (extractRankings frameSize results)
          cmpDistance a b = compare (rk_distance a) (rk_distance b)
          fMeasure        = evaluate rankings q

      return QueryResult { qr_query_id = q_identifier q
                         , qr_results  = rankings
                         , qr_fMeasure = fMeasure }
    withDB Nothing = error $ "Could not open database " ++ dbFileName

extractRankings :: FrameSize -> ADBQueryResults -> [Ranking]
extractRankings framesToSecs r = map resToRank (query_results_results r)
  where
    resToRank (ADBResult { result_ikey = key, result_ipos = pos, result_dist = dist }) =
      Ranking { rk_key = key
              , rk_distance = dist
              , rk_start = (framesToSecs pos)
              , rk_length = 0.0
              , rk_nsequence = Nothing
              , rk_distThresh = Nothing
              , rk_startThresh = Nothing
              , rk_lengthThresh = Nothing }

evaluate :: [Ranking] -> Query -> (Accuracy, Precision, Recall)
evaluate rs q@(Query { q_evaluation = MatchDistances }) = (accuracy, precision, recall)
  where
    requiredResults = (q_requiredResults q)
    positiveResults = zipWith (\a b -> if a == b then Just a else Nothing) rs requiredResults
    tPR = trace ("positiveResults: " ++ (show positiveResults)) positiveResults
    negativeResults = zipWith (\a b -> if a == b then Just a else Nothing) requiredResults rs
    trueNegatives = [] -- What? the number of possible sequences in the database that are not requiredResults?
    falseNegatives = filter (== Nothing) negativeResults
    truePositives = filter (/= Nothing) positiveResults
    falsePositives = filter (== Nothing) positiveResults
    fracLen = realToFrac . length
    accuracy = ((fracLen trueNegatives) + (fracLen truePositives)) / (fracLen requiredResults)
    precision = (fracLen truePositives) / (fracLen truePositives) + (fracLen falsePositives)
    recall = (fracLen truePositives) / (fracLen truePositives) + (fracLen falseNegatives)

evaluate rs q@(Query { q_evaluation = MatchOrder }) = undefined

saveRun :: TestRun -> IO ()
saveRun = putStrLn . show

runTest :: Test -> IO TestRun
runTest test = do
  libaudioDB <- audiodb_lib_build_id >>= peekCString
  os         <- return Sys.os
  arch       <- return Sys.arch
  method     <- return Serial

  startTime  <- getCurrentTime
  results    <- mapM runQuery (t_queries test)
  endTime    <- getCurrentTime

  return TestRun {
      tr_test        = test
    , tr_results     = results
    , tr_startTime   = startTime
    , tr_endTime     = endTime
    , tr_libaudioDBv = libaudioDB
    , tr_systemName  = os
    , tr_systemArch  = arch
    , tr_execMethod  = method }

dryRunTest :: Test -> IO ()
dryRunTest = putStrLn . show

runEitherTest :: Bool -> Either ParseException Test -> IO ()
runEitherTest False (Right t) = runTest t >>= saveRun
runEitherTest True  (Right t) = dryRunTest t
runEitherTest _     (Left ex) = error $ "Could not parse configuration file: " ++ (show ex)

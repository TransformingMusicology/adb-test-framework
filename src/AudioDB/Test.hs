-- AudioDBTest - A test framework for the audioDB system
--
-- Copyright (C) 2015, 2016 Richard Lewis, Goldsmiths' College
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

{-# LANGUAGE OverloadedStrings #-}

module AudioDB.Test where

import           AudioDB.Test.Types
import           Control.Logging (log', timedLog', warn', errorL')
import qualified Data.ByteString.Char8 as BS (putStrLn)
import           Data.DateTime
import           Data.List (sortBy, (\\), intersect, deleteFirstsBy)
import           Data.Maybe (isJust)
import qualified Data.Text as T (pack, append)
import           Data.Yaml (encode, ParseException)
import           Foreign.C.String
import           Sound.Audio.Database
import           Sound.Audio.Database.Query
import           Sound.Audio.Database.Types
import           System.Info as Sys

configurePointQuery :: QueryConf -> ADBDatum -> FeatureRate -> FrameSize -> (QueryAllocator, Maybe QueryTransformer, Maybe (QueryCallback ()), Maybe QueryComplete)
configurePointQuery conf@( QueryConf { qc_rotations = [] }) qDatum secToFrames frameToSecs =
  (mkPointQuery
     qDatum
     secToFrames
     frameToSecs
     (qc_npoints conf),
   Nothing,
   Nothing,
   Nothing)

configurePointQuery conf@( QueryConf { qc_rotations = (_:_) }) qDatum secToFrames frameToSecs = undefined

configureTrackQuery :: QueryConf -> ADBDatum -> (QueryAllocator, Maybe QueryTransformer, Maybe (QueryCallback ()), Maybe QueryComplete)
configureTrackQuery conf@( QueryConf { qc_rotations = [] }) qDatum =
  (mkTrackQuery
     qDatum
     (qc_ntracks conf),
   Nothing,
   Nothing,
   Nothing)

configureTrackQuery conf@( QueryConf { qc_rotations = (_:_) }) qDatum = undefined

configureSequenceQuery :: QueryConf -> ADBDatum -> FeatureRate -> (QueryAllocator, Maybe QueryTransformer, Maybe (QueryCallback ()), Maybe QueryComplete)
configureSequenceQuery conf@( QueryConf { qc_rotations = [] }) qDatum secToFrames =
  (mkSequenceQuery
     qDatum
     secToFrames
     (qc_npoints conf)
     (qc_ntracks conf)
     (qc_start conf)
     (qc_length conf)
     (mkDistance (qc_distance conf))
     (mkAbsPower (qc_absoluteThreshold conf))
     (qc_queryHopSize conf)
     (qc_dbHopSize conf),
   Nothing,
   Nothing,
   Nothing)

configureSequenceQuery conf@( QueryConf { qc_rotations = (_:_) }) qDatum secToFrames = (a, Just t, Just reportRotation, Just c)
  where
    (a, t, c) = mkSequenceQueryWithRotation
                  qDatum
                  secToFrames
                  (qc_npoints conf)
                  (qc_ntracks conf)
                  (qc_start conf)
                  (qc_length conf)
                  (mkDistance (qc_distance conf))
                  (mkAbsPower (qc_absoluteThreshold conf))
                  (qc_queryHopSize conf)
                  (qc_dbHopSize conf)
                  (qc_rotations conf)

configureSequencePerTrackQuery :: QueryConf -> ADBDatum -> FeatureRate -> (QueryAllocator, Maybe QueryTransformer, Maybe (QueryCallback ()), Maybe QueryComplete)
configureSequencePerTrackQuery conf@( QueryConf { qc_rotations = [] }) qDatum secToFrames =
  (mkSequencePerTrackQuery
     qDatum
     secToFrames
     (qc_ntracks conf)
     (qc_start conf)
     (qc_length conf)
     (mkDistance (qc_distance conf))
     (mkAbsPower (qc_absoluteThreshold conf)),
   Nothing,
   Nothing,
   Nothing)

configureSequencePerTrackQuery conf@( QueryConf { qc_rotations = (_:_) }) qDatum secToFrames = (a, Just t, Just reportRotation, Just c)
  where
    (a, t, c) = mkSequencePerTrackQueryWithRotation
                  qDatum
                  secToFrames
                  (qc_ntracks conf)
                  (qc_start conf)
                  (qc_length conf)
                  (mkDistance (qc_distance conf))
                  (mkAbsPower (qc_absoluteThreshold conf))
                  (qc_rotations conf)

configureNSequenceQuery :: QueryConf -> ADBDatum -> FeatureRate -> (QueryAllocator, Maybe QueryTransformer, Maybe (QueryCallback ()), Maybe QueryComplete)
configureNSequenceQuery conf@( QueryConf { qc_rotations = [] }) qDatum secToFrames =
  (mkNSequenceQuery
     qDatum
     secToFrames
     (qc_npoints conf)
     (qc_ntracks conf)
     (qc_length conf)
     (mkDistance (qc_distance conf))
     (mkAbsPower (qc_absoluteThreshold conf))
     (qc_queryHopSize conf)
     (qc_dbHopSize conf),
   Nothing,
   Nothing,
   Nothing)

configureNSequenceQuery conf@( QueryConf { qc_rotations = (_:_) }) qDatum secToFrames = (a, Just t, Just reportRotation, Just c)
  where
    (a, t, c) = mkNSequenceQueryWithRotation
                  qDatum
                  secToFrames
                  (qc_npoints conf)
                  (qc_ntracks conf)
                  (qc_length conf)
                  (mkDistance (qc_distance conf))
                  (mkAbsPower (qc_absoluteThreshold conf))
                  (qc_queryHopSize conf)
                  (qc_dbHopSize conf)
                  (qc_rotations conf)

mkDistance :: Distance -> Maybe [DistanceFlag]
mkDistance DotProduct                = Just [dotProductFlag]
mkDistance EuclideanNormed           = Just [euclideanNormedFlag]
mkDistance Euclidean                 = Just [euclideanFlag]
mkDistance KullbackLeiblerDivergence = Just [kullbackLeiblerDivergenceFlag]

mkAbsPower :: Double -> Maybe Double
mkAbsPower 0.0 = Nothing
mkAbsPower x   = Just x

reportRotation :: QueryCallback ()
reportRotation i _ = log' $ "Rotation #" `T.append` (T.pack $ show i)

(//) :: Maybe a -> a -> a
Just x  // _ = x
Nothing // y = y

prepareQuery :: Query -> ADBDatum -> (QueryAllocator, Maybe QueryTransformer, Maybe (QueryCallback ()), Maybe QueryComplete)
prepareQuery q qDatum
  | qtype == PointQuery             = configurePointQuery conf qDatum featureRate frameSize
  | qtype == TrackQuery             = configureTrackQuery conf { qc_accumulation = AccumPerTrack } qDatum
  | qtype == SequenceQuery          = configureSequenceQuery conf { qc_accumulation = AccumPerTrack } qDatum featureRate
  | qtype == SequencePerTrackQuery  = configureSequencePerTrackQuery conf { qc_accumulation = AccumPerTrack } qDatum featureRate
  | qtype == NSequenceQuery         = configureNSequenceQuery conf { qc_accumulation = AccumPerTrack } qDatum featureRate
  | otherwise                       = errorL' $ "No such query type: " `T.append` (T.pack $ show qtype)
  where qtype = (qo_type . q_query) q
        featureRate = (db_featureRate . q_db) q
        frameSize = (db_frameSize . q_db) q
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
                         , qc_dbHopSize = (qo_sequenceHop qo)
                         , qc_rotations = (qo_rotations qo) // [] }

runQuery :: Bool -> Query -> IO QueryResult
runQuery withEval q = do
  log' $ "Started query: " `T.append` (T.pack $ q_identifier q)
  withExistingROAudioDB dbFileName withDB
  where
    dbFileName = (db_fileName . q_db) q
    key        = (qo_key . q_query) q

    queryForDatum _    Nothing     = errorL' $ "Query features not found: " `T.append` T.pack key
    queryForDatum adb (Just datum) = do
      log' $ "Retrieved features: " `T.append` T.pack key
      let (qAlloc, qTransform, qCallback, qComplete) = prepareQuery q datum
      timedLog' "Executing query" $ query adb qAlloc qTransform qCallback qComplete

    withDB Nothing    = errorL' $ "Could not open database " `T.append` T.pack dbFileName
    withDB (Just adb) = do
      log' $ "Loaded database: " `T.append` T.pack dbFileName
      let frameSize = (db_frameSize . q_db) q

      results <- withMaybeDatumIO adb key (queryForDatum adb)

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
              , rk_length = Nothing
              , rk_nsequence = Nothing
              , rk_distThresh = Nothing
              , rk_startThresh = Nothing
              , rk_lengthThresh = Nothing }

evaluate :: [Ranking] -> Query -> (Accuracy, Precision, Recall)
evaluate returnedResults q@(Query { q_evaluation = MatchDistances }) = (accuracy, precision, recall)
  where
    requiredResults = (q_requiredResults q)
    truePositives   = returnedResults `intersect` requiredResults
    falsePositives  = returnedResults \\ requiredResults
    falseNegatives  = requiredResults \\ returnedResults
    fracLen         = realToFrac . length
    accuracy        = 0
    precision       = (fracLen truePositives) / ((fracLen truePositives) + (fracLen falsePositives))
    recall          = (fracLen truePositives) / ((fracLen falseNegatives) + (fracLen truePositives))

evaluate returnedResults q@(Query { q_evaluation = MatchOrder }) = (accuracy, precision, recall)
  where
    requiredResults = (q_requiredResults q)
    truePositives   = map fst $ filter (\(rt,rq) -> rt `locEq` rq) $ zip returnedResults requiredResults
    falsePositives  = deleteFirstsBy locEq returnedResults truePositives
    falseNegatives  = deleteFirstsBy locEq requiredResults truePositives
    fracLen         = realToFrac . length
    accuracy        = 0
    precision       = (fracLen truePositives) / ((fracLen truePositives) + (fracLen falsePositives))
    recall          = (fracLen truePositives) / ((fracLen falseNegatives) + (fracLen truePositives))

showRun :: TestRun -> IO ()
showRun = putStrLn . show

dumpRun :: TestRun -> IO ()
dumpRun tr = BS.putStrLn $ encode tr

runTest :: Bool -> Test -> IO TestRun
runTest withEval test = do
  log' $ "Starting test: " `T.append` (T.pack $ t_identifier test)

  libaudioDB <- audiodb_lib_build_id >>= peekCString
  os         <- return Sys.os
  arch       <- return Sys.arch
  method     <- return Serial

  startTime  <- getCurrentTime
  results    <- mapM (runQuery withEval) (t_queries test)
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

runEitherTest :: Bool -> Bool -> Either ParseException Test -> IO ()
runEitherTest False withEval@False (Right t) = runTest withEval t >>= dumpRun
runEitherTest False withEval@True  (Right t) = runTest withEval t >>= showRun
runEitherTest True  _              (Right t) = dryRunTest t
runEitherTest _     _              (Left ex) = errorL' $ "Could not parse configuration file: " `T.append` (T.pack $ show ex)

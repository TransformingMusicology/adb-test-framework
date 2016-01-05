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

module AudioDB.Test where

import AudioDB.Test.Types
import Data.DateTime
import Data.Maybe (isJust)
import Data.Yaml (ParseException)
import Foreign.C.String
import Sound.Audio.Database
import Sound.Audio.Database.Query
import Sound.Audio.Database.Types
import System.Info as Sys

configurePointQuery :: QueryConf -> ADBDatum -> FeatureRate -> FrameSize -> (QueryAllocator, Maybe QueryTransformer, Maybe QueryComplete)
configurePointQuery conf@( QueryConf { qc_rotations = [] }) qDatum secToFrames frameToSecs =
  (mkPointQuery qDatum secToFrames frameToSecs (qc_npoints conf), Nothing, Nothing)

configurePointQuery conf@( QueryConf { qc_rotations = (_:_) }) qDatum secToFrames frameToSecs = undefined

configureTrackQuery :: QueryConf -> ADBDatum -> (QueryAllocator, Maybe QueryTransformer, Maybe QueryComplete)
configureTrackQuery conf@( QueryConf { qc_rotations = [] }) qDatum =
  (mkTrackQuery qDatum (qc_ntracks conf), Nothing, Nothing)

configureTrackQuery conf@( QueryConf { qc_rotations = (_:_) }) qDatum = undefined

configureSequenceQuery :: QueryConf -> ADBDatum -> FeatureRate -> FrameSize -> (QueryAllocator, Maybe QueryTransformer, Maybe QueryComplete)
configureSequenceQuery conf@( QueryConf { qc_rotations = [] }) qDatum secToFrames frameToSecs =
  (mkSequenceQuery qDatum secToFrames (qc_ntracks conf) (qc_start conf) (qc_length conf) (mkDistance (qc_distance conf)) (mkAbsPower (qc_absoluteThreshold conf)), Nothing, Nothing)

configureSequenceQuery conf@( QueryConf { qc_rotations = (_:_) }) qDatum secToFrames frameToSecs = (a, Just t, Just c)
  where
    (a, t, c) = mkSequenceQueryWithRotation qDatum secToFrames frameToSecs (qc_ntracks conf) (qc_start conf) (qc_length conf) (mkDistance (qc_distance conf)) (mkAbsPower (qc_absoluteThreshold conf)) (qc_rotations conf)

configureNSequenceQuery :: QueryConf -> ADBDatum -> FeatureRate -> FrameSize -> (QueryAllocator, Maybe QueryTransformer, Maybe QueryComplete)
configureNSequenceQuery conf@( QueryConf { qc_rotations = [] }) qDatum secToFrames frameToSecs =
  (mkNSequenceQuery qDatum secToFrames (qc_npoints conf) (qc_ntracks conf) (qc_length conf) (mkDistance (qc_distance conf)) (mkAbsPower (qc_absoluteThreshold conf)) (qc_queryHopSize conf) (qc_dbHopSize conf), Nothing, Nothing)

configureNSequenceQuery conf@( QueryConf { qc_rotations = (_:_) }) qDatum secToFrames frameToSecs = undefined

mkDistance :: Distance -> Maybe [DistanceFlag]
mkDistance DotProduct                = Just [dotProductFlag]
mkDistance EuclideanNormed           = Just [euclideanNormedFlag]
mkDistance Euclidean                 = Just [euclideanFlag]
mkDistance KullbackLeiblerDivergence = Just [kullbackLeiblerDivergenceFlag]

mkAbsPower :: Double -> Maybe Double
mkAbsPower 0.0 = Nothing
mkAbsPower x   = Just x

(//) :: Maybe a -> a -> a
Just x  // _ = x
Nothing // y = y

prepareQuery :: Query -> ADBDatum -> (QueryAllocator, Maybe QueryTransformer, Maybe QueryComplete)
prepareQuery q qDatum
  | qtype == PointQuery     = configurePointQuery conf qDatum featureRate frameSize
  | qtype == TrackQuery     = configureTrackQuery conf { qc_accumulation = AccumPerTrack } qDatum
  | qtype == SequenceQuery  = configureSequenceQuery conf { qc_accumulation = AccumPerTrack } qDatum featureRate frameSize
  | qtype == NSequenceQuery = configureNSequenceQuery conf { qc_accumulation = AccumPerTrack } qDatum featureRate frameSize
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

runQuery :: Query -> IO QueryResult
runQuery q = withExistingROAudioDB dbFileName withDB
  where
    dbFileName = (db_fileName . q_db) q
    key        = (qo_key . q_query) q

    queryForDatum _    Nothing     = error "Query features not found."
    queryForDatum adb (Just datum) = do
      let (qAlloc, qTransform, qComplete) = prepareQuery q datum
      query adb qAlloc qTransform Nothing qComplete

    withDB Nothing    = error $ "Could not open database " ++ dbFileName
    withDB (Just adb) = do
      let frameSize = (db_frameSize . q_db) q

      results <- withMaybeDatumIO adb key (queryForDatum adb)

      let rankings = extractRankings frameSize results

      return QueryResult { qr_query_id = q_identifier q
                         , qr_results  = rankings }

extractRankings :: FrameSize -> ADBQueryResults -> [Ranking]
extractRankings frameSize r = map resToRank (query_results_results r)
  where
    resToRank (ADBResult { result_ikey = key, result_ipos = pos, result_dist = dist }) =
      Ranking { rk_key = key
              , rk_distance = dist
              , rk_start = (frameSize pos)
              , rk_length = 0.0 }

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

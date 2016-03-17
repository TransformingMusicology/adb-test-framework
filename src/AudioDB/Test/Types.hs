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
{-# LANGUAGE RecordWildCards #-}

module AudioDB.Test.Types where

import Control.Applicative
import Data.Aeson
import Data.DateTime
import Data.Maybe (maybe)
import Sound.Audio.Database
import Sound.Audio.Database.Types
import Text.Printf

data QueryOpts = QueryOpts {
    qo_type              :: QueryType
  , qo_key               :: Key
  , qo_start             :: Seconds
  , qo_length            :: Seconds
  , qo_pointNN           :: Int
  , qo_radius            :: Double
  , qo_resultLen         :: Int
  , qo_sequenceHop       :: Seconds
  , qo_absoluteThreshold :: Maybe Double
  , qo_relativeThreshold :: Maybe Double
  , qo_unitNorming       :: Bool
  , qo_distance          :: Distance
  , qo_rotations         :: Maybe [Int] } deriving (Eq, Show)

instance FromJSON QueryOpts where
  parseJSON (Object qo) = QueryOpts
    <$> qo .:  "type"
    <*> qo .:  "key"
    <*> qo .:? "start"             .!= 0.0
    <*> qo .:? "length"            .!= 1.0
    <*> qo .:? "pointNN"           .!= 10
    <*> qo .:? "radius"            .!= 1.0
    <*> qo .:? "count"             .!= 10
    <*> qo .:? "sequenceHop"       .!= 1
    <*> qo .:? "absoluteThreshold" .!= Nothing
    <*> qo .:? "relativeThreshold" .!= Nothing
    <*> qo .:? "unitNorming"       .!= False
    <*> qo .:? "distanceMeasure"   .!= Euclidean
    <*> qo .:? "rotations"         .!= Nothing
  parseJSON _ = error "Could not parse query options."

instance ToJSON QueryOpts where
  toJSON QueryOpts{..} = object [ "key"    .= qo_key
                                , "start"  .= qo_start
                                , "length" .= qo_length
                                ]

data QueryType = PointQuery
               | TrackQuery
               | SequenceQuery
               | SequencePerTrackQuery
               | NSequenceQuery
               deriving (Eq, Show)

instance FromJSON QueryType where
  parseJSON (String "point")               = pure PointQuery
  parseJSON (String "track")               = pure TrackQuery
  parseJSON (String "sequence")            = pure SequenceQuery
  parseJSON (String "sequence-per-track")  = pure SequencePerTrackQuery
  parseJSON (String "nsequence")           = pure NSequenceQuery
  parseJSON qt = error $ "Invalid query type: " ++ (show qt)

data QueryConf = QueryConf {
    qc_key                 :: Key
  , qc_start               :: Seconds
  , qc_length              :: Seconds
  , qc_exhaustive          :: Bool
  , qc_allowFalsePositives :: Bool
  , qc_accumulation        :: Accumulation
  , qc_distance            :: Distance
  , qc_npoints             :: Int
  , qc_ntracks             :: Int
  , qc_includeKeys         :: [Key]
  , qc_excludeKeys         :: [Key]
  , qc_radius              :: Double
  , qc_absoluteThreshold   :: Double
  , qc_relativeThreshold   :: Double
  , qc_durationRatio       :: Double
  , qc_queryHopSize        :: Seconds
  , qc_dbHopSize           :: Seconds
  , qc_rotations           :: [Int] } deriving (Eq, Show)

type Key = String

data Accumulation = AccumDB
                  | AccumPerTrack
                  | AccumOneToOne
                  deriving (Eq, Show)

data Distance = DotProduct
              | EuclideanNormed
              | Euclidean
              | KullbackLeiblerDivergence
              deriving (Eq, Show)

instance FromJSON Distance where
  parseJSON (String "dot-product")      = pure DotProduct
  parseJSON (String "euclidean-normed") = pure EuclideanNormed
  parseJSON (String "euclidean")        = pure Euclidean
  parseJSON (String "kullback")         = pure KullbackLeiblerDivergence
  parseJSON d = error $ "Invalid distance measure type: " ++ (show d)

data Query = Query {
    q_identifier      :: String
  , q_db              :: Database
  , q_query           :: QueryOpts
  , q_specifiedBy     :: String
  , q_requiredResults :: [Ranking]
  , q_evaluation      :: Evaluation } deriving (Eq, Show)

instance FromJSON Query where
  parseJSON (Object q) = Query
    <$> q .: "identifier"
    <*> q .: "db"
    <*> q .: "query"
    <*> q .: "specifiedBy"
    <*> q .: "requiredResults"
    <*> q .: "evaluation"
  parseJSON _ = error "Could not parse query."

instance ToJSON Query where
  toJSON Query{..} = object [ "identifier"      .= q_identifier
                            , "db"              .= q_db
                            , "query"           .= q_query
                            , "specifiedBy"     .= q_specifiedBy
                            , "requiredResults" .= q_requiredResults
                            ]

-- FIXME Consider adding things like FMeasure as evaluation types
data Evaluation = MatchDistances
                | MatchOrder
                deriving (Eq, Show)

instance FromJSON Evaluation where
  parseJSON (String "distance") = pure MatchDistances
  parseJSON (String "order")    = pure MatchOrder
  parseJSON e = error $ "Invalid evaluation type value: " ++ (show e)

data Database = Database {
    db_fileName    :: FilePath
  , db_feature     :: String
  , db_frameSize   :: FrameSize
  , db_featureRate :: FeatureRate
  , db_power       :: Bool }

instance Eq Database where
  (==) (Database { db_fileName = aFn }) (Database { db_fileName = bFn }) = aFn == bFn

instance Show Database where
  show (Database { db_fileName = fn }) = fn

instance FromJSON Database where
  parseJSON (Object db) = do
    fn      <- db .: "filename"
    feature <- db .: "feature"
    sr      <- db .: "sampleRate"
    ss      <- db .: "stepSize"
    power   <- db .: "power"

    let fps         = sampleRate / stepSize
        sampleRate  = fromIntegral (sr :: Int)
        stepSize    = fromIntegral (ss :: Int)
        frameSize   = (\f -> (realToFrac f) / fps)
        featureRate = (\s -> ceiling $ s * fps)

    return $ Database { db_fileName    = fn
                      , db_feature     = feature
                      , db_frameSize   = frameSize
                      , db_featureRate = featureRate
                      , db_power       = power }

  parseJSON _ = error "Could not parse database specification."

instance ToJSON Database where
  toJSON Database{..} = object [ "filename" .= db_fileName
                               , "feature"  .= db_feature
                               ]

data Ranking = Ranking {
    rk_key          :: Key
  , rk_distance     :: Double
  , rk_distThresh   :: Maybe Double
  , rk_start        :: Seconds
  , rk_startThresh  :: Maybe Seconds
  , rk_length       :: Maybe Seconds
  , rk_lengthThresh :: Maybe Seconds} deriving (Show)

inMaybeRange :: (Num a, Ord a) => (a, Maybe a) -> (a, Maybe a) -> Bool
inMaybeRange (a, Nothing)     (b, Nothing)     = a == b
inMaybeRange (a, Just aRange) (b, Nothing)     = b >= (a - aRange) && b <= (a + aRange)
inMaybeRange (a, Nothing)     (b, Just _)      = a == b
inMaybeRange (a, Just aRange) (b, Just bRange) = (a >= (b - bRange) && a <= (b + bRange))
                                                 || (b >= (a - aRange) && b <= (a + aRange))

maybeInMaybeRange :: (Num a, Ord a) => (Maybe a, Maybe a) -> (Maybe a, Maybe a) -> Bool
maybeInMaybeRange (Just a, aRange)  (Just b, bRange)  = inMaybeRange (a, aRange) (b, bRange)
maybeInMaybeRange (_, aRange) (_, bRange)             = True

instance Eq Ranking where
  (==) (Ranking { rk_key = aKey
                , rk_distance = aDist
                , rk_distThresh = aDistThresh
                , rk_start = aStart
                , rk_startThresh = aStartThresh
                , rk_length = aLen
                , rk_lengthThresh = aLenThresh})
    (Ranking { rk_key = bKey
             , rk_distance = bDist
             , rk_distThresh = bDistThresh
             , rk_start = bStart
             , rk_startThresh = bStartThresh
             , rk_length = bLen
             , rk_lengthThresh = bLenThresh})
    | aKey /= bKey = False
    | (aStart, aStartThresh) `inMaybeRange` (bStart, bStartThresh) &&
      (aLen, aLenThresh) `maybeInMaybeRange` (bLen, bLenThresh) &&
      (aDist, aDistThresh) `inMaybeRange` (bDist, bDistThresh) = True
    | otherwise = False

seqEq :: Ranking -> Ranking -> Bool
seqEq (Ranking { rk_key = aKey
                , rk_start = aStart
                , rk_startThresh = aStartThresh
                , rk_length = aLen
                , rk_lengthThresh = aLenThresh})
    (Ranking { rk_key = bKey
             , rk_start = bStart
             , rk_startThresh = bStartThresh
             , rk_length = bLen
             , rk_lengthThresh = bLenThresh})
    | aKey /= bKey = False
    | (aStart, aStartThresh) `inMaybeRange` (bStart, bStartThresh) &&
      (aLen, aLenThresh) `maybeInMaybeRange` (bLen, bLenThresh) = True
    | otherwise = False

instance Ord Ranking where
  compare a b = undefined

instance FromJSON Ranking where
  parseJSON (Object r) = Ranking
    <$> r .:  "key"
    <*> r .:  "distance"
    <*> r .:? "distanceThreshold" .!= Nothing
    <*> r .:  "start"
    <*> r .:? "startThreshold"    .!= Nothing
    <*> r .:? "length"            .!= Nothing
    <*> r .:? "lengthThreshold"   .!= Nothing
  parseJSON _ = error "Could not parse ranking."

showDbl :: (PrintfArg a, Fractional a) => a -> String
showDbl = printf "%.4f"

instance ToJSON Ranking where
  toJSON Ranking{..} = object [ "key"      .= rk_key
                              , "distance" .= showDbl rk_distance
                              , "start"    .= showDbl rk_start
                              , "length"   .= maybe "" showDbl rk_length ]

data Test = Test {
    t_identifier :: String
  , t_queries    :: [Query] } deriving (Eq, Show)

instance FromJSON Test where
  parseJSON (Object t) = Test
    <$> t .: "identifier"
    <*> t .: "queries"
  parseJSON _ = error "Could not parse test specification."

data ExecutionMethod = Serial
                     | Parallel
                     deriving (Eq, Show)

type Accuracy = Double
type Precision = Double
type Recall = Double

accuracy :: (Accuracy, Precision, Recall) -> Accuracy
accuracy (a,_,_) = a

precision :: (Accuracy, Precision, Recall) -> Precision
precision (_,p,_) = p

recall :: (Accuracy, Precision, Recall) -> Recall
recall (_,_,r) = r

-- FIXME How can we make this more polymorphic to accommodate
-- different kinds of Evaluation?
data QueryResult = QueryResult {
    qr_query    :: Query
  , qr_results  :: [Ranking]
  , qr_fMeasure :: (Accuracy, Precision, Recall) } deriving (Eq, Show)

instance ToJSON QueryResult where
  toJSON QueryResult{..} = object [ "query"     .= qr_query
                                  , "results"   .= qr_results
                                  , "accuracy"  .= showDbl (accuracy qr_fMeasure)
                                  , "precision" .= showDbl (precision qr_fMeasure)
                                  , "recall"    .= showDbl (recall qr_fMeasure) ]

data TestRun = TestRun {
    tr_test        :: Test
  , tr_results     :: [QueryResult]
  , tr_startTime   :: DateTime
  , tr_endTime     :: DateTime
  , tr_libaudioDBv :: String
  , tr_systemName  :: String
  , tr_systemArch  :: String
  , tr_execMethod  :: ExecutionMethod } deriving (Eq, Show)

instance ToJSON TestRun where
  toJSON TestRun{..} = object [ "test"        .= (t_identifier tr_test)
                              , "queries"     .= tr_results
                              , "start-time"  .= tr_startTime
                              , "end-time"    .= tr_endTime
                              , "lib-version" .= tr_libaudioDBv
                              , "system"      .= tr_systemName
                              , "arch"        .= tr_systemArch ]

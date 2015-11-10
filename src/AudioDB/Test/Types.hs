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

{-# LANGUAGE OverloadedStrings #-}

module AudioDB.Test.Types where

import AudioDB
import Data.DateTime
import System.Info
import Control.Applicative
import Data.Aeson

data QueryOpts = QueryOpts {
    qo_type              :: QueryType
  , qo_key               :: Key
  , qo_start             :: Seconds
  , qo_length            :: Seconds
  , qo_pointNN           :: Int
  , qo_radius            :: Double
  , qo_resultLen         :: Int
  , qo_sequenceHop       :: Int
  , qo_absoluteThreshold :: Maybe Double
  , qo_relativeThreshold :: Maybe Double
  , qo_unitNorming       :: Bool
  , qo_distance          :: Distance } deriving (Eq, Show)

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
  parseJSON _ = error "Could not parse query options."

data QueryType = PointQuery
               | TrackQuery
               | SequenceQuery
               | NSequenceQuery
               deriving (Eq, Show)

instance FromJSON QueryType where
  parseJSON (String "point")     = pure PointQuery
  parseJSON (String "track")     = pure TrackQuery
  parseJSON (String "sequence")  = pure SequenceQuery
  parseJSON (String "nsequence") = pure NSequenceQuery
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
  , qc_queryHopSize        :: Int
  , qc_datumHopSzie        :: Int } deriving (Eq, Show)

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
    dq_db              :: Database
  , dq_query           :: QueryOpts
  , dq_specifiedBy     :: String
  , dq_requiredResults :: [Ranking]
  , dq_evaluation      :: Evaluation } deriving (Eq, Show)

instance FromJSON Query where
  parseJSON (Object q) = Query
    <$> q .: "identifier"
    <*> q .: "db"
    <*> q .: "query"
    <*> q .: "specifiedBy"
    <*> q .: "requiredResults"
    <*> q .: "evaluation"
  parseJSON _ = error "Could not parse query."

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
    fps     <- db .: "framesPerSec"
    power   <- db .: "power"

    let frameSize   = (\f -> (realToFrac f) / fps)
        featureRate = (\s -> ceiling $ s * fps)

    return $ Database { db_fileName    = fn
                      , db_feature     = feature
                      , db_frameSize   = frameSize
                      , db_featureRate = featureRate
                      , db_power       = power }

  parseJSON _ = error "Could not parse database specification."

data Ranking = Ranking {
    rk_key      :: Key
  , rk_distance :: Double
  , rk_start    :: Seconds
  , rk_length   :: Seconds } deriving (Eq, Show)

instance FromJSON Ranking where
  parseJSON (Object r) = Ranking
    <$> r .: "key"
    <*> r .: "distance"
    <*> r .: "start"
    <*> r .: "length"
  parseJSON _ = error "Could not parse ranking."

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

data TestRun = TestRun {
    tr_test        :: Test
  , tr_startTime   :: DateTime
  , tr_endTime     :: DateTime
  , tr_libaudioDBv :: String
  , tr_systemName  :: String
  , tr_systemArch  :: String
  , tr_execMethod  :: ExecutionMethod } deriving (Eq, Show)

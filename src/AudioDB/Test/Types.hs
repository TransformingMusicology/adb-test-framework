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

module AudioDB.Test.Types where

import AudioDB
import Data.DateTime
import System.Info

data QueryOpts = QueryOpts {
    qo_type              :: QueryType
  , qo_key               :: Key
  , qo_start             :: Maybe Int
  , qo_length            :: Maybe Int
  , qo_pointNN           :: Int
  , qo_resultLen         :: Int
  , qo_sequenceHop       :: Maybe Int
  , qo_absoluteThreshold :: Double
  , qo_relativeThreshold :: Double
  , qo_unitNorming       :: Bool
  , qo_distance          :: Distance } deriving (Eq, Show)

data QueryType = PointQuery
               | TrackQuery
               | SequenceQuery
               | NSequenceQuery
               deriving (Eq, Show)

data QueryConf = QueryConf {
    qc_key                 :: Key
  , qc_start               :: Int
  , qc_length              :: Int
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

data Query = Query {
    dq_db              :: Database
  , dq_query           :: QueryOpts
  , dq_specifiedBy     :: String
  , dq_requiredResults :: Rankings } deriving (Eq, Show)

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

data Rankings = Rankings {
    rk_key      :: Key
  , rk_distance :: Double
  , rk_start    :: Int
  , rk_length   :: Int } deriving (Eq, Show)

data Evaluation = MatchDistances
                | MatchOrder
                deriving (Eq, Show)

data Test = Test {
    t_identifier :: String
  , t_queries    :: [Query]
  , t_evaluation :: Evaluation } deriving (Eq, Show)

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

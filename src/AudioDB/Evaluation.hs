-- AudioDBTest - A test framework for the audioDB system
--
-- Copyright (C) 2016 Richard Lewis, Goldsmiths' College
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

module AudioDB.Evaluation ( evaluate
                          , interpolatedAvgPrecision ) where

import           AudioDB.Test.Types
import           Data.List ((\\), intersect, intersectBy, deleteFirstsBy, nubBy)

evaluate :: [Ranking] -> Query -> (FMeasure, Precision, Recall)
evaluate returnedResults q@(Query { q_evaluation = MatchDistances }) = (fMeasure, precision, recall)
  where
    requiredResults = (q_requiredResults q)
    truePositives   = returnedResults `intersect` requiredResults
    falsePositives  = returnedResults \\ requiredResults
    falseNegatives  = requiredResults \\ returnedResults
    fracLen         = realToFrac . length
    fMeasure        = 2 * ((precision * recall) / (precision + recall))
    precision       = (fracLen truePositives) / ((fracLen truePositives) + (fracLen falsePositives))
    recall          = (fracLen truePositives) / ((fracLen falseNegatives) + (fracLen truePositives))

evaluate returnedResults q@(Query { q_evaluation = MatchOrder }) = (fMeasure, precision, recall)
  where
    requiredResults = (q_requiredResults q)
    truePositives   = map fst $ filter (\(rt,rq) -> rt `seqEq` rq) $ zip returnedResults requiredResults --FIXME Maybe this is intersectBy?
    falsePositives  = deleteFirstsBy seqEq returnedResults truePositives
    falseNegatives  = deleteFirstsBy seqEq requiredResults truePositives
    fracLen         = realToFrac . length
    fMeasure        = 2 * ((precision * recall) / (precision + recall))
    precision       = (fracLen truePositives) / ((fracLen truePositives) + (fracLen falsePositives))
    recall          = (fracLen truePositives) / ((fracLen falseNegatives) + (fracLen truePositives))

evaluate returnedResults q@(Query { q_evaluation = MatchLocations }) = (fMeasure, precision, recall)
  where
    requiredResults = nubBy startEq (q_requiredResults q)
    retResults      = nubBy startEq returnedResults
    truePositives   = intersectBy startEq retResults requiredResults
    falsePositives  = deleteFirstsBy startEq retResults truePositives
    falseNegatives  = deleteFirstsBy startEq requiredResults truePositives

    fracLen         = realToFrac . length
    fMeasure        = 2 * ((precision * recall) / (precision + recall))
    precision       = (fracLen truePositives) / ((fracLen truePositives) + (fracLen falsePositives))
    recall          = (fracLen truePositives) / ((fracLen falseNegatives) + (fracLen truePositives))

interpolatedAvgPrecision :: [Ranking] -> Query -> [Recall] -> [(FMeasure, Precision, Recall)]
interpolatedAvgPrecision returnedResults q levels =
  applyGivenRecalls $ map (flip evaluate $ q) resultsAtLevels
  where
    applyGivenRecalls fs = map (\(r, (f, p, _)) -> (f, p, r)) $ zip levels fs
    resultsAtLevels = map (flip take $ returnedResults) levelSizes
    levelSizes      = map (\lvl -> ceiling $ lvl * (realToFrac $ length returnedResults)) levels

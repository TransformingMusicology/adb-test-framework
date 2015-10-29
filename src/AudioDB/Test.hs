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

runTest :: Test -> IO ()
runTest = undefined

dryRunTest :: Test -> IO ()
dryRunTest = putStrLn . show

runEitherTest :: Bool -> Either ParseException Test -> IO ()
runEitherTest False (Right t) = runTest t
runEitherTest True  (Right t) = dryRunTest t
runEitherTest _     (Left ex) = error $ "Could not parse configuration file: " ++ (show ex)

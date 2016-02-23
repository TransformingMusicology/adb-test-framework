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

module Main where

import AudioDB.Test
import AudioDB.Test.Types
import Control.Logging (LogLevel(..), setLogLevel, withStderrLogging)
import Data.Yaml (decodeFileEither)
import Options.Applicative

data TestConfig = TestConfig {
    configFile  :: String
  , dryRun      :: Bool
  , dumpResults :: Bool
  , verbose     :: Bool }

config :: Parser TestConfig
config = TestConfig
         <$> strOption
             (    short 'c'
               <> long "config"
               <> help "Configuration file name." )
         <*> switch
             (    short 'n'
               <> long "dry-run"
               <> help "Don't execute the test; just check the configuration file." )
         <*> switch
             (    short 'd'
               <> long "dump-results"
               <> help "Execute the queries in the configuration but not the evaluation. Instead, dump YAML-formatted results suitable for future tests.")
         <*> switch
             (    short 'v'
               <> long "verbose"
               <> help "Print logging messages.")

opts :: ParserInfo TestConfig
opts = info ( helper <*> config )
       ( fullDesc
         <> progDesc "A test framework for audioDB"
         <> header "adbtest - a test framework for audioDB" )

execTest :: TestConfig -> IO ()
execTest (TestConfig f dry dump verbose) = do
  setLogLevel $ if verbose then LevelInfo else LevelError
  withStderrLogging $ Data.Yaml.decodeFileEither f >>= runEitherTest dry (not dump)

main :: IO ()
main = execParser opts >>= execTest

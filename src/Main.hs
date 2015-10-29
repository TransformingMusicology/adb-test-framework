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

module Main where

import AudioDB.Test
import AudioDB.Test.Types
import Options.Applicative
import Data.Yaml (decodeFileEither)

data TestConfig = TestConfig {
    configFile :: String
  , dryRun :: Bool }

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

opts :: ParserInfo TestConfig
opts = info ( helper <*> config )
       ( fullDesc
         <> progDesc "A test framework for audioDB"
         <> header "adb-test - a test framework for audioDB" )

execTest :: TestConfig -> IO ()
execTest (TestConfig f dry) =
  Data.Yaml.decodeFileEither f >>= runEitherTest dry

main :: IO ()
main = execParser opts >>= execTest

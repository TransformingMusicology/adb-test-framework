module TestSuite where

import Distribution.TestSuite

import AudioDB.Test
import AudioDB.Test.Types hiding (Test)
import Control.Logging (withStderrLogging)

testInMaybeRange :: TestInstance
testInMaybeRange = TestInstance {
    run = return $ inRange
  , name = "Threshold Double comparisons"
  , tags = []
  , options = []
  , setOption = \_ _ -> Right testInMaybeRange
  }
  where
    a = (1.0 :: Double, Just (0.1 :: Double))
    b = (1.1 :: Double, Nothing :: Maybe Double)
    c = (1.0 :: Double, Nothing :: Maybe Double)
    d = (1.1 :: Double, Nothing :: Maybe Double)
    e = (1.0 :: Double, Just (0.1 :: Double))
    f = (1.1 :: Double, Just (0.1 :: Double))

    comparisons = [     (a `inMaybeRange` b)
                  , not (b `inMaybeRange` a)
                  , not (c `inMaybeRange` d)
                  , not (d `inMaybeRange` c)
                  ,     (e `inMaybeRange` f)
                  ,     (f `inMaybeRange` e)
                  ]

    inRange = Finished $ if all (== True) comparisons
                         then Pass
                         else Fail $ "Threshold Double comparisons not correct: " ++ (show comparisons)

testRankingCmp :: TestInstance
testRankingCmp = TestInstance {
    run = return $ rankingsInRange
  , name = "Ranking comparisons"
  , tags = []
  , options = []
  , setOption = \_ _ -> Right testRankingCmp
  }
  where
    r1 = Ranking {
        rk_key = "Track 1"
      , rk_distance = 1.0
      , rk_distThresh = Just 0.1
      , rk_start = 60.0
      , rk_startThresh = Just 0.5
      , rk_length = Just 10.0
      , rk_lengthThresh = Just 0.5 }
    r2 = Ranking {
        rk_key = "Track 1"
      , rk_distance = 0.9
      , rk_distThresh = Nothing
      , rk_start = 59.6
      , rk_startThresh = Nothing
      , rk_length = Just 10.4
      , rk_lengthThresh = Nothing }

    rankingsInRange = Finished $ if (r1 == r2) && (r2 /= r1)
                                 then Pass
                                 else Fail $ "Compare rankings not correct: r1 == r2: " ++ (show (r1 == r2)) ++ "; r2 /= r1: " ++ (show (r2 /= r1))

tests :: IO [Test]
tests = withStderrLogging $
          return [ Test testInMaybeRange
                 , Test testRankingCmp
                 ]

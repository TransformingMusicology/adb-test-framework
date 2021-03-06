module TestSuite where

import Distribution.TestSuite

import AudioDB.Test
import AudioDB.Test.Types hiding (Test)
import AudioDB.Evaluation
import Control.Logging (withStderrLogging)
import Data.List (permutations)

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

dummyQueryOpts :: QueryOpts
dummyQueryOpts = QueryOpts {
    qo_type              = SequenceQuery
  , qo_key               = "Track1"
  , qo_start             = 0.0
  , qo_length            = 1.0
  , qo_pointNN           = 0
  , qo_radius            = 0.0
  , qo_resultLen         = 0
  , qo_maxDistance       = Nothing
  , qo_sequenceHop       = 0.0
  , qo_absoluteThreshold = Nothing
  , qo_relativeThreshold = Nothing
  , qo_unitNorming       = True
  , qo_distance          = EuclideanNormed
  , qo_rotations         = Nothing }

dummyDB :: Database
dummyDB = Database {
    db_fileName    = ""
  , db_feature     = ""
  , db_frameSize   = fromIntegral
  , db_featureRate = ceiling
  , db_power       = True }

track1_start0_dist0 :: Ranking
track1_start0_dist0 = Ranking {
    rk_key          = "Track1"
  , rk_distance     = 0.0
  , rk_distThresh   = Nothing
  , rk_start        = 0.0
  , rk_startThresh  = Nothing
  , rk_length       = Just 1.0
  , rk_lengthThresh = Nothing }

track1_start1_dist0 :: Ranking
track1_start1_dist0 = Ranking {
    rk_key          = "Track1"
  , rk_distance     = 0.0
  , rk_distThresh   = Nothing
  , rk_start        = 1.0
  , rk_startThresh  = Nothing
  , rk_length       = Just 1.0
  , rk_lengthThresh = Nothing }

track1_start2_dist01 :: Ranking
track1_start2_dist01 = Ranking {
    rk_key          = "Track1"
  , rk_distance     = 0.1
  , rk_distThresh   = Nothing
  , rk_start        = 1.0
  , rk_startThresh  = Nothing
  , rk_length       = Just 1.0
  , rk_lengthThresh = Nothing }

track1_start3_dist05 :: Ranking
track1_start3_dist05 = Ranking {
    rk_key          = "Track1"
  , rk_distance     = 0.5
  , rk_distThresh   = Nothing
  , rk_start        = 3.0
  , rk_startThresh  = Nothing
  , rk_length       = Just 1.0
  , rk_lengthThresh = Nothing }

track1_start4_dist10 :: Ranking
track1_start4_dist10 = Ranking {
    rk_key          = "Track1"
  , rk_distance     = 1.0
  , rk_distThresh   = Nothing
  , rk_start        = 5.0
  , rk_startThresh  = Nothing
  , rk_length       = Just 1.0
  , rk_lengthThresh = Nothing }

testMatchDistancesPrecision10 :: TestInstance
testMatchDistancesPrecision10 = TestInstance {
    run = return $ checkPrecision
  , name = "MatchDistances precision 1.0"
  , tags = []
  , options = []
  , setOption = \_ _ -> Right testMatchDistancesPrecision10
  }
  where
    requiredResults = [track1_start0_dist0, track1_start1_dist0, track1_start2_dist01, track1_start3_dist05, track1_start4_dist10]
    returnedResults = requiredResults
    requiredPrecision = 1.0
    (_, computedPrecision, _) = evaluate returnedResults query
    query = Query { q_identifier      = "TestMatchDistancesPrecision10"
                  , q_db              = dummyDB
                  , q_query           = dummyQueryOpts
                  , q_specifiedBy     = "Tester"
                  , q_requiredResults = requiredResults
                  , q_evaluation      = MatchDistances }
    checkPrecision = Finished $
                       if computedPrecision == requiredPrecision
                       then Pass
                       else Fail $ "Precision 1.0 mismatch: expected: " ++ (show requiredPrecision) ++ "; computed: " ++ (show computedPrecision)

testMatchOrderPrecision10 :: TestInstance
testMatchOrderPrecision10 = TestInstance {
    run = return $ checkPrecision
  , name = "MatchOrder precision 1.0"
  , tags = []
  , options = []
  , setOption = \_ _ -> Right testMatchOrderPrecision10
  }
  where
    requiredResults = [track1_start0_dist0, track1_start1_dist0, track1_start2_dist01, track1_start3_dist05, track1_start4_dist10]
    returnedResults = requiredResults
    requiredPrecision = 1.0
    (_, computedPrecision, _) = evaluate returnedResults query
    query = Query { q_identifier      = "TestMatchOrderPrecision10"
                  , q_db              = dummyDB
                  , q_query           = dummyQueryOpts
                  , q_specifiedBy     = "Tester"
                  , q_requiredResults = requiredResults
                  , q_evaluation      = MatchOrder }
    checkPrecision = Finished $
                       if computedPrecision == requiredPrecision
                       then Pass
                       else Fail $ "Precision 1.0 mismatch: expected: " ++ (show requiredPrecision) ++ "; computed: " ++ (show computedPrecision)

testMatchLocationsPrecision10 :: TestInstance
testMatchLocationsPrecision10 = TestInstance {
    run = return $ checkPrecision
  , name = "MatchLocations precision 1.0"
  , tags = []
  , options = []
  , setOption = \_ _ -> Right testMatchLocationsPrecision10
  }
  where
    requiredResults = [track1_start0_dist0, track1_start1_dist0, track1_start2_dist01, track1_start3_dist05, track1_start4_dist10]
    returnedResults = reverse $ concat $ permutations requiredResults -- i.e., ordering and duplication in results do not affect this
    requiredPrecision = 1.0
    (_, computedPrecision, _) = evaluate returnedResults query
    query = Query { q_identifier      = "TestMatchLocationsPrecision10"
                  , q_db              = dummyDB
                  , q_query           = dummyQueryOpts
                  , q_specifiedBy     = "Tester"
                  , q_requiredResults = requiredResults
                  , q_evaluation      = MatchLocations }
    checkPrecision = Finished $
                       if computedPrecision == requiredPrecision
                       then Pass
                       else Fail $ "Precision 1.0 mismatch: expected: " ++ (show requiredPrecision) ++ "; computed: " ++ (show computedPrecision)

testMatchDistancesPrecisionAllDifferent :: TestInstance
testMatchDistancesPrecisionAllDifferent = TestInstance {
    run = return $ checkPrecision
  , name = "MatchDistances all different; precision 0.0"
  , tags = []
  , options = []
  , setOption = \_ _ -> Right testMatchDistancesPrecisionAllDifferent
  }
  where
    requiredResults = [track1_start0_dist0, track1_start1_dist0, track1_start2_dist01]
    returnedResults = [track1_start3_dist05, track1_start4_dist10]
    requiredPrecision = 0.0
    (_, computedPrecision, _) = evaluate returnedResults query
    query = Query { q_identifier      = "TestMatchDistancesPrecisionAllDifferent"
                  , q_db              = dummyDB
                  , q_query           = dummyQueryOpts
                  , q_specifiedBy     = "Tester"
                  , q_requiredResults = requiredResults
                  , q_evaluation      = MatchDistances }
    checkPrecision = Finished $
                       if computedPrecision == requiredPrecision
                       then Pass
                       else Fail $ "Precision 0.0 mismatch: expected: " ++ (show requiredPrecision) ++ "; computed: " ++ (show computedPrecision)

testMatchOrderPrecisionAllMisplaced :: TestInstance
testMatchOrderPrecisionAllMisplaced = TestInstance {
    run = return $ checkPrecision
  , name = "MatchOrder all misplaced; precision 0.0"
  , tags = []
  , options = []
  , setOption = \_ _ -> Right testMatchOrderPrecisionAllMisplaced
  }
  where
    requiredResults = [track1_start0_dist0, track1_start1_dist0, track1_start2_dist01, track1_start3_dist05, track1_start4_dist10]
    returnedResults = [track1_start1_dist0, track1_start0_dist0, track1_start3_dist05, track1_start4_dist10, track1_start2_dist01]
    requiredPrecision = 0.0
    (_, computedPrecision, _) = evaluate returnedResults query
    query = Query { q_identifier      = "TestMatchOrderPrecisionAllMisplaced"
                  , q_db              = dummyDB
                  , q_query           = dummyQueryOpts
                  , q_specifiedBy     = "Tester"
                  , q_requiredResults = requiredResults
                  , q_evaluation      = MatchOrder }
    checkPrecision = Finished $
                       if computedPrecision == requiredPrecision
                       then Pass
                       else Fail $ "Precision 0.0 mismatch: expected: " ++ (show requiredPrecision) ++ "; computed: " ++ (show computedPrecision)

testMatchDistancesPrecisionOneDifferentHead :: TestInstance
testMatchDistancesPrecisionOneDifferentHead = TestInstance {
    run = return $ checkPrecision
  , name = "MatchDistances one different at head; precision 0.75"
  , tags = []
  , options = []
  , setOption = \_ _ -> Right testMatchDistancesPrecisionOneDifferentHead
  }
  where
    requiredResults = [track1_start1_dist0, track1_start2_dist01, track1_start3_dist05, track1_start4_dist10]
    returnedResults = [track1_start0_dist0, track1_start2_dist01, track1_start3_dist05, track1_start4_dist10]
    requiredPrecision = 0.75
    (_, computedPrecision, _) = evaluate returnedResults query
    query = Query { q_identifier      = "TestMatchDistancesPrecisionOneDifferentHead"
                  , q_db              = dummyDB
                  , q_query           = dummyQueryOpts
                  , q_specifiedBy     = "Tester"
                  , q_requiredResults = requiredResults
                  , q_evaluation      = MatchDistances }
    checkPrecision = Finished $
                       if computedPrecision == requiredPrecision
                       then Pass
                       else Fail $ "Precision 0.75 mismatch: expected: " ++ (show requiredPrecision) ++ "; computed: " ++ (show computedPrecision)

testMatchDistancesPrecisionOneDifferentLast :: TestInstance
testMatchDistancesPrecisionOneDifferentLast = TestInstance {
    run = return $ checkPrecision
  , name = "MatchDistances one different at end; precision 0.75"
  , tags = []
  , options = []
  , setOption = \_ _ -> Right testMatchDistancesPrecisionOneDifferentLast
  }
  where
    requiredResults = [track1_start0_dist0, track1_start1_dist0, track1_start2_dist01, track1_start3_dist05]
    returnedResults = [track1_start0_dist0, track1_start1_dist0, track1_start2_dist01, track1_start4_dist10]
    requiredPrecision = 0.75
    (_, computedPrecision, _) = evaluate returnedResults query
    query = Query { q_identifier      = "TestMatchDistancesPrecisionOneDifferentLast"
                  , q_db              = dummyDB
                  , q_query           = dummyQueryOpts
                  , q_specifiedBy     = "Tester"
                  , q_requiredResults = requiredResults
                  , q_evaluation      = MatchDistances }
    checkPrecision = Finished $
                       if computedPrecision == requiredPrecision
                       then Pass
                       else Fail $ "Precision 0.75 mismatch: expected: " ++ (show requiredPrecision) ++ "; computed: " ++ (show computedPrecision)

testMatchOrderPrecisionTwoTransposed :: TestInstance
testMatchOrderPrecisionTwoTransposed = TestInstance {
    run = return $ checkPrecision
  , name = "MatchOrder two transposed; precision 0.6"
  , tags = []
  , options = []
  , setOption = \_ _ -> Right testMatchOrderPrecisionTwoTransposed
  }
  where
    requiredResults = [track1_start0_dist0, track1_start1_dist0, track1_start2_dist01, track1_start3_dist05, track1_start4_dist10]
    returnedResults = [track1_start1_dist0, track1_start0_dist0, track1_start2_dist01, track1_start3_dist05, track1_start4_dist10]
    requiredPrecision = 0.6
    (_, computedPrecision, _) = evaluate returnedResults query
    query = Query { q_identifier      = "TestMatchOrderPrecisionTwoTransposed"
                  , q_db              = dummyDB
                  , q_query           = dummyQueryOpts
                  , q_specifiedBy     = "Tester"
                  , q_requiredResults = requiredResults
                  , q_evaluation      = MatchOrder }
    checkPrecision = Finished $
                       if computedPrecision == requiredPrecision
                       then Pass
                       else Fail $ "Precision 0.6 mismatch: expected: " ++ (show requiredPrecision) ++ "; computed: " ++ (show computedPrecision)

tests :: IO [Test]
tests = withStderrLogging $
          return [ Test testInMaybeRange
                 , Test testRankingCmp
                 , Test testMatchDistancesPrecision10
                 , Test testMatchOrderPrecision10
                 , Test testMatchLocationsPrecision10
                 , Test testMatchDistancesPrecisionAllDifferent
                 , Test testMatchOrderPrecisionAllMisplaced
                 , Test testMatchDistancesPrecisionOneDifferentHead
                 , Test testMatchDistancesPrecisionOneDifferentLast
                 , Test testMatchOrderPrecisionTwoTransposed
                 ]

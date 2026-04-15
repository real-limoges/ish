module Ish.Analysis.GapsSpec (gapsTests) where

import Data.Map.Strict qualified as Map
import Data.Time.Calendar (fromGregorian)
import DataFrame (DataFrame)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import Ish.Analysis.Cluster (ClusterConfig (..), ClusterResult, clusterMoodData)
import Ish.Analysis.DataFrame (fillMissingDates)
import Ish.Analysis.Fuzzify (buildMoodFIS, defaultMembershipFuncDefs)
import Ish.Analysis.Gaps (GapAnalysis (..), analyzeGaps)
import Ish.Fixtures (entry1, sameDimsAt)
import Ish.Types (MoodEntry)

gapsTests :: TestTree
gapsTests =
    testGroup
        "Analysis.Gaps"
        [ testCase "analyzeGaps reports no transitions when entries are contiguous" $ do
            let ga = runAnalyze contiguous
            assertEqual "no transitions" 0 (length (gapTransitions ga))
        , testCase "analyzeGaps reports empty length distribution with no gaps" $ do
            let ga = runAnalyze contiguous
            assertEqual "empty distribution" Map.empty (gapLengthDistribution ga)
        , testCase "analyzeGaps surfaces a single transition across a gap" $ do
            let ga = runAnalyze withGap
            assertEqual "one transition" 1 (length (gapTransitions ga))
        , testCase "analyzeGaps imputes one membership per absent day" $ do
            let ga = runAnalyze withGap
            assertEqual "imputed rows" 3 (Map.size (gapImputedMemberships ga))
        , testCase "analyzeGaps length distribution keyed by run length" $ do
            let ga = runAnalyze withGap
            assertEqual
                "distribution"
                (Map.fromList [(3, 1)])
                (gapLengthDistribution ga)
        ]
  where
    contiguous =
        [ entry1
        , sameDimsAt (fromGregorian 2024 1 2)
        , sameDimsAt (fromGregorian 2024 1 3)
        ]
    withGap = [entry1, sameDimsAt (fromGregorian 2024 1 5)]

    runAnalyze :: [MoodEntry] -> GapAnalysis
    runAnalyze es =
        let df = fillMissingDates es
            cr = runClusterOn df
         in analyzeGaps df cr

    runClusterOn :: DataFrame -> ClusterResult
    runClusterOn df =
        clusterMoodData
            (buildMoodFIS defaultMembershipFuncDefs)
            ClusterConfig{clusterK = 2, clusterM = 2.0}
            df

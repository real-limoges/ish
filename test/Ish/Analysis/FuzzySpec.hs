module Ish.Analysis.FuzzySpec (fuzzyTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Ish.Analysis.DataFrame (fillMissingDates)
import Ish.Analysis.Fuzzify (buildMoodFIS, defaultMembershipFuncDefs)
import Ish.Analysis.Fuzzy (analyzeMoodEntries, clusterEntries)
import Ish.Fixtures (sampleEntries)
import Ish.Types (AnalysisResult (..))

fuzzyTests :: TestTree
fuzzyTests =
    testGroup
        "Analysis.Fuzzy"
        [ testCase "analyzeMoodEntries returns clusters and a non-empty summary" $ do
            let result = analyzeMoodEntries fis df
            assertBool "clusters populated" (not (null (analysisClusters result)))
            assertBool "summary populated" (not (null (analysisSummary result)))
        , testCase "clusterEntries matches analyzeMoodEntries clusters" $ do
            let clusters = clusterEntries fis df
                result = analyzeMoodEntries fis df
            assertEqual
                "cluster counts line up"
                (length (analysisClusters result))
                (length clusters)
        , testCase "analyzeMoodEntries produces k=3 clusters on a 3-point fixture" $ do
            let result = analyzeMoodEntries fis df
            assertEqual "" 3 (length (analysisClusters result))
        ]
  where
    fis = buildMoodFIS defaultMembershipFuncDefs
    df = fillMissingDates sampleEntries

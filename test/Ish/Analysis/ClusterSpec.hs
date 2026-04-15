module Ish.Analysis.ClusterSpec (clusterTests) where

import Data.Map.Strict qualified as Map
import Data.Time.Calendar (fromGregorian)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Ish.Analysis.Cluster (ClusterConfig (..), ClusterResult (..), clusterMoodData)
import Ish.Analysis.DataFrame (fillMissingDates)
import Ish.Analysis.Fuzzify (buildMoodFIS, defaultMembershipFuncDefs)
import Ish.Fixtures (sampleEntries)
import Ish.Types (MoodCluster (..), MoodDimension (..), MoodEntry (..))

clusterTests :: TestTree
clusterTests =
    testGroup
        "Analysis.Cluster"
        [ testCase "clusterMoodData with k=1 yields a single cluster" $ do
            let cr = runCluster 1 sampleEntries
            assertEqual "cluster count" 1 (length (resultClusters cr))
        , testCase "clusterMoodData with k=2 yields two clusters" $ do
            let cr = runCluster 2 (sampleEntries <> moreEntries)
            assertEqual "cluster count" 2 (length (resultClusters cr))
        , testCase "clusterMoodData reports at least one iteration" $ do
            let cr = runCluster 2 sampleEntries
            assertBool "iterations > 0" (resultIterations cr > 0)
        , testCase "cluster sizes sum to the number of present rows" $ do
            let entries = sampleEntries <> moreEntries
                cr = runCluster 2 entries
                reported = sum (map clusterSize (resultClusters cr))
            assertEqual "sizes sum" (length entries) reported
        ]
  where
    runCluster k es =
        clusterMoodData
            (buildMoodFIS defaultMembershipFuncDefs)
            ClusterConfig{clusterK = k, clusterM = 2.0}
            (fillMissingDates es)

moreEntries :: [MoodEntry]
moreEntries =
    [ MoodEntry
        (fromGregorian 2024 1 10)
        (Map.fromList [(Sleep, 3), (Anxiety, 4), (Sensitivity, 4), (Outlook, 2), (Speed, 3)])
    , MoodEntry
        (fromGregorian 2024 1 11)
        (Map.fromList [(Sleep, 4), (Anxiety, 4), (Sensitivity, 5), (Outlook, 3), (Speed, 2)])
    , MoodEntry
        (fromGregorian 2024 1 12)
        (Map.fromList [(Sleep, 3), (Anxiety, 5), (Sensitivity, 5), (Outlook, 2), (Speed, 3)])
    ]

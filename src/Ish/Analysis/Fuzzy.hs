module Ish.Analysis.Fuzzy (
    analyzeMoodEntries,
    clusterEntries,
) where

import Data.Map.Strict qualified as Map
import DataFrame (DataFrame)
import Hazy (FIS)

import Ish.Analysis.Cluster (ClusterConfig (..), ClusterResult (..), clusterMoodData)
import Ish.Analysis.Fuzzify (fuzzifyEntriesWith)
import Ish.Types (AnalysisResult (..), FuzzyLabel (..), MoodCluster (..))

analyzeMoodEntries :: FIS -> DataFrame -> AnalysisResult
analyzeMoodEntries fis df =
    let fuzzified = fuzzifyEntriesWith fis df
        cr = clusterMoodData fis defaultClusterConfig fuzzified
     in AnalysisResult
            { analysisClusters = resultClusters cr
            , analysisSummary = summarize cr
            }

clusterEntries :: FIS -> DataFrame -> [MoodCluster]
clusterEntries fis df =
    let fuzzified = fuzzifyEntriesWith fis df
        cr = clusterMoodData fis defaultClusterConfig fuzzified
     in resultClusters cr

defaultClusterConfig :: ClusterConfig
defaultClusterConfig = ClusterConfig{clusterK = 3, clusterM = 2.0}

summarize :: ClusterResult -> [FuzzyLabel]
summarize cr =
    let total = sum (map clusterSize (resultClusters cr))
        weighted = concatMap (weightLabels total) (resultClusters cr)
     in mergeLabels weighted

weightLabels :: Int -> MoodCluster -> [FuzzyLabel]
weightLabels total mc =
    let w = fromIntegral (clusterSize mc) / fromIntegral total
     in [ FuzzyLabel (labelName l) (labelMembership l * w)
        | l <- clusterLabels mc
        ]

mergeLabels :: [FuzzyLabel] -> [FuzzyLabel]
mergeLabels labels =
    let grouped =
            Map.fromListWith
                max
                [(labelName l, labelMembership l) | l <- labels]
     in [FuzzyLabel name deg | (name, deg) <- Map.toList grouped]

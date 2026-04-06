module Ish.Analysis.Fuzzy (
    analyzeMoodEntries,
    clusterEntries,
) where

import Data.Map.Strict qualified as Map
import DataFrame (DataFrame)

import Ish.Analysis.Cluster (ClusterConfig (..), ClusterResult (..), clusterMoodData)
import Ish.Analysis.Fuzzify (fuzzifyEntries)
import Ish.Types (AnalysisResult (..), FuzzyLabel (..), MoodCluster (..))

-- | Run fuzzy analysis on a date-spine mood DataFrame.
--
-- Flow: DataFrame → fuzzify → cluster → label → AnalysisResult
analyzeMoodEntries :: DataFrame -> AnalysisResult
analyzeMoodEntries df =
    let fuzzified = fuzzifyEntries df
        cr = clusterMoodData defaultClusterConfig fuzzified
     in AnalysisResult
            { analysisClusters = resultClusters cr
            , analysisSummary = summarize cr
            }

-- | Cluster mood entries using fuzzy similarity with default parameters.
clusterEntries :: DataFrame -> [MoodCluster]
clusterEntries df =
    let fuzzified = fuzzifyEntries df
        cr = clusterMoodData defaultClusterConfig fuzzified
     in resultClusters cr

defaultClusterConfig :: ClusterConfig
defaultClusterConfig = ClusterConfig{clusterK = 3, clusterM = 2.0}

-- | Derive a high-level summary from clustering results.
summarize :: ClusterResult -> [FuzzyLabel]
summarize cr =
    let total = sum (map clusterSize (resultClusters cr))
        weighted = concatMap (weightLabels total) (resultClusters cr)
     in mergeLabels weighted

-- | Scale a cluster's labels by its proportion of total data points.
weightLabels :: Int -> MoodCluster -> [FuzzyLabel]
weightLabels total mc =
    let w = fromIntegral (clusterSize mc) / fromIntegral total
     in [ FuzzyLabel (labelName l) (labelMembership l * w)
        | l <- clusterLabels mc
        ]

-- | Combine FuzzyLabels with the same name, keeping the max degree.
mergeLabels :: [FuzzyLabel] -> [FuzzyLabel]
mergeLabels labels =
    let grouped = Map.fromListWith max
            [ (labelName l, labelMembership l) | l <- labels ]
     in [ FuzzyLabel name deg | (name, deg) <- Map.toList grouped ]

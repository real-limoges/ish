module Ish.Analysis.Fuzzy (
    analyzeMoodEntries,
    clusterEntries,
) where

-- Expected hazy imports (only import site for hazy in the project):
--
-- import Hazy.FuzzySet (triangular, trapezoidal, membershipDegree)
-- import Hazy.Operations (fuzzyUnion, fuzzyIntersect, complement)
-- import Hazy.Inference (Rule, evaluate)
-- import Hazy.Defuzzify (centroid, bisector, meanOfMaxima)

import DataFrame (DataFrame)

import Ish.Types (AnalysisResult, MoodCluster)

{- | Run fuzzy analysis on a date-spine mood DataFrame.

Flow: DataFrame (nullable dimensions) → fuzzy set membership →
inference rules → defuzzify → AnalysisResult
-}
analyzeMoodEntries :: DataFrame -> AnalysisResult
analyzeMoodEntries _df = error "TODO: implement analyzeMoodEntries using hazy"

-- | Cluster mood entries using fuzzy similarity.
clusterEntries :: DataFrame -> [MoodCluster]
clusterEntries _df = error "TODO: implement clusterEntries using hazy"

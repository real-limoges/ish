module Ish.Analysis.Gaps (
    GapAnalysis (..),
    GapTransition (..),
    analyzeGaps,
) where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Time.Calendar (Day, diffDays)
import Data.Vector qualified as V
import DataFrame (DataFrame)
import Hazy (Degree)

import Ish.Analysis.Cluster (ClusterResult (..))
import Ish.Analysis.DataFrame (extractEntries, extractPresentRows, identifyGaps)
import Ish.Types (Gap (..), MoodCluster (..))

data GapTransition = GapTransition
    { transitionGap :: Gap
    , transitionBefore :: Map Text Degree
    , transitionAfter :: Map Text Degree
    , transitionClusterChanged :: Bool
    }
    deriving stock (Show)

instance ToJSON GapTransition where
    toJSON t =
        object
            [ "gap" .= transitionGap t
            , "before" .= transitionBefore t
            , "after" .= transitionAfter t
            , "clusterChanged" .= transitionClusterChanged t
            ]

data GapAnalysis = GapAnalysis
    { gapTransitions :: [GapTransition]
    , gapLengthDistribution :: Map Int Int
    , gapImputedMemberships :: Map Day (Map Text Degree)
    }
    deriving stock (Show)

instance ToJSON GapAnalysis where
    toJSON g =
        object
            [ "transitions" .= gapTransitions g
            , "lengthDistribution" .= gapLengthDistribution g
            , "imputedMemberships" .= gapImputedMemberships g
            ]

analyzeGaps :: DataFrame -> ClusterResult -> GapAnalysis
analyzeGaps df cr =
    let entries = extractEntries df
        gaps = identifyGaps entries
        (_, dateToRow) = extractPresentRows df
        rowToDate = Map.fromList [(i, d) | (i, d) <- Map.toList dateToRow]
        dateToIdx = Map.fromList [(d, i) | (i, d) <- Map.toList rowToDate]
        clusterNames = map clusterName (resultClusters cr)
        transitions = map (computeTransition cr dateToIdx clusterNames) gaps
        distribution = Map.fromListWith (+) [(gapLength g, 1) | g <- gaps]
        imputed = concatMap (imputeGap cr dateToIdx clusterNames) gaps
     in GapAnalysis
            { gapTransitions = transitions
            , gapLengthDistribution = distribution
            , gapImputedMemberships = Map.fromList imputed
            }

lookupMembership :: ClusterResult -> Map Day Int -> [Text] -> Day -> Map Text Degree
lookupMembership cr dateToIdx clusterNames day =
    case Map.lookup day dateToIdx of
        Nothing -> Map.empty
        Just i ->
            let row = resultMembership cr V.! i
             in Map.fromList (zip clusterNames (V.toList row))

dominantCluster :: Map Text Degree -> Text
dominantCluster m = fst (Map.foldlWithKey' maxBy ("", 0) m)
  where
    maxBy (bestK, bestV) k v = if v > bestV then (k, v) else (bestK, bestV)

computeTransition :: ClusterResult -> Map Day Int -> [Text] -> Gap -> GapTransition
computeTransition cr dateToIdx clusterNames g =
    let before = lookupMembership cr dateToIdx clusterNames (gapBefore g)
        after = lookupMembership cr dateToIdx clusterNames (gapAfter g)
     in GapTransition
            { transitionGap = g
            , transitionBefore = before
            , transitionAfter = after
            , transitionClusterChanged = dominantCluster before /= dominantCluster after
            }

imputeGap :: ClusterResult -> Map Day Int -> [Text] -> Gap -> [(Day, Map Text Degree)]
imputeGap cr dateToIdx clusterNames g =
    let before = lookupMembership cr dateToIdx clusterNames (gapBefore g)
        after = lookupMembership cr dateToIdx clusterNames (gapAfter g)
        totalDays = diffDays (gapAfter g) (gapBefore g)
        absentDays = [succ (gapBefore g) .. pred (gapAfter g)]
     in [ (d, interpolate before after w)
        | d <- absentDays
        , let w = fromIntegral (diffDays d (gapBefore g)) / fromIntegral totalDays
        ]

interpolate :: Map Text Degree -> Map Text Degree -> Double -> Map Text Degree
interpolate before after w =
    Map.mergeWithKey
        (\_ b a -> Just ((1 - w) * b + w * a))
        (Map.map (* (1 - w)))
        (Map.map (* w))
        before
        after

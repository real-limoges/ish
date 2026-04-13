module Ish.Analysis.Cluster (
    ClusterConfig (..),
    ClusterResult (..),
    clusterMoodData,
) where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import DataFrame (DataFrame)
import Hazy (Degree, FCMConfig (..), FCMResult (..), FIS, evaluate, fcm)

import Ish.Analysis.DataFrame (extractPresentRows)
import Ish.Types (FuzzyLabel (..), MoodCluster (..), MoodDimension (..), dimensionToText)

data ClusterConfig = ClusterConfig
    { clusterK :: Int
    , clusterM :: Double
    }
    deriving stock (Show, Eq)

instance ToJSON ClusterConfig where
    toJSON c = object ["k" .= clusterK c, "m" .= clusterM c]

instance FromJSON ClusterConfig where
    parseJSON = withObject "ClusterConfig" $ \v ->
        ClusterConfig <$> v .: "k" <*> v .: "m"

data ClusterResult = ClusterResult
    { resultClusters :: [MoodCluster]
    , resultCenters :: Vector (Vector Double)
    , resultMembership :: Vector (Vector Degree)
    , resultIterations :: Int
    }
    deriving stock (Show)

instance ToJSON ClusterResult where
    toJSON r =
        object
            [ "clusters" .= resultClusters r
            , "centers" .= resultCenters r
            , "membership" .= resultMembership r
            , "iterations" .= resultIterations r
            ]

clusterMoodData :: FIS -> ClusterConfig -> DataFrame -> ClusterResult
clusterMoodData fis cfg df =
    let (points, _dateMap) = extractPresentRows df
        fcmCfg = toFCMConfig cfg
        result = fcm fcmCfg points
        clusters = labelCenters fis (fcmCenters result) (fcmMembership result)
     in ClusterResult
            { resultClusters = clusters
            , resultCenters = fcmCenters result
            , resultMembership = fcmMembership result
            , resultIterations = fcmIterations result
            }

toFCMConfig :: ClusterConfig -> FCMConfig
toFCMConfig c =
    FCMConfig
        { fcmClusters = clusterK c
        , fcmFuzziness = clusterM c
        , fcmEpsilon = 1e-5
        , fcmMaxIter = 100
        }

dimOrder :: [MoodDimension]
dimOrder = [Sleep, Anxiety, Sensitivity, Outlook, Speed]

labelCenters :: FIS -> Vector (Vector Double) -> Vector (Vector Degree) -> [MoodCluster]
labelCenters fis centers membership =
    [mkCluster i center | (i, center) <- zip [0 ..] (V.toList centers)]
  where
    mkCluster i center =
        let centroidMap = Map.fromList (zip dimOrder (V.toList center))
            fisInput = Map.fromList (zip (map dimensionToText dimOrder) (V.toList center))
            fisOutput = evaluate fis fisInput
            labels =
                [ FuzzyLabel (k <> " " <> lvl) deg
                | (k, deg) <- Map.toList fisOutput
                , let lvl
                        | deg > 6.67 = "high"
                        | deg > 3.33 = "medium"
                        | otherwise = "low"
                ]
            size =
                length
                    [ ()
                    | row <- V.toList membership
                    , V.maxIndex row == i
                    ]
            name = T.intercalate " / " [labelName l | l <- labels]
         in MoodCluster
                { clusterName = name
                , clusterCentroid = centroidMap
                , clusterSize = size
                , clusterLabels = labels
                }

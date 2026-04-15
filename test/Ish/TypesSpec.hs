module Ish.TypesSpec (typesTests) where

import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.Map.Strict qualified as Map
import Data.Time.Calendar (fromGregorian)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)

import Ish.Types (
    AnalysisResult (..),
    FuzzyLabel (..),
    Gap (..),
    MamdaniRequest (..),
    MamdaniResponse (..),
    MembershipFuncDefs (..),
    MoodCluster (..),
    MoodDimension (..),
    RuleDef (..),
    TermDef (..),
    VarDef (..),
 )

typesTests :: TestTree
typesTests =
    testGroup
        "Types (JSON round-trip)"
        [ testCase "TermDef round-trips" $
            roundTrip (TermDef "low" (0, 0, 5))
        , testCase "VarDef round-trips" $
            roundTrip
                ( VarDef
                    "temperature"
                    (0, 40)
                    [TermDef "cold" (0, 0, 20), TermDef "hot" (20, 40, 40)]
                )
        , testCase "MembershipFuncDefs round-trips" $
            roundTrip sampleMfs
        , testCase "RuleDef round-trips" $
            roundTrip (RuleDef [("temperature", "cold")] [("fan", "low")])
        , testCase "MamdaniRequest round-trips" $
            roundTrip sampleRequest
        , testCase "MamdaniResponse round-trips" $
            roundTrip sampleResponse
        , testCase "FuzzyLabel round-trips" $
            roundTrip (FuzzyLabel "wellbeing high" 0.75)
        , testCase "Gap round-trips" $
            roundTrip
                Gap
                    { gapStart = fromGregorian 2024 1 2
                    , gapLength = 3
                    , gapBefore = fromGregorian 2024 1 1
                    , gapAfter = fromGregorian 2024 1 5
                    }
        , testCase "MoodCluster round-trips" $
            roundTrip
                MoodCluster
                    { clusterName = "sample"
                    , clusterCentroid =
                        Map.fromList
                            [ (Sleep, 7)
                            , (Anxiety, 2)
                            , (Sensitivity, 3)
                            , (Outlook, 8)
                            , (Speed, 6)
                            ]
                    , clusterSize = 4
                    , clusterLabels = [FuzzyLabel "wellbeing high" 0.8]
                    }
        , testCase "AnalysisResult round-trips" $
            roundTrip
                AnalysisResult
                    { analysisClusters = []
                    , analysisSummary = [FuzzyLabel "wellbeing high" 0.8]
                    }
        ]

roundTrip :: (Eq a, Show a, ToJSON a, FromJSON a) => a -> Assertion
roundTrip x = assertEqual "round-trip" (Just x) (decode (encode x))

sampleMfs :: MembershipFuncDefs
sampleMfs =
    MembershipFuncDefs
        { mfdInputs =
            [ VarDef
                "temperature"
                (0, 40)
                [TermDef "cold" (0, 0, 20), TermDef "hot" (20, 40, 40)]
            ]
        , mfdOutputs =
            [ VarDef
                "fan"
                (0, 100)
                [TermDef "low" (0, 0, 50), TermDef "high" (50, 100, 100)]
            ]
        }

sampleRequest :: MamdaniRequest
sampleRequest =
    MamdaniRequest
        { mrDefs = sampleMfs
        , mrRules =
            [ RuleDef [("temperature", "cold")] [("fan", "low")]
            , RuleDef [("temperature", "hot")] [("fan", "high")]
            ]
        , mrValues = Map.fromList [("temperature", 25.0)]
        }

sampleResponse :: MamdaniResponse
sampleResponse =
    MamdaniResponse
        { mrsInputDegrees =
            Map.fromList
                [("temperature", Map.fromList [("cold", 0.3), ("hot", 0.7)])]
        , mrsRuleStrengths = [0.3, 0.7]
        , mrsOutputCurves =
            Map.fromList [("fan", [(0, 0.3), (50, 0.3), (100, 0.7)])]
        , mrsCrisp = Map.fromList [("fan", 72.5)]
        }

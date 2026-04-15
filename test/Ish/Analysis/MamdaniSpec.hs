module Ish.Analysis.MamdaniSpec (mamdaniTests) where

import Data.Map.Strict qualified as Map
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Ish.Analysis.Mamdani (runMamdani)
import Ish.Types (
    MamdaniRequest (..),
    MamdaniResponse (..),
    MembershipFuncDefs (..),
    RuleDef (..),
    TermDef (..),
    VarDef (..),
 )

mamdaniTests :: TestTree
mamdaniTests =
    testGroup
        "Analysis.Mamdani"
        [ testCase "runMamdani returns a strength per rule" $ do
            let resp = runMamdani (fanRequest 10)
            assertEqual "rule strengths count" 2 (length (mrsRuleStrengths resp))
        , testCase "runMamdani reports input membership degrees" $ do
            let resp = runMamdani (fanRequest 10)
                terms = Map.lookup "temperature" (mrsInputDegrees resp)
            assertEqual "temperature terms" (Just 2) (Map.size <$> terms)
        , testCase "runMamdani produces a crisp output per output var" $ do
            let resp = runMamdani (fanRequest 10)
            assertBool "fan key present in crisp" (Map.member "fan" (mrsCrisp resp))
        , testCase "cold input biases crisp fan below midpoint" $ do
            let resp = runMamdani (fanRequest 5)
            case Map.lookup "fan" (mrsCrisp resp) of
                Nothing -> assertBool "fan key missing in crisp output" False
                Just fan -> assertBool ("fan should be < 50, got " <> show fan) (fan < 50)
        , testCase "hot input biases crisp fan above midpoint" $ do
            let resp = runMamdani (fanRequest 35)
            case Map.lookup "fan" (mrsCrisp resp) of
                Nothing -> assertBool "fan key missing in crisp output" False
                Just fan -> assertBool ("fan should be > 50, got " <> show fan) (fan > 50)
        , testCase "output curve samples are present for firing rules" $ do
            let resp = runMamdani (fanRequest 10)
                curve = Map.lookup "fan" (mrsOutputCurves resp)
            assertBool "fan curve non-empty" (maybe False (not . null) curve)
        , testCase "rule strengths stay in [0,1]" $ do
            let resp = runMamdani (fanRequest 25)
            assertBool
                "strengths in [0,1]"
                (all (\s -> s >= 0 && s <= 1) (mrsRuleStrengths resp))
        ]

fanRequest :: Double -> MamdaniRequest
fanRequest temp =
    MamdaniRequest
        { mrDefs =
            MembershipFuncDefs
                { mfdInputs =
                    [ VarDef
                        "temperature"
                        (0, 40)
                        [ TermDef "cold" (0, 0, 20)
                        , TermDef "hot" (20, 40, 40)
                        ]
                    ]
                , mfdOutputs =
                    [ VarDef
                        "fan"
                        (0, 100)
                        [ TermDef "low" (0, 0, 50)
                        , TermDef "high" (50, 100, 100)
                        ]
                    ]
                }
        , mrRules =
            [ RuleDef [("temperature", "cold")] [("fan", "low")]
            , RuleDef [("temperature", "hot")] [("fan", "high")]
            ]
        , mrValues = Map.fromList [("temperature", temp)]
        }

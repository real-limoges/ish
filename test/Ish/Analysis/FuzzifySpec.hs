module Ish.Analysis.FuzzifySpec (fuzzifyTests) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Hazy (FIS (..), FuzzyRule (..))

import Ish.Analysis.DataFrame (fillMissingDates)
import Ish.Analysis.Fuzzify (
    buildFIS,
    buildMoodFIS,
    defaultMembershipFuncDefs,
    moodRules,
    suggestMembershipFuncDefs,
 )
import Ish.Fixtures (sampleEntries)
import Ish.Types (MembershipFuncDefs (..), VarDef (..))

fuzzifyTests :: TestTree
fuzzifyTests =
    testGroup
        "Analysis.Fuzzify"
        [ testCase "moodRules has six rules" $
            assertEqual "" 6 (length moodRules)
        , testCase "buildMoodFIS wires moodRules into the FIS" $
            assertEqual
                "rule count"
                (length moodRules)
                (length (fisRules (buildMoodFIS defaultMembershipFuncDefs)))
        , testCase "buildMoodFIS exposes all five mood input vars" $ do
            let fis = buildMoodFIS defaultMembershipFuncDefs
                inputNames = Map.keys (fisInputs fis) :: [Text]
            assertEqual
                "input names sorted"
                ["anxiety", "outlook", "sensitivity", "sleep", "speed"]
                inputNames
        , testCase "buildFIS passes custom rules through unchanged" $ do
            let customRule = FuzzyRule [("sleep", "high")] [("wellbeing", "high")]
                fis = buildFIS [customRule] defaultMembershipFuncDefs
            assertEqual "rule count" 1 (length (fisRules fis))
        , testCase "buildFIS with empty rules yields a rule-less FIS" $ do
            let fis = buildFIS [] defaultMembershipFuncDefs
            assertEqual "" 0 (length (fisRules fis))
        , testCase "suggestMembershipFuncDefs preserves bounds and output defs" $ do
            let df = fillMissingDates sampleEntries
                suggested = suggestMembershipFuncDefs defaultMembershipFuncDefs df
            assertEqual
                "outputs unchanged"
                (mfdOutputs defaultMembershipFuncDefs)
                (mfdOutputs suggested)
            assertEqual
                "input count unchanged"
                (length (mfdInputs defaultMembershipFuncDefs))
                (length (mfdInputs suggested))
            assertEqual
                "input bounds unchanged"
                (map varBounds (mfdInputs defaultMembershipFuncDefs))
                (map varBounds (mfdInputs suggested))
        , testCase "suggestMembershipFuncDefs produces three terms per input" $ do
            let df = fillMissingDates sampleEntries
                suggested = suggestMembershipFuncDefs defaultMembershipFuncDefs df
            assertBool
                "every input has 3 terms"
                (all (\v -> length (varTerms v) == 3) (mfdInputs suggested))
        ]

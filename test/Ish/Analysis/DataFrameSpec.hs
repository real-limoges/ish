module Ish.Analysis.DataFrameSpec (dataFrameTests) where

import Data.Map.Strict qualified as Map
import Data.Time.Calendar (fromGregorian)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Ish.Analysis.DataFrame (extractEntries, fillMissingDates, identifyGaps)
import Ish.Fixtures (entry1, entry2, entry3)
import Ish.Types (Gap (..), MoodDimension (..), MoodEntry (..))

dataFrameTests :: TestTree
dataFrameTests =
    testGroup
        "Analysis.DataFrame"
        [ testCase "fillMissingDates then extractEntries round-trips contiguous entries" $ do
            let df = fillMissingDates [entry1, entry2, entry3]
                back = extractEntries df
            assertEqual "entry count" 3 (length back)
            assertEqual
                "entry dates preserved"
                (map entryDate [entry1, entry2, entry3])
                (map entryDate back)
        , testCase "fillMissingDates inserts placeholder rows for missing dates" $ do
            let df = fillMissingDates [entry1, entry3]
                back = extractEntries df
            assertEqual "only present entries survive extractEntries" 2 (length back)
        , testCase "identifyGaps returns empty for no entries" $
            assertEqual "" [] (identifyGaps [])
        , testCase "identifyGaps returns empty for a single entry" $
            assertEqual "" [] (identifyGaps [entry1])
        , testCase "identifyGaps returns empty for contiguous entries" $
            assertEqual "" [] (identifyGaps [entry1, entry2, entry3])
        , testCase "identifyGaps finds a single gap between present runs" $ do
            let jan5 =
                    MoodEntry
                        (fromGregorian 2024 1 5)
                        ( Map.fromList
                            [ (Sleep, 5)
                            , (Anxiety, 2)
                            , (Sensitivity, 2)
                            , (Outlook, 6)
                            , (Speed, 5)
                            ]
                        )
                gaps = identifyGaps [entry1, jan5]
            assertEqual "one gap" 1 (length gaps)
            case gaps of
                [g] -> do
                    assertEqual "gap length" 3 (gapLength g)
                    assertEqual "gap before" (entryDate entry1) (gapBefore g)
                    assertEqual "gap after" (fromGregorian 2024 1 5) (gapAfter g)
                _ -> assertBool "unexpected gap count" False
        ]

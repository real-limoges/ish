module Ish.Fixtures (
    entry1,
    entry2,
    entry3,
    sampleEntries,
    sameDimsAt,
) where

import Data.Map.Strict qualified as Map
import Data.Time.Calendar (Day, fromGregorian)

import Ish.Types (MoodDimension (..), MoodEntry (..))

entry1 :: MoodEntry
entry1 =
    MoodEntry
        { entryDate = fromGregorian 2024 1 1
        , entryDimensions = Map.fromList [(Sleep, 7), (Anxiety, 2), (Sensitivity, 3), (Outlook, 8), (Speed, 6)]
        }

entry2 :: MoodEntry
entry2 =
    MoodEntry
        { entryDate = fromGregorian 2024 1 2
        , entryDimensions = Map.fromList [(Sleep, 6), (Anxiety, 3), (Sensitivity, 4), (Outlook, 7), (Speed, 5)]
        }

entry3 :: MoodEntry
entry3 =
    MoodEntry
        { entryDate = fromGregorian 2024 1 3
        , entryDimensions = Map.fromList [(Sleep, 8), (Anxiety, 1), (Sensitivity, 2), (Outlook, 9), (Speed, 7)]
        }

sampleEntries :: [MoodEntry]
sampleEntries = [entry1, entry2, entry3]

sameDimsAt :: Day -> MoodEntry
sameDimsAt d =
    MoodEntry
        { entryDate = d
        , entryDimensions =
            Map.fromList
                [ (Sleep, 6)
                , (Anxiety, 3)
                , (Sensitivity, 3)
                , (Outlook, 6)
                , (Speed, 5)
                ]
        }

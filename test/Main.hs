module Main (main) where

import Data.Map.Strict qualified as Map
import Data.Time.Calendar (fromGregorian)
import Database.SQLite.Simple (Connection, execute)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import Ish.Db (fetchAllEntries, fetchEntriesInRange, initDb, openDb)
import Ish.Types (MoodDimension (..), MoodEntry (..))

insertEntry :: Connection -> MoodEntry -> IO ()
insertEntry conn e =
    execute
        conn
        "INSERT INTO mood_entries (date, sleep, anxiety, sensitivity, outlook, mental_speed) VALUES (?, ?, ?, ?, ?, ?)"
        ( entryDate e
        , dims Map.! Sleep
        , dims Map.! Anxiety
        , dims Map.! Sensitivity
        , dims Map.! Outlook
        , dims Map.! MentalSpeed
        )
  where
    dims = entryDimensions e

entry1 :: MoodEntry
entry1 =
    MoodEntry
        { entryDate = fromGregorian 2024 1 1
        , entryDimensions = Map.fromList [(Sleep, 7), (Anxiety, 2), (Sensitivity, 3), (Outlook, 8), (MentalSpeed, 6)]
        }

entry2 :: MoodEntry
entry2 =
    MoodEntry
        { entryDate = fromGregorian 2024 1 2
        , entryDimensions = Map.fromList [(Sleep, 6), (Anxiety, 3), (Sensitivity, 4), (Outlook, 7), (MentalSpeed, 5)]
        }

entry3 :: MoodEntry
entry3 =
    MoodEntry
        { entryDate = fromGregorian 2024 1 3
        , entryDimensions = Map.fromList [(Sleep, 8), (Anxiety, 1), (Sensitivity, 2), (Outlook, 9), (MentalSpeed, 7)]
        }

main :: IO ()
main =
    defaultMain $
        testGroup
            "Db"
            [ testCase "fetchAllEntries returns empty list for empty db" $ do
                conn <- openDb ":memory:"
                initDb conn
                entries <- fetchAllEntries conn
                assertEqual "" [] entries
            , testCase "fetchAllEntries returns all entries ordered by date" $ do
                conn <- openDb ":memory:"
                initDb conn
                mapM_ (insertEntry conn) [entry2, entry1, entry3]
                entries <- fetchAllEntries conn
                assertEqual "" (map entryDate [entry1, entry2, entry3]) (map entryDate entries)
            , testCase "fetchAllEntries preserves dimension values" $ do
                conn <- openDb ":memory:"
                initDb conn
                insertEntry conn entry1
                entries <- fetchAllEntries conn
                assertEqual "" [entry1] entries
            , testCase "fetchEntriesInRange returns only entries within range" $ do
                conn <- openDb ":memory:"
                initDb conn
                mapM_ (insertEntry conn) [entry1, entry2, entry3]
                entries <- fetchEntriesInRange conn (fromGregorian 2024 1 1) (fromGregorian 2024 1 2)
                assertEqual "" [fromGregorian 2024 1 1, fromGregorian 2024 1 2] (map entryDate entries)
            , testCase "fetchEntriesInRange is inclusive on both ends" $ do
                conn <- openDb ":memory:"
                initDb conn
                mapM_ (insertEntry conn) [entry1, entry2, entry3]
                entries <- fetchEntriesInRange conn (fromGregorian 2024 1 1) (fromGregorian 2024 1 3)
                assertEqual "" 3 (length entries)
            , testCase "fetchEntriesInRange returns empty for out-of-range dates" $ do
                conn <- openDb ":memory:"
                initDb conn
                mapM_ (insertEntry conn) [entry1, entry2, entry3]
                entries <- fetchEntriesInRange conn (fromGregorian 2025 1 1) (fromGregorian 2025 12 31)
                assertEqual "" [] entries
            ]

module Ish.DbSpec (dbTests) where

import Data.Map.Strict qualified as Map
import Data.Time.Calendar (fromGregorian)
import Database.SQLite.Simple (Connection, execute, execute_)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import Ish.Db (fetchEntries, openDb)
import Ish.Fixtures (entry1, entry2, entry3)
import Ish.Types (MoodDimension (..), MoodEntry (..))

dbTests :: TestTree
dbTests =
    testGroup
        "Db"
        [ testCase "fetchEntries with no bounds returns empty list for empty db" $ do
            conn <- openDb ":memory:"
            createSchema conn
            entries <- fetchEntries conn Nothing Nothing
            assertEqual "" [] entries
        , testCase "fetchEntries with no bounds returns all entries ordered by date" $ do
            conn <- openDb ":memory:"
            createSchema conn
            mapM_ (insertEntry conn) [entry2, entry1, entry3]
            entries <- fetchEntries conn Nothing Nothing
            assertEqual "" (map entryDate [entry1, entry2, entry3]) (map entryDate entries)
        , testCase "fetchEntries preserves dimension values" $ do
            conn <- openDb ":memory:"
            createSchema conn
            insertEntry conn entry1
            entries <- fetchEntries conn Nothing Nothing
            assertEqual "" [entry1] entries
        , testCase "fetchEntries with both bounds returns only entries within range" $ do
            conn <- openDb ":memory:"
            createSchema conn
            mapM_ (insertEntry conn) [entry1, entry2, entry3]
            entries <-
                fetchEntries
                    conn
                    (Just (fromGregorian 2024 1 1))
                    (Just (fromGregorian 2024 1 2))
            assertEqual "" [fromGregorian 2024 1 1, fromGregorian 2024 1 2] (map entryDate entries)
        , testCase "fetchEntries with both bounds is inclusive on both ends" $ do
            conn <- openDb ":memory:"
            createSchema conn
            mapM_ (insertEntry conn) [entry1, entry2, entry3]
            entries <-
                fetchEntries
                    conn
                    (Just (fromGregorian 2024 1 1))
                    (Just (fromGregorian 2024 1 3))
            assertEqual "" 3 (length entries)
        , testCase "fetchEntries with both bounds returns empty for out-of-range dates" $ do
            conn <- openDb ":memory:"
            createSchema conn
            mapM_ (insertEntry conn) [entry1, entry2, entry3]
            entries <-
                fetchEntries
                    conn
                    (Just (fromGregorian 2025 1 1))
                    (Just (fromGregorian 2025 12 31))
            assertEqual "" [] entries
        , testCase "fetchEntries with only `from` returns entries on or after that date" $ do
            conn <- openDb ":memory:"
            createSchema conn
            mapM_ (insertEntry conn) [entry1, entry2, entry3]
            entries <- fetchEntries conn (Just (fromGregorian 2024 1 2)) Nothing
            assertEqual
                ""
                [fromGregorian 2024 1 2, fromGregorian 2024 1 3]
                (map entryDate entries)
        , testCase "fetchEntries with only `to` returns entries on or before that date" $ do
            conn <- openDb ":memory:"
            createSchema conn
            mapM_ (insertEntry conn) [entry1, entry2, entry3]
            entries <- fetchEntries conn Nothing (Just (fromGregorian 2024 1 2))
            assertEqual
                ""
                [fromGregorian 2024 1 1, fromGregorian 2024 1 2]
                (map entryDate entries)
        ]

createSchema :: Connection -> IO ()
createSchema conn =
    execute_
        conn
        "CREATE TABLE mood_entries (\
        \  date          TEXT PRIMARY KEY,\
        \  sleep         REAL NOT NULL,\
        \  anxiety       REAL NOT NULL,\
        \  sensitivity   REAL NOT NULL,\
        \  outlook       REAL NOT NULL,\
        \  speed         REAL NOT NULL\
        \)"

insertEntry :: Connection -> MoodEntry -> IO ()
insertEntry conn e =
    execute
        conn
        "INSERT INTO mood_entries (date, sleep, anxiety, sensitivity, outlook, speed) VALUES (?, ?, ?, ?, ?, ?)"
        ( entryDate e
        , dims Map.! Sleep
        , dims Map.! Anxiety
        , dims Map.! Sensitivity
        , dims Map.! Outlook
        , dims Map.! Speed
        )
  where
    dims = entryDimensions e

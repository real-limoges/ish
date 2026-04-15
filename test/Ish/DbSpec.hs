module Ish.DbSpec (dbTests) where

import Data.Map.Strict qualified as Map
import Data.Time.Calendar (fromGregorian)
import Database.SQLite.Simple (Connection, execute, execute_)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import Ish.Db (fetchAllEntries, fetchEntriesInRange, openDb)
import Ish.Fixtures (entry1, entry2, entry3)
import Ish.Types (MoodDimension (..), MoodEntry (..))

dbTests :: TestTree
dbTests =
    testGroup
        "Db"
        [ testCase "fetchAllEntries returns empty list for empty db" $ do
            conn <- openDb ":memory:"
            createSchema conn
            entries <- fetchAllEntries conn
            assertEqual "" [] entries
        , testCase "fetchAllEntries returns all entries ordered by date" $ do
            conn <- openDb ":memory:"
            createSchema conn
            mapM_ (insertEntry conn) [entry2, entry1, entry3]
            entries <- fetchAllEntries conn
            assertEqual "" (map entryDate [entry1, entry2, entry3]) (map entryDate entries)
        , testCase "fetchAllEntries preserves dimension values" $ do
            conn <- openDb ":memory:"
            createSchema conn
            insertEntry conn entry1
            entries <- fetchAllEntries conn
            assertEqual "" [entry1] entries
        , testCase "fetchEntriesInRange returns only entries within range" $ do
            conn <- openDb ":memory:"
            createSchema conn
            mapM_ (insertEntry conn) [entry1, entry2, entry3]
            entries <- fetchEntriesInRange conn (fromGregorian 2024 1 1) (fromGregorian 2024 1 2)
            assertEqual "" [fromGregorian 2024 1 1, fromGregorian 2024 1 2] (map entryDate entries)
        , testCase "fetchEntriesInRange is inclusive on both ends" $ do
            conn <- openDb ":memory:"
            createSchema conn
            mapM_ (insertEntry conn) [entry1, entry2, entry3]
            entries <- fetchEntriesInRange conn (fromGregorian 2024 1 1) (fromGregorian 2024 1 3)
            assertEqual "" 3 (length entries)
        , testCase "fetchEntriesInRange returns empty for out-of-range dates" $ do
            conn <- openDb ":memory:"
            createSchema conn
            mapM_ (insertEntry conn) [entry1, entry2, entry3]
            entries <- fetchEntriesInRange conn (fromGregorian 2025 1 1) (fromGregorian 2025 12 31)
            assertEqual "" [] entries
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

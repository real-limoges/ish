module Ish.Db (
    openDb,
    initDb,
    fetchAllEntries,
    fetchEntriesInRange,
) where

import Data.Map.Strict qualified as Map
import Data.Time.Calendar (Day)
import Database.SQLite.Simple (Connection, Query, execute_, open, query, query_)

import Ish.Types (MoodDimension (..), MoodEntry (..))

openDb :: FilePath -> IO Connection
openDb = open

{- | Fetch all mood entries from the database, ordered by date ascending.

Maps the 5 columns (sleep, anxiety, sensitivity, outlook, mental_speed) to
'MoodDimension' values.  This is the single place where DB column names
are mapped to domain types.
-}
fetchAllEntries :: Connection -> IO [MoodEntry]
fetchAllEntries conn = do
    rows <- query_ conn allEntriesQuery
    pure $ map rowToEntry rows

-- | Fetch mood entries within a date range (inclusive), ordered by date.
fetchEntriesInRange :: Connection -> Day -> Day -> IO [MoodEntry]
fetchEntriesInRange conn from to = do
    rows <- query conn rangeQuery (from, to)
    pure $ map rowToEntry rows

allEntriesQuery :: Query
allEntriesQuery =
    "SELECT date, sleep, anxiety, sensitivity, outlook, mental_speed \
    \FROM mood_entries ORDER BY date ASC"

rangeQuery :: Query
rangeQuery =
    "SELECT date, sleep, anxiety, sensitivity, outlook, mental_speed \
    \FROM mood_entries WHERE date >= ? AND date <= ? ORDER BY date ASC"

rowToEntry :: (Day, Double, Double, Double, Double, Double) -> MoodEntry
rowToEntry (day, sl, ax, se, ol, ms) =
    MoodEntry
        { entryDate = day
        , entryDimensions =
            Map.fromList
                [ (Sleep, sl)
                , (Anxiety, ax)
                , (Sensitivity, se)
                , (Outlook, ol)
                , (MentalSpeed, ms)
                ]
        }

initDb :: Connection -> IO ()
initDb conn =
    execute_
        conn
        "CREATE TABLE IF NOT EXISTS mood_entries (\
        \  date          TEXT PRIMARY KEY,\
        \  sleep         REAL NOT NULL CHECK (sleep         >= 0 AND sleep         <= 15),\
        \  anxiety       REAL NOT NULL CHECK (anxiety       >= 0 AND anxiety       <= 5),\
        \  sensitivity   REAL NOT NULL CHECK (sensitivity   >= 0 AND sensitivity   <= 5),\
        \  outlook       REAL NOT NULL CHECK (outlook       >= 0 AND outlook       <= 10),\
        \  mental_speed  REAL NOT NULL CHECK (mental_speed  >= 0 AND mental_speed  <= 10)\
        \)"

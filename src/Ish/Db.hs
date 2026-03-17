module Ish.Db (
    openDb,
    initDb,
    fetchAllEntries,
    fetchEntriesInRange,
) where

import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Time.Calendar (Day)
import Database.SQLite.Simple (Connection, Query, execute_, open, query, query_)

import Ish.Types (MoodDimension (..), MoodEntry (..), Score, mkScore)

-- | Open a SQLite connection from a file path.
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
    pure $ mapMaybe rowToEntry rows

-- | Fetch mood entries within a date range (inclusive), ordered by date.
fetchEntriesInRange :: Connection -> Day -> Day -> IO [MoodEntry]
fetchEntriesInRange conn from to = do
    rows <- query conn rangeQuery (from, to)
    pure $ mapMaybe rowToEntry rows

-- ---------------------------------------------------------------------------
-- Queries
-- ---------------------------------------------------------------------------

allEntriesQuery :: Query
allEntriesQuery =
    "SELECT date, sleep, anxiety, sensitivity, outlook, mental_speed \
    \FROM mood_entries ORDER BY date ASC"

rangeQuery :: Query
rangeQuery =
    "SELECT date, sleep, anxiety, sensitivity, outlook, mental_speed \
    \FROM mood_entries WHERE date >= ? AND date <= ? ORDER BY date ASC"

-- ---------------------------------------------------------------------------
-- Row mapping
-- ---------------------------------------------------------------------------

{- | Convert a raw DB row to a 'MoodEntry'.  Returns 'Nothing' if any score
is out of range (should not happen if CHECK constraints are honoured).
-}
rowToEntry :: (Day, Double, Double, Double, Double, Double) -> Maybe MoodEntry
rowToEntry (day, e, v, a, f, s) = do
    eS <- mkScore e
    vS <- mkScore v
    aS <- mkScore a
    fS <- mkScore f
    sS <- mkScore s
    pure
        MoodEntry
            { entryDate = day
            , entryDimensions = buildDimMap eS vS aS fS sS
            }

buildDimMap :: Score -> Score -> Score -> Score -> Score -> Map.Map MoodDimension Score
buildDimMap sl ax se ol ms =
    Map.fromList
        [ (Sleep, sl)
        , (Anxiety, ax)
        , (Sensitivity, se)
        , (Outlook, ol)
        , (MentalSpeed, ms)
        ]

-- ---------------------------------------------------------------------------
-- Schema bootstrap
-- ---------------------------------------------------------------------------

{- | Create the mood_entries table if it doesn't exist.  Safe to call on
every startup.
-}
initDb :: Connection -> IO ()
initDb conn =
    execute_
        conn
        "CREATE TABLE IF NOT EXISTS mood_entries (\
        \  date          TEXT PRIMARY KEY,\
        \  sleep         REAL NOT NULL CHECK (sleep         >= 0 AND sleep         <= 1),\
        \  anxiety       REAL NOT NULL CHECK (anxiety       >= 0 AND anxiety       <= 1),\
        \  sensitivity   REAL NOT NULL CHECK (sensitivity   >= 0 AND sensitivity   <= 1),\
        \  outlook       REAL NOT NULL CHECK (outlook       >= 0 AND outlook       <= 1),\
        \  mental_speed  REAL NOT NULL CHECK (mental_speed  >= 0 AND mental_speed  <= 1)\
        \)"

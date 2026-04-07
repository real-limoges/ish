module Ish.Db (
    openDb,
    fetchAllEntries,
    fetchEntriesInRange,
) where

import Data.Map.Strict qualified as Map
import Data.Time.Calendar (Day)
import Database.SQLite.Simple (Connection, Query, open, query, query_)

import Ish.Types (MoodDimension (..), MoodEntry (..))

openDb :: FilePath -> IO Connection
openDb = open

{- | Fetch all mood entries from the database, ordered by date ascending.

Maps the 5 columns (sleep, anxiety, sensitivity, outlook, speed) to
'MoodDimension' values.  This is the single place where DB column names
are mapped to domain types.
-}
fetchAllEntries :: Connection -> IO [MoodEntry]
fetchAllEntries conn = do
    rows <- query_ conn allEntriesQuery
    pure $ map rowToEntry rows

fetchEntriesInRange :: Connection -> Day -> Day -> IO [MoodEntry]
fetchEntriesInRange conn from to = do
    rows <- query conn rangeQuery (from, to)
    pure $ map rowToEntry rows

allEntriesQuery :: Query
allEntriesQuery =
    "SELECT date, sleep, anxiety, sensitivity, outlook, speed \
    \FROM mood_entries ORDER BY date ASC"

rangeQuery :: Query
rangeQuery =
    "SELECT date, sleep, anxiety, sensitivity, outlook, speed \
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
                , (Speed, ms)
                ]
        }

module Ish.Db (
    openDb,
    fetchEntries,
) where

import Data.Map.Strict qualified as Map
import Data.Time.Calendar (Day)
import Database.SQLite.Simple (
    Connection,
    Only (..),
    Query,
    open,
    query,
    query_,
 )

import Ish.Types (MoodDimension (..), MoodEntry (..))

openDb :: FilePath -> IO Connection
openDb = open

{- | Fetch mood entries with optional date bounds (inclusive), ordered by date
ascending. Pass 'Nothing' on either side to leave that bound open.

  * @fetchEntries conn Nothing Nothing@        — all entries
  * @fetchEntries conn (Just d) Nothing@       — d and later
  * @fetchEntries conn Nothing (Just d)@       — d and earlier
  * @fetchEntries conn (Just a) (Just b)@      — a..b inclusive

Maps the 5 columns (sleep, anxiety, sensitivity, outlook, speed) to
'MoodDimension' values. This is the single place where DB column names
are mapped to domain types.
-}
fetchEntries :: Connection -> Maybe Day -> Maybe Day -> IO [MoodEntry]
fetchEntries conn mFrom mTo = do
    rows <- case (mFrom, mTo) of
        (Nothing, Nothing) ->
            query_ conn (selectCols <> orderBy)
        (Just from, Nothing) ->
            query conn (selectCols <> " WHERE date >= ?" <> orderBy) (Only from)
        (Nothing, Just to) ->
            query conn (selectCols <> " WHERE date <= ?" <> orderBy) (Only to)
        (Just from, Just to) ->
            query conn (selectCols <> " WHERE date >= ? AND date <= ?" <> orderBy) (from, to)
    pure $ map rowToEntry rows

selectCols :: Query
selectCols = "SELECT date, sleep, anxiety, sensitivity, outlook, speed FROM mood_entries"

orderBy :: Query
orderBy = " ORDER BY date ASC"

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

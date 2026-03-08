module Ish.Db
  ( openDb
  , fetchAllEntries
  ) where

import Database.SQLite.Simple (Connection, open)

import Ish.Types (MoodEntry)

-- | Open a SQLite connection from a file path.
openDb :: FilePath -> IO Connection
openDb = open

-- | Fetch all mood entries from the database.
--
-- Maps the 5 columns (energy, valence, anxiety, focus, sociability) to
-- 'MoodDimension' values. This is the single place where DB column names
-- are mapped to domain types.
fetchAllEntries :: Connection -> IO [MoodEntry]
fetchAllEntries _conn = error "TODO: implement fetchAllEntries"

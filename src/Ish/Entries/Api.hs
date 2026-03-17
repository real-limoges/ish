module Ish.Entries.Api (
    EntriesApi,
) where

import Data.Time.Calendar (Day)
import Servant.API (Get, JSON, QueryParam, (:>))

import Ish.Types (MoodEntry)

{- | Read-only endpoints for mood entries.

GET  /entries                           -> all entries
GET  /entries?from=YYYY-MM-DD&to=...   -> entries in date range
-}
type EntriesApi =
    "entries"
        :> QueryParam "from" Day
        :> QueryParam "to" Day
        :> Get '[JSON] [MoodEntry]

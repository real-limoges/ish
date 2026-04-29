module Ish.Entries.Server (
    entriesServer,
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Time.Calendar (Day)
import Servant (ServerT)

import Ish.App (AppEnv (..), AppM)
import Ish.Db (fetchEntries)
import Ish.Entries.Api (EntriesApi)
import Ish.Types (MoodEntry)

entriesServer :: ServerT EntriesApi AppM
entriesServer = getEntries

getEntries :: Maybe Day -> Maybe Day -> AppM [MoodEntry]
getEntries mFrom mTo = do
    conn <- asks envConnection
    liftIO $ fetchEntries conn mFrom mTo

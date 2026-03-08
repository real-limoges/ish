module Ish.App
  ( Config (..)
  , AppEnv (..)
  , AppM
  , runAppM
  ) where

import Control.Monad.Reader (ReaderT, runReaderT)
import Database.SQLite.Simple (Connection)
import Servant (Handler)

-- | Application configuration.
data Config = Config
  { configPort   :: Int
  , configDbPath :: FilePath
  } deriving stock (Show)

-- | Runtime environment available to all handlers.
data AppEnv = AppEnv
  { envConfig     :: Config
  , envConnection :: Connection
  }

-- | The application monad: ReaderT over Servant's Handler.
type AppM = ReaderT AppEnv Handler

-- | Natural transformation from AppM to Handler.
runAppM :: AppEnv -> AppM a -> Handler a
runAppM env action = runReaderT action env

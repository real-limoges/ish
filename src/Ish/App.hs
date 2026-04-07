module Ish.App (
    Config (..),
    AppEnv (..),
    AppM,
    runAppM,
) where

import Control.Monad.Reader (ReaderT, runReaderT)
import Data.IORef (IORef)
import Database.SQLite.Simple (Connection)
import Servant (Handler)

import Ish.Types (MembershipFuncDefs)

data Config = Config
    { configPort :: Int
    , configDbPath :: FilePath
    }
    deriving stock (Show)

data AppEnv = AppEnv
    { envConfig :: Config
    , envConnection :: Connection
    , envMembershipFns :: IORef MembershipFuncDefs
    }

type AppM = ReaderT AppEnv Handler

-- | Natural transformation for Servant's hoistServer.
runAppM :: AppEnv -> AppM a -> Handler a
runAppM env action = runReaderT action env

module Main (main) where

import Control.Applicative ((<|>))
import Data.IORef (newIORef)
import Data.Maybe (fromMaybe)
import Network.Wai.Handler.Warp (run)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import Ish.Analysis.Fuzzify (defaultMembershipFuncDefs)
import Ish.Api (app)
import Ish.App (AppEnv (..), Config (..))
import Ish.Db (openDb)

main :: IO ()
main = do
    -- Cloud Run sets PORT; fall back to ISH_PORT for local dev
    portStr <- lookupEnv "PORT"
    ishPortStr <- lookupEnv "ISH_PORT"
    let port = fromMaybe 8080 ((portStr >>= readMaybe) <|> (ishPortStr >>= readMaybe))
    dbPath <- fromMaybe "data/ish.db" <$> lookupEnv "ISH_DB_PATH"
    let config = Config{configPort = port, configDbPath = dbPath}
    conn <- openDb dbPath
    mfRef <- newIORef defaultMembershipFuncDefs
    let env = AppEnv{envConfig = config, envConnection = conn, envMembershipFns = mfRef}
    putStrLn $ "Starting ish on port " <> show port
    run port (app env)

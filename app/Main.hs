module Main (main) where

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
    port <- fromMaybe 8080 . (>>= readMaybe) <$> lookupEnv "ISH_PORT"
    dbPath <- fromMaybe "data/ish.db" <$> lookupEnv "ISH_DB_PATH"
    let config = Config{configPort = port, configDbPath = dbPath}
    conn <- openDb dbPath
    mfRef <- newIORef defaultMembershipFuncDefs
    let env = AppEnv{envConfig = config, envConnection = conn, envMembershipFns = mfRef}
    putStrLn $ "Starting ish on port " <> show port
    run port (app env)

module Main (main) where

import Network.Wai.Handler.Warp (run)
import System.Environment (lookupEnv)

import Ish.Api (app)
import Ish.App (AppEnv (..), Config (..))
import Ish.Db (openDb)

main :: IO ()
main = do
  port <- maybe 8080 read <$> lookupEnv "ISH_PORT"
  dbPath <- maybe "ish.db" id <$> lookupEnv "ISH_DB_PATH"
  let config = Config { configPort = port, configDbPath = dbPath }
  conn <- openDb dbPath
  let env = AppEnv { envConfig = config, envConnection = conn }
  putStrLn $ "Starting ish on port " <> show port
  run port (app env)

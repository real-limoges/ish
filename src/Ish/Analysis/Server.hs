module Ish.Analysis.Server (
    analysisServer,
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Text (Text)
import Servant (ServerT, (:<|>) (..))

import Ish.Analysis.Api (AnalysisApi)
import Ish.Analysis.Fuzzy (analyzeMoodEntries, clusterEntries)
import Ish.App (AppEnv (..), AppM)
import Ish.Db (fetchAllEntries)
import Ish.Types (AnalysisResult, MoodCluster)

analysisServer :: ServerT AnalysisApi AppM
analysisServer = healthHandler :<|> analysisHandler :<|> clustersHandler

healthHandler :: AppM Text
healthHandler = pure "ok"

analysisHandler :: AppM AnalysisResult
analysisHandler = do
    conn <- asks envConnection
    entries <- liftIO $ fetchAllEntries conn
    pure $ analyzeMoodEntries entries

clustersHandler :: AppM [MoodCluster]
clustersHandler = do
    conn <- asks envConnection
    entries <- liftIO $ fetchAllEntries conn
    pure $ clusterEntries entries

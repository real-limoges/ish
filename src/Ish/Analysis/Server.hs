module Ish.Analysis.Server (
    analysisServer,
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Text (Text)
import Servant (ServerT, (:<|>) (..))

import Ish.Analysis.Api (AnalysisApi)
import Ish.Analysis.DataFrame (fillMissingDates)
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
    df <- fillMissingDates <$> liftIO (fetchAllEntries conn)
    pure $ analyzeMoodEntries df

clustersHandler :: AppM [MoodCluster]
clustersHandler = do
    conn <- asks envConnection
    df <- fillMissingDates <$> liftIO (fetchAllEntries conn)
    pure $ clusterEntries df

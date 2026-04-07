module Ish.Analysis.Server (
    analysisServer,
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.IORef (readIORef, writeIORef)
import Data.Text (Text)
import Servant (ServerT, (:<|>) (..))

import Ish.Analysis.Api (AnalysisApi)
import Ish.Analysis.Cluster (ClusterConfig (..), ClusterResult, clusterMoodData)
import Ish.Analysis.DataFrame (fillMissingDates)
import Ish.Analysis.Fuzzy (analyzeMoodEntries, clusterEntries)
import Ish.Analysis.Gaps (GapAnalysis, analyzeGaps)
import Ish.App (AppEnv (..), AppM)
import Ish.Db (fetchAllEntries)
import Ish.Types (AnalysisResult, MembershipFuncDefs, MoodCluster, MoodEntry)

analysisServer :: ServerT AnalysisApi AppM
analysisServer =
    healthHandler
        :<|> analysisHandler
        :<|> clustersHandler
        :<|> dataHandler
        :<|> clusterHandler
        :<|> gapsHandler
        :<|> getMembershipFnsHandler
        :<|> postMembershipFnsHandler

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

dataHandler :: AppM [MoodEntry]
dataHandler = do
    conn <- asks envConnection
    liftIO $ fetchAllEntries conn

clusterHandler :: ClusterConfig -> AppM ClusterResult
clusterHandler cfg = do
    conn <- asks envConnection
    df <- fillMissingDates <$> liftIO (fetchAllEntries conn)
    pure $ clusterMoodData cfg df

gapsHandler :: AppM GapAnalysis
gapsHandler = do
    conn <- asks envConnection
    entries <- liftIO $ fetchAllEntries conn
    let df = fillMissingDates entries
        cr = clusterMoodData defaultCfg df
    pure $ analyzeGaps df cr
  where
    defaultCfg = ClusterConfig{clusterK = 3, clusterM = 2.0}

getMembershipFnsHandler :: AppM MembershipFuncDefs
getMembershipFnsHandler = do
    ref <- asks envMembershipFns
    liftIO $ readIORef ref

postMembershipFnsHandler :: MembershipFuncDefs -> AppM MembershipFuncDefs
postMembershipFnsHandler defs = do
    ref <- asks envMembershipFns
    liftIO $ writeIORef ref defs
    pure defs

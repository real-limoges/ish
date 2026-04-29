module Ish.Analysis.Server (
    analysisServer,
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.IORef (readIORef, writeIORef)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Hazy (FIS)
import Servant (ServerT, (:<|>) (..))

import Ish.Analysis.Api (AnalysisApi)
import Ish.Analysis.Cluster (ClusterConfig (..), ClusterResult, clusterMoodData)
import Ish.Analysis.DataFrame (fillMissingDates)
import Ish.Analysis.Fuzzify (buildMoodFIS, suggestMembershipFuncDefs)
import Ish.Analysis.Fuzzy (analyzeMoodEntries, clusterEntries)
import Ish.Analysis.Gaps (GapAnalysis, analyzeGaps)
import Ish.Analysis.Mamdani (runMamdani)
import Ish.App (AppEnv (..), AppM)
import Ish.Db (fetchEntries)
import Ish.Types (
    AnalysisResult,
    MamdaniRequest,
    MamdaniResponse,
    MembershipFuncDefs,
    MoodCluster,
    MoodEntry,
 )

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
        :<|> suggestMembershipFnsHandler
        :<|> mamdaniHandler

healthHandler :: AppM Text
healthHandler = pure "ok"

{- | Reads the current MembershipFuncDefs from the AppEnv IORef and builds a FIS.
Every handler that runs the fuzzify/cluster pipeline calls this so that
POST /membership-functions edits take effect on subsequent requests.
-}
currentFis :: AppM FIS
currentFis = do
    ref <- asks envMembershipFns
    defs <- liftIO $ readIORef ref
    pure (buildMoodFIS defs)

analysisHandler :: Maybe Day -> Maybe Day -> AppM AnalysisResult
analysisHandler mFrom mTo = do
    conn <- asks envConnection
    fis <- currentFis
    df <- fillMissingDates <$> liftIO (fetchEntries conn mFrom mTo)
    pure $ analyzeMoodEntries fis df

clustersHandler :: Maybe Day -> Maybe Day -> AppM [MoodCluster]
clustersHandler mFrom mTo = do
    conn <- asks envConnection
    fis <- currentFis
    df <- fillMissingDates <$> liftIO (fetchEntries conn mFrom mTo)
    pure $ clusterEntries fis df

dataHandler :: Maybe Day -> Maybe Day -> AppM [MoodEntry]
dataHandler mFrom mTo = do
    conn <- asks envConnection
    liftIO $ fetchEntries conn mFrom mTo

clusterHandler :: Maybe Day -> Maybe Day -> ClusterConfig -> AppM ClusterResult
clusterHandler mFrom mTo cfg = do
    conn <- asks envConnection
    fis <- currentFis
    df <- fillMissingDates <$> liftIO (fetchEntries conn mFrom mTo)
    pure $ clusterMoodData fis cfg df

gapsHandler :: Maybe Day -> Maybe Day -> AppM GapAnalysis
gapsHandler mFrom mTo = do
    conn <- asks envConnection
    fis <- currentFis
    entries <- liftIO $ fetchEntries conn mFrom mTo
    let df = fillMissingDates entries
        cr = clusterMoodData fis defaultCfg df
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

{- | Derives candidate MembershipFuncDefs from the data distribution within the
given window (percentile-based). Does NOT mutate envMembershipFns — the caller
decides whether to apply the result via POST /membership-functions.
-}
suggestMembershipFnsHandler :: Maybe Day -> Maybe Day -> AppM MembershipFuncDefs
suggestMembershipFnsHandler mFrom mTo = do
    conn <- asks envConnection
    ref <- asks envMembershipFns
    current <- liftIO $ readIORef ref
    entries <- liftIO $ fetchEntries conn mFrom mTo
    pure $ suggestMembershipFuncDefs current (fillMissingDates entries)

{- | Pure, stateless Mamdani inference driven entirely by the request body.
  No database, no AppEnv — the caller supplies MFs, rules, and crisp inputs,
  and gets back the full intermediate trace for visualization.
-}
mamdaniHandler :: MamdaniRequest -> AppM MamdaniResponse
mamdaniHandler = pure . runMamdani

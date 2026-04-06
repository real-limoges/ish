module Ish.Analysis.Api (
    AnalysisApi,
) where

import Data.Text (Text)
import Servant.API (Get, JSON, Post, ReqBody, (:<|>), (:>))

import Ish.Analysis.Cluster (ClusterConfig, ClusterResult)
import Ish.Analysis.Gaps (GapAnalysis)
import Ish.Types (AnalysisResult, MoodCluster, MoodEntry)

-- | The analysis API.
type AnalysisApi =
    "health" :> Get '[JSON] Text
        :<|> "analysis" :> Get '[JSON] AnalysisResult
        :<|> "analysis" :> "clusters" :> Get '[JSON] [MoodCluster]
        :<|> "data" :> Get '[JSON] [MoodEntry]
        :<|> "cluster" :> ReqBody '[JSON] ClusterConfig :> Post '[JSON] ClusterResult
        :<|> "gaps" :> Get '[JSON] GapAnalysis

module Ish.Analysis.Api (
    AnalysisApi,
) where

import Data.Text (Text)
import Data.Time.Calendar (Day)
import Servant.API (Get, JSON, Post, QueryParam, ReqBody, (:<|>), (:>))

import Ish.Analysis.Cluster (ClusterConfig, ClusterResult)
import Ish.Analysis.Gaps (GapAnalysis)
import Ish.Types (
    AnalysisResult,
    MamdaniRequest,
    MamdaniResponse,
    MembershipFuncDefs,
    MoodCluster,
    MoodEntry,
 )

{- | All data-derived endpoints accept optional @from@/@to@ 'Day' query params
(inclusive). Omit either to leave that bound open. The membership-function
GET/POST endpoints are config, not data-derived, so they take no dates.
-}
type AnalysisApi =
    "health" :> Get '[JSON] Text
        :<|> "analysis"
            :> QueryParam "from" Day
            :> QueryParam "to" Day
            :> Get '[JSON] AnalysisResult
        :<|> "analysis"
            :> "clusters"
            :> QueryParam "from" Day
            :> QueryParam "to" Day
            :> Get '[JSON] [MoodCluster]
        :<|> "data"
            :> QueryParam "from" Day
            :> QueryParam "to" Day
            :> Get '[JSON] [MoodEntry]
        :<|> "cluster"
            :> QueryParam "from" Day
            :> QueryParam "to" Day
            :> ReqBody '[JSON] ClusterConfig
            :> Post '[JSON] ClusterResult
        :<|> "gaps"
            :> QueryParam "from" Day
            :> QueryParam "to" Day
            :> Get '[JSON] GapAnalysis
        :<|> "membership-functions" :> Get '[JSON] MembershipFuncDefs
        :<|> "membership-functions" :> ReqBody '[JSON] MembershipFuncDefs :> Post '[JSON] MembershipFuncDefs
        :<|> "membership-functions"
            :> "suggest"
            :> QueryParam "from" Day
            :> QueryParam "to" Day
            :> Post '[JSON] MembershipFuncDefs
        :<|> "inference" :> "mamdani" :> ReqBody '[JSON] MamdaniRequest :> Post '[JSON] MamdaniResponse

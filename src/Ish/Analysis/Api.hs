module Ish.Analysis.Api
  ( AnalysisApi
  ) where

import Data.Text (Text)
import Servant.API (Get, JSON, (:<|>), (:>))

import Ish.Types (AnalysisResult, MoodCluster)

-- | The analysis API: health check + two read-only analysis endpoints.
type AnalysisApi =
       "health"   :> Get '[JSON] Text
  :<|> "analysis" :> Get '[JSON] AnalysisResult
  :<|> "analysis" :> "clusters" :> Get '[JSON] [MoodCluster]

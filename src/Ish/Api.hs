module Ish.Api (
    API,
    app,
) where

import Network.Wai (Application)
import Servant (Proxy (..), hoistServer, serve, (:<|>) (..))

import Ish.Analysis.Api (AnalysisApi)
import Ish.Analysis.Server (analysisServer)
import Ish.App (AppEnv, runAppM)
import Ish.Entries.Api (EntriesApi)
import Ish.Entries.Server (entriesServer)

type API = AnalysisApi :<|> EntriesApi

apiProxy :: Proxy API
apiProxy = Proxy

app :: AppEnv -> Application
app env =
    serve apiProxy $
        hoistServer
            apiProxy
            (runAppM env)
            (analysisServer :<|> entriesServer)

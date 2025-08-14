module API where

import Servant
import Types
import RINSE
import Data.Time (getCurrentTime)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Control.Monad.IO.Class (liftIO)

type API = "process" :> ReqBody '[JSON] RINSEInput :> Post '[JSON] RINSEOutput

api :: Proxy API
api = Proxy

server :: Server API
server = processHandler

processHandler :: RINSEInput -> Handler RINSEOutput
processHandler input = liftIO $ do
  currentTime <- getCurrentTime
  result <- processExperience $ input { timestamp = currentTime }
  return result

app :: Application
app = logStdoutDev $ serve api server

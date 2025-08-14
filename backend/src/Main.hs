module Main where

import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as A

main :: IO ()
main = do
    putStrLn "Starting LIMINAL backend..."
    run 8080 app

app :: Application
app req respond = do
    let body = A.encode $ A.object ["message" A..= ("LIMINAL backend is running" :: Text)]
    respond $ responseLBS status200 [] body

responseLBS :: Status -> ResponseHeaders -> BL.ByteString -> Response
responseLBS s h b = responseLBS s h b

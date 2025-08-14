module Logger where

import Types
import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 (unpack)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.FilePath ((</>))
import Control.Monad (unless)

-- | Logs RINSEOutput to a JSONL file in the logs directory
logRINSEOutput :: RINSEOutput -> IO ()
logRINSEOutput output = do
    -- Ensure logs directory exists
    let logDir = "logs"
    dirExists <- doesDirectoryExist logDir
    unless dirExists $ createDirectoryIfMissing True logDir
    
    -- Append JSON-encoded output to the file
    let logFile = logDir </> "insights.jsonl"
    appendFile logFile (unpack (encode output) ++ "\n")

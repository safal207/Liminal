module Main where

import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import API
import InsightStats (analyzeEmotionTrends, TimePeriod(..), parseOptions, formatTimeStats)
import FuzzyEmotion (fuzzyInterpret, formatFuzzyResults)
import EmotionSurge (detectEmotionSurge, parseThreshold, formatStatsTable, exportStats, SurgeStats(..))
import AlertEngine (processAlerts, formatAlert, defaultAlertConfig, Alert)
import System.Environment (getArgs, getProgName)
import Data.Text (pack, unpack, Text)
import qualified Data.Map as M
import System.Exit (exitFailure, exitSuccess)
import Data.Time (fromGregorian)
import Data.Maybe (fromMaybe, mapMaybe)
import System.Console.GetOpt (getOpt, usageInfo, ArgOrder(..), OptDescr(..), ArgDescr(..))
import Data.List (intercalate)
import Control.Monad (when)
import Text.Printf (printf)
import Data.Time (Day)
import Data.Text (unpack)
import qualified Data.Text.IO as T

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("analyze":rest) -> doAnalysis rest
        ("analyze-time":rest) -> doTimeAnalysis rest
        ("detect-surges":args) -> do
            case parseSurgeOpts args of
                (opts, nonOpts, []) -> doSurgeDetection opts (if null nonOpts then "logs/insights.jsonl" else head nonOpts)
                (_, _, errs) -> do
                    putStrLn $ concat errs
                    printSurgeUsage
                    exitFailure
                    
        ("alerts":args) -> do
            let filePath = if null args then "logs/insights.jsonl" else head args
            putStrLn "🔔 Analyzing logs for emotional patterns..."
            alerts <- processAlerts defaultAlertConfig filePath
            if null alerts
                then putStrLn "✅ No significant emotional patterns detected."
                else do
                    putStrLn "\n🚨 Emotional Patterns Detected:"
                    mapM_ (putStrLn . formatAlert) alerts
                    -- Export to file
                    let alertFile = "logs/alerts.md"
                    writeFile alertFile $ unlines $ 
                        "# Emotional Pattern Alerts\n" : 
                        map (\a -> "## " ++ unpack (alertTitle a) ++ "\n\n" 
                             ++ unpack (alertDescription a) ++ "\n"
                             ++ "**Dates:** " ++ unpack (formatDay (alertStartDate a)) 
                             ++ (if alertStartDate a /= alertEndDate a 
                                 then " - " ++ unpack (formatDay (alertEndDate a)) 
                                 else "")
                             ++ "\n\n**Recommendations:**\n"
                             ++ unlines (map (("* " ++) . unpack) (alertRecommendations a)))
                            alerts
                    putStrLn $ "\n📝 Full report saved to " ++ alertFile
        _ -> runServer

runServer :: IO ()
runServer = do
    putStrLn "Starting RINSE server..."
    run 3000 $ logStdoutDev app

doAnalysis :: [String] -> IO ()
doAnalysis args = do
    when (null args) $ do
        progName <- getProgName
        putStrLn $ "Usage: " ++ progName ++ " analyze <file>"
        exitFailure
    
    let filePath = head args
    putStrLn $ "Analyzing emotion trends in " ++ filePath
    
    -- Use the existing non-time-based analysis
    stats <- analyzeEmotionTrends filePath Daily  -- Default to daily for backward compatibility
    let fuzzyResults = fuzzyInterpret (concat (M.elems stats))
    
    putStrLn "\n=== Fuzzy Emotion Analysis ===\n"
    putStrLn $ formatFuzzyResults fuzzyResults

doTimeAnalysis :: [String] -> IO ()
doTimeAnalysis args = do
    options <- parseOptions args
    let filePath = optFile options
    
    putStrLn $ "Analyzing emotion trends over time from " ++ filePath
    putStrLn $ "Time period: " ++ show (optPeriod options)
    
    -- Apply filters if specified
    stats <- analyzeEmotionTrends filePath (optPeriod options)
    
    -- Apply emotion filter if specified
    let filteredStats = case optFilterEmotion options of
            Just emotion -> M.map (filter ((== emotion) . emotionName)) stats
            Nothing -> stats
    
    -- Apply date range filter if specified
    let finalStats = case (optStartDate options, optEndDate options) of
            (Just start, Just end) -> M.filterWithKey (\k _ -> 
                case parseTimeM False defaultTimeLocale (if optPeriod options == Daily 
                                                         then "%Y-%m-%d" 
                                                         else "%Y-W%V") k of
                    Just day -> day >= start && day <= end
                    Nothing -> False) filteredStats
            _ -> filteredStats
    
    -- Print results
    if M.null finalStats
        then putStrLn "No data found matching the specified criteria."
        else do
            putStrLn $ "\n=== Emotion Trends by " ++ 
                     (if optPeriod options == Daily then "Day" else "Week") ++
                     " ===\n"
            putStrLn $ formatTimeStats finalStats

data SurgeOptions = SurgeOptions
    { soThresholds :: [EmotionThreshold]
    , soOutputFile :: Maybe FilePath
    , soShowHelp :: Bool
    }

defaultSurgeOptions :: SurgeOptions
defaultSurgeOptions = SurgeOptions
    { soThresholds = defaultThresholds
    , soOutputFile = Nothing
    , soShowHelp = False
    }
  where
    defaultThresholds =
        [ ("страх", 3.5, 3)    -- 3+ days with weight ≥ 3.5
        , ("гнев", 4.0, 2)     -- 2+ days with weight ≥ 4.0
        , ("тревога", 3.0, 3)  -- 3+ days with weight ≥ 3.0
        , ("радость", 4.0, 4)  -- 4+ days with weight ≥ 4.0
        ]

surgeOpts :: [OptDescr (SurgeOptions -> IO SurgeOptions)]
surgeOpts =
    [ Option ["t","threshold"] (ReqArg addThreshold "EMOTION:WEIGHT:DAYS")
        "Add custom threshold (e.g., 'страх:3.5:3')"
    , Option ["o","output"] (ReqArg setOutput "FILE")
        "Output results to JSON file"
    , Option ["h","help"] (NoArg (\_ -> return $ defaultSurgeOptions { soShowHelp = True }))
        "Show this help message"
    ]
  where
    addThreshold arg opt = do
        case parseThreshold arg of
            Just t -> return $ opt { soThresholds = t : soThresholds opt }
            Nothing -> do
                putStrLn $ "Invalid threshold format: " ++ arg
                putStrLn "Expected format: EMOTION:WEIGHT:DAYS (e.g., 'страх:3.5:3')"
                exitFailure
    
    setOutput file opt = return $ opt { soOutputFile = Just file }

parseSurgeOpts :: [String] -> (SurgeOptions, [String], [String])
parseSurgeOpts args = 
    case getOpt Permute surgeOpts args of
        (actions, nonOpts, []) ->
            let (opts, msgs) = foldl' (flip ($)) (defaultSurgeOptions, []) actions
            in (opts, nonOpts, msgs)
        (_, _, errs) -> (defaultSurgeOptions, [], errs)

printSurgeUsage :: IO ()
printSurgeUsage = do
    progName <- getProgName
    putStrLn $ "Usage: " ++ progName ++ " detect-surges [OPTIONS] [FILE]"
    putStrLn "\nOptions:"
    putStrLn $ usageInfo "" surgeOpts
    putStrLn "\nDefault thresholds:"
    mapM_ (putStrLn . formatThreshold) (soThresholds defaultSurgeOptions)
  where
    formatThreshold (e, w, d) = printf "  %-10s  %4.1f  %2d" (unpack e) w d

-- | Detect emotion surges based on thresholds
doSurgeDetection :: SurgeOptions -> FilePath -> IO ()
doSurgeDetection opts filePath = do
    when (soShowHelp opts) $ do
        printSurgeUsage
        exitSuccess
    
    putStrLn $ "🔍 Detecting emotion surges in " ++ filePath
    putStrLn "\n🔔 Using thresholds:"
    mapM_ (putStrLn . formatThreshold) (soThresholds opts)
    
    -- Run surge detection
    result <- detectEmotionSurge filePath (soThresholds opts)
    
    -- Process and display results
    let allStats = concat (M.elems result)
    
    -- Print summary table
    putStrLn "\n📊 Surge Statistics:"
    putStrLn $ formatStatsTable allStats
    
    -- Export to JSON if requested
    case soOutputFile opts of
        Just outFile -> do
            let jsonFile = if ".json" `isSuffixOf` outFile 
                          then outFile 
                          else outFile ++ ".json"
            exportStats jsonFile allStats
        Nothing -> return ()
  where
    formatThreshold (e, w, d) = printf "  %-10s  Weight ≥ %.1f for %d+ days" (unpack e) w d
    isSuffixOf suf str = suf `isSuffixOf'` (reverse str)
    isSuffixOf' _ [] = False
    isSuffixOf' [] _ = True
    isSuffixOf' (x:xs) (y:ys) = x == y && isSuffixOf' xs ys

-- Format day as YYYY-MM-DD
formatDay :: Day -> Text
formatDay = pack . formatTime defaultTimeLocale "%Y-%m-%d"

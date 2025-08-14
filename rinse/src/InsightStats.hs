{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module InsightStats where

import Types
import Data.Aeson (eitherDecode, FromJSON)
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Lazy (readFile)
import qualified Data.ByteString.Char8 as B
import Data.Text (Text, unpack, pack)
import Data.List (sortOn, groupBy, sortBy)
import Data.Ord (Down(..), comparing)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe, catMaybes)
import Control.Monad (forM_, when)
import Text.Printf (printf)
import Data.Vector (toList)
import Data.Time (UTCTime, Day, utctDay, toGregorian, fromGregorian, diffDays)
import Data.Time.Calendar.WeekDate (toWeekDate, fromWeekDate)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..), utcToLocalTime, localDay, midnight, utc, getCurrentTimeZone)
import Data.Time.Calendar (Day(..))
import Data.Function (on)
import Data.Bifunctor (bimap)
import Data.Char (toLower)
import System.Console.GetOpt (ArgOrder(..), OptDescr(..), ArgDescr(..), getOpt, usageInfo)
import System.Environment (getArgs, getProgName)
import Control.Applicative ((<|>))

-- | Time period for aggregation
data TimePeriod = Daily | Weekly deriving (Show, Eq)

-- | Command line options
data Options = Options
    { optPeriod :: TimePeriod
    , optFile   :: FilePath
    , optFilterEmotion :: Maybe Text
    , optStartDate :: Maybe Day
    , optEndDate   :: Maybe Day
    } deriving (Show)

defaultOptions :: Options
defaultOptions = Options
    { optPeriod = Daily
    , optFile = "logs/insights.jsonl"
    , optFilterEmotion = Nothing
    , optStartDate = Nothing
    , optEndDate = Nothing
    }

-- | Statistics for a single emotion
data EmotionStats = EmotionStats
    { emotionName :: Text
    , totalCount  :: Int     -- Total occurrences
    , totalWeight :: Double  -- Sum of all weights
    , avgWeight   :: Double  -- Average weight
    , timestamp   :: Maybe UTCTime  -- Optional timestamp for time-based analysis
    } deriving (Show)

-- | Convert UTCTime to start of day in local timezone
toLocalDay :: UTCTime -> IO Day
toLocalDay utcTime = do
    tz <- getCurrentTimeZone
    let localTime = utcToLocalTime tz utcTime
    return $ localDay localTime

-- | Convert UTCTime to (Year, Week) tuple
-- Returns (Year, Week) where Week is ISO 8601 week number
toYearWeek :: UTCTime -> (Integer, Int)
toYearWeek = toWeekDate . utctDay

-- | Group by time period
groupByPeriod :: TimePeriod -> [(a, UTCTime)] -> IO [(String, [a])]
groupByPeriod period items = do
    let withKey = map (\(a, t) -> (periodToKey period t, a)) items
    return $ groupWith fst withKey
  where
    groupWith f = map (\xs -> (f $ head xs, map snd xs)) . groupBy ((==) `on` fst) . sortBy (comparing fst)

    periodToKey Daily t = 
        let (y, m, d) = toGregorian (utctDay t)
        in printf "%04d-%02d-%02d" y m d
    
    periodToKey Weekly t =
        let (y, w, _) = toWeekDate (utctDay t)
        in printf "%04d-W%02d" y w

-- | Read and parse the log file with timestamps
readLogFile :: FilePath -> IO [(RINSEOutput, UTCTime)]
readLogFile filePath = do
    content <- Data.ByteString.Lazy.readFile filePath
    let lines' = filter (not . null) (map unpack (lines content))
    catMaybes <$> mapM (processLine . (eitherDecode . toStrictBS . pack)) lines'
  where
    toStrictBS = Data.ByteString.Lazy.fromStrict . B.pack
    pack = id
    
    processLine (Left _) = return Nothing
    processLine (Right output) = Just . (output,) . timestamp $> output

-- | Process logs and compute statistics with time-based grouping
analyzeEmotionTrends :: FilePath -> TimePeriod -> IO (Map String [EmotionStats])
analyzeEmotionTrends filePath period = do
    logs <- readLogFile filePath
    
    -- Process all emotion tags with timestamps
    let allEmotions = concatMap \(output, ts) ->
            map (\t -> (emotion t, fromIntegral (weight t), ts)) $ toList (tags output)
            $ output
    
    -- Group by time period
    timeGroups <- groupByPeriod period $ map (\(e, w, t) -> ((e, w), t)) allEmotions
    
    -- Process each time group
    mconcat <$> mapM processTimeGroup timeGroups
  where
    processTimeGroup (periodStr, items) = do
        -- Group by emotion within the time period
        let emotionGroups = M.fromListWith (\(c1, w1) (c2, w2) -> (c1 + c2, w1 + w2))
                         $ map (\((e, w), _) -> (e, (1 :: Int, w))) items
        
        -- Convert to EmotionStats
        let stats = map (\(e, (cnt, wt)) -> 
                    EmotionStats e cnt wt (wt / fromIntegral cnt) Nothing)
                $ M.toList emotionGroups
        
        return $ M.singleton periodStr stats

-- | Get top N emotions by weight
getTopEmotions :: Int -> [EmotionStats] -> [EmotionStats]
getTopEmotions n = take n . sortBy (flip (comparing totalWeight))

-- | Format time-based statistics as a table
formatTimeStats :: Map String [EmotionStats] -> String
formatTimeStats timeMap =
    let header = printf "%-15s %-15s %10s %10s %10s" "Period" "Emotion" "Count" "Avg Wt" "Total Wt"
        separator = replicate 70 '-'
        formatPeriod period stats =
            let topEmotions = getTopEmotions 3 stats
                periodLines = map (formatEmotionLine period) topEmotions
            in unlines periodLines
        formatEmotionLine period stat =
            printf "%-15s %-15s %10d %10.2f %10.2f"
                (take 15 period)  -- Trim period if too long
                (unpack $ emotionName stat)
                (totalCount stat)
                (avgWeight stat)
                (totalWeight stat)
        periods = M.toAscList timeMap
        body = concatMap (uncurry formatPeriod) periods
    in unlines $ header : separator : body

-- | Parse command line arguments
parseOptions :: [String] -> IO Options
parseOptions args = do
    let options =
            [ Option "p" ["period"] (ReqArg (\s -> Right $ \opts -> 
                case map toLower s of
                    "daily"  -> opts { optPeriod = Daily }
                    "weekly" -> opts { optPeriod = Weekly }
                    _        -> error "Period must be 'daily' or 'weekly'"
                ) "PERIOD")
                "Time period for grouping (daily|weekly)"
            , Option "f" ["file"] (ReqArg (\s -> Right $ \opts -> 
                opts { optFile = s }) "FILE")
                "Input file path"
            , Option "e" ["emotion"] (ReqArg (\s -> Right $ \opts -> 
                opts { optFilterEmotion = Just $ pack s }) "EMOTION")
                "Filter by emotion"
            , Option "s" ["start"] (ReqArg (\s -> Right $ \opts -> 
                case parseTimeM False defaultTimeLocale "%Y-%m-%d" s of
                    Just d -> opts { optStartDate = Just d }
                    Nothing -> error "Invalid start date format. Use YYYY-MM-DD"
                ) "START_DATE") 
                "Start date (YYYY-MM-DD)"
            , Option "e" ["end"] (ReqArg (\s -> Right $ \opts -> 
                case parseTimeM False defaultTimeLocale "%Y-%m-%d" s of
                    Just d -> opts { optEndDate = Just d }
                    Nothing -> error "Invalid end date format. Use YYYY-MM-DD"
                ) "END_DATE") 
                "End date (YYYY-MM-DD)"
            ]
    
    let (actions, nonOptions, errors) = getOpt RequireOrder options args
    
    when (not $ null errors) $ do
        progName <- getProgName
        ioError . userError $ concat errors ++ usageInfo ("Usage: " ++ progName ++ " [OPTION...]") options
    forM_ statsList $ \stat ->
        putStrLn $ printf "%-15s %10d %10.1f %10.2f"
            (unpack $ emotionName stat)
            (totalCount stat)
            (totalWeight stat)
            (avgWeight stat)
    
    putStrLn ""

-- Helper function to convert Vector to list
toList :: Vector a -> [a]
toList = foldr (:) []

-- Helper function to convert String to ByteString
pack :: String -> String
pack = id

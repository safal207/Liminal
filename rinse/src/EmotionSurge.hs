{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
module EmotionSurge where

import InsightStats (readLogFile)
import Types (RINSEOutput(..), EmotionTag(..))
import Data.Text (Text, unpack, pack)
import Data.Map (Map)
import qualified Data.Map as M
import Data.List (sortOn, groupBy, sortBy, nub, maximumBy, foldl')
import Data.Ord (comparing)
import Data.Time (UTCTime, Day, utctDay, diffDays)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Maybe (mapMaybe, catMaybes)
import Data.Function (on)
import Data.Time.LocalTime (utcToLocalTime, getCurrentTimeZone, localDay)
import Control.Monad (forM_, when)
import Data.Vector (toList)
import Data.Aeson (ToJSON, FromJSON, encode)
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as BL
import Data.Char (toLower)
import Text.Read (readMaybe)
import Data.List (intercalate)

-- | Surge statistics
data SurgeStats = SurgeStats
    { emotionName :: Text
    , totalSurges :: Int
    , maxStreak :: Int
    , avgIntensity :: Double
    } deriving (Show, Generic)

instance ToJSON SurgeStats
instance FromJSON SurgeStats

-- | Threshold for detecting emotion surges
type EmotionThreshold = (Text, Double, Int)  -- (emotion, minWeight, consecutiveDays)

-- | Parse threshold from string (format: "emotion:minWeight:days")
parseThreshold :: String -> Maybe EmotionThreshold
parseThreshold s = 
    case words (map (\c -> if c == ':' then ' ' else c) s) of
        [emo, wt, days] -> 
            case (readMaybe wt, readMaybe days) of
                (Just w, Just d) -> Just (pack emo, w, d)
                _ -> Nothing
        _ -> Nothing

-- | Detect emotion surges and return statistics
detectEmotionSurge :: FilePath -> [EmotionThreshold] -> IO (Map Text [SurgeStats])
detectEmotionSurge filePath thresholds = do
    logs <- readLogFile filePath
    let dailyEmotions = groupByDay (map fst logs)
    
    -- Process each threshold and collect results
    surgeResults <- forM thresholds $\(emotion, minWeight, days) -> do
        case M.lookup emotion dailyEmotions of
            Nothing -> return (emotion, [])
            Just daysData -> do
                let sortedDays = sortBy (comparing fst) $ M.toList daysData
                    allSurges = filter (not . null) $ consecutiveDates sortedDays
                    relevantSurges = filter (\g -> length g >= days) allSurges
                    
                    -- Calculate statistics
                    total = length relevantSurges
                    maxStrk = if null relevantSurges then 0 else maximum (map length relevantSurges)
                    avgInt = if null relevantSurges then 0
                             else sum (map (\g -> sum (map snd g) / fromIntegral (length g)) relevantSurges) 
                                  / fromIntegral total
                    
                    -- Format surge info
                    formatted = map (\surge ->
                        let dayStrs = map (formatDay . fst) $ take (length surge) surge
                        in (dayStrs, sum (map snd surge) / fromIntegral (length surge))
                        ) relevantSurges
                    
                    -- Create stats
                    stats = if total > 0 
                           then [SurgeStats emotion total maxStrk avgInt]
                           else []
                    
                -- Print immediate feedback
                forM_ formatted $\(days, avgWt) ->
                    putStrLn $"⚠️  Emotion surge detected: " ++ unpack emotion 
                            ++ " on days " ++ show days
                            ++ " (avg weight: " ++ printf "%.2f" avgWt ++ ")"
                
                return (emotion, stats)
    
    -- Return aggregated statistics
    return $ M.fromList surgeResults
  where
    formatDay = formatTime defaultTimeLocale "%Y-%m-%d"

-- | Group emotions by day and calculate average weight per emotion per day
groupByDay :: [RINSEOutput] -> Map Text (Map Day Double)
groupByDay logs =
    let -- Extract all emotion-day-weight tuples with proper timezone handling
        allEmotions = concatMap (\logEntry ->
            let emoTags = toList (tags logEntry)
            in map (\tag -> (emotion tag, utctDay (timestamp logEntry), fromIntegral (weight tag))) emoTags
            ) logs
        
        -- Group by emotion and then by day
        grouped = foldl' (\m (emo, day, wt) ->
                    M.alter (Just . maybe (M.singleton day (wt, 1)) 
                                         (M.alter (Just . maybe (wt, 1) 
                                                      (\(sumWt, count) -> (sumWt + wt, count + 1))) day))
                           emo m)
                  M.empty allEmotions
        
        -- Calculate average weight per day per emotion
        averaged = M.map (M.map (\(sumWt, count) -> sumWt / fromIntegral count)) grouped
    in averaged

-- | Format statistics as a markdown table
formatStatsTable :: [SurgeStats] -> String
formatStatsTable stats =
    let header = "Emotion      | Total Surges | Max Streak | Avg Intensity\n"
              ++ "-----------------------------------------------"
        formatRow s = printf "%-12s | %12d | %10d | %12.2f" 
                        (unpack $ emotionName s) 
                        (totalSurges s)
                        (maxStreak s)
                        (avgIntensity s)
        rows = map formatRow stats
    in unlines $ header : rows

-- | Export statistics to JSON file
exportStats :: FilePath -> [SurgeStats] -> IO ()
exportStats filePath stats = do
    BL.writeFile filePath (encode stats)
    putStrLn $"\n📊 Exported surge statistics to " ++ filePath

-- | Group consecutive days with weights above threshold
consecutiveDates :: [(Day, Double)] -> [[(Day, Double)]]
consecutiveDates [] = []
consecutiveDates (x:xs) = 
    let (group, rest) = span (\(d2,_) -> diffDays (fst x) d2 <= 1) (x:xs)
    in reverse (map reverse group) : consecutiveDates rest
  where
    diffDays d1 d2 = fromIntegral $ diffDays d1 d2

-- | Helper function to get the day from UTCTime
getDay :: UTCTime -> IO Day
getDay utcTime = do
    tz <- getCurrentTimeZone
    return $ localDay (utcToLocalTime tz utcTime)

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
module AlertEngine where

import Types (RINSEOutput(..), EmotionTag(..))
import Data.Text (Text, unpack, pack, toLower, isInfixOf)
import Data.Map (Map)
import qualified Data.Map as M
import Data.List (sortOn, groupBy, sortBy, nub, find)
import Data.Ord (comparing, Down(..))
import Data.Time (UTCTime, Day, diffDays)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Maybe (mapMaybe, catMaybes, fromMaybe)
import Data.Aeson (ToJSON, FromJSON, encode, object, (.=))
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as BL
import Text.Printf (printf)
import Data.Char (toLower)

-- | Persona context for alerts
data Persona = Persona 
    { personaName :: Text
    , personaKeywords :: [Text]
    , personaDescription :: Text
    } deriving (Show, Eq, Generic)

-- | Alert level based on surge intensity
data AlertLevel = Info | Notice | Warning | Critical
    deriving (Show, Eq, Ord, Enum, Bounded, Generic)

-- | Alert type with contextual information
data Alert = Alert
    { alertId :: Text
    , alertLevel :: AlertLevel
    , alertEmotion :: Text
    , alertPersona :: Maybe Persona
    , alertTitle :: Text
    , alertDescription :: Text
    , alertRecommendations :: [Text]
    , alertStartDate :: Day
    , alertEndDate :: Day
    , alertIntensity :: Double
    } deriving (Show, Generic)

-- | Alert configuration
data AlertConfig = AlertConfig
    { configPersonas :: [Persona]
    , configThresholds :: Map AlertLevel (Double, Double) -- (minWeight, minDays)
    , configRecommendations :: Map Text [Text] -- emotion -> recommendations
    } deriving (Show, Generic)

-- Default personas
defaultPersonas :: [Persona]
defaultPersonas =
    [ Persona "work" ["работа", "коллеги", "начальник"] "Work-related context"
    , Persona "family" ["семья", "дети", "родители"] "Family relationships"
    , Persona "inner_child" ["детств", "внутренний ребенок"] "Inner child work"
    , Persona "health" ["здоровье", "болит", "врач"] "Health concerns"
    ]

-- Default recommendations for common emotions
defaultRecommendations :: Map Text [Text]
defaultRecommendations = M.fromList
    [ ("страх", 
        [ "Попробуйте технику заземления 5-4-3-2-1 (5 вещей, которые вы видите, 4 слышите, и т.д.)"
        , "Запишите, что именно вызывает страх и возможные варианты развития событий"
        ])
    , ("тревога",
        [ "Практикуйте диафрагмальное дыхание в течение 5 минут"
        , "Составьте список конкретных шагов для управления ситуацией"
        ])
    , ("гнев",
        [ "Используйте технику тайм-аута - отойдите от ситуации на 10 минут"
        , "Выразите чувства на бумаге, затем порвите или сожгите лист"
        ])
    , ("радость",
        [ "Поделитесь своей радостью с близкими"
        , "Запишите моменты, которые вызвали это состояние"
        ])
    ]

-- | Default alert configuration
defaultAlertConfig :: AlertConfig
DefaultAlertConfig =
    let thresholds = M.fromList
            [ (Critical, (4.0, 3))  -- 3+ days with weight ≥ 4.0
            , (Warning, (3.5, 3))   -- 3+ days with weight ≥ 3.5
            , (Notice, (3.0, 3))    -- 3+ days with weight ≥ 3.0
            , (Info, (2.5, 3))      -- 3+ days with weight ≥ 2.5
            ]
    in AlertConfig defaultPersonas thresholds defaultRecommendations

-- | Match text to personas
matchPersonas :: Text -> [Persona] -> [Persona]
matchPersonas text personas =
    filter (\p -> any (`isInfixOf` toLower text) (map toLower <$> personaKeywords p)) personas

-- | Generate alerts from emotion surges
generateAlerts :: AlertConfig -> [(Text, [(Day, Double)])] -> [Alert]
generateAlerts config emotionDays =
    concatMap (uncurry $ checkEmotionSurge config) emotionDays

-- | Check for surges in a specific emotion
checkEmotionSurge :: AlertConfig -> Text -> [(Day, Double)] -> [Alert]
checkEmotionSurge AlertConfig{..} emotion daysData =
    let sortedDays = sortBy (comparing fst) daysData
        surges = consecutiveDates sortedDays
        
        -- Check each surge against alert thresholds
        checkSurge surge =
            let surgeLength = length surge
                avgWeight = sum (map snd surge) / fromIntegral surgeLength
                startDay = fst $ head surge
                endDay = fst $ last surge
                
                -- Find matching alert level
                matchingLevel = find (\(_, (minW, minD)) -> 
                    avgWeight >= minW && surgeLength >= minD) 
                    (M.toList configThresholds)
                
                -- Get recommendations
                recs = M.findWithDefault 
                    ["Рекомендуется обсудить это с психологом или доверенным лицом."] 
                    (toLower emotion) configRecommendations
                
            in case matchingLevel of
                Just (level, _) -> 
                    let alertId = mconcat 
                            [ toLower emotion, "-", 
                              formatDay startDay, "-", 
                              formatDay endDay ]
                        title = case level of
                            Critical -> "Критический уровень " <> emotion
                            Warning -> "Высокий уровень " <> emotion
                            Notice -> "Повышенный уровень " <> emotion
                            Info -> "Замечено " <> emotion
                        
                        desc = case level of
                            Critical -> "Очень высокий уровень " <> emotion <> " в течение " 
                                     <> show surgeLength <> " дней"
                            _ -> "Замечен устойчивый уровень " <> emotion 
                                <> " в течение " <> show surgeLength <> " дней"
                        
                    in Just $ Alert
                        { alertId = alertId
                        , alertLevel = level
                        , alertEmotion = emotion
                        , alertPersona = Nothing  -- Will be set later
                        , alertTitle = title
                        , alertDescription = desc
                        , alertRecommendations = take 2 recs  -- Limit to top 2 recommendations
                        , alertStartDate = startDay
                        , alertEndDate = endDay
                        , alertIntensity = avgWeight
                        }
                Nothing -> Nothing
                
    in catMaybes $ map checkSurge surges

-- | Add persona context to alerts
addPersonaContext :: [RINSEOutput] -> [Alert] -> [Alert]
addPersonaContext logs alerts =
    let -- Create a map of day to log entry
        dayToLogs = M.fromListWith (++) $ 
            map (\log -> (utctDay (timestamp log), [log])) logs
        
        -- Add persona to each alert
        addPersona alert@Alert{..} =
            let relevantLogs = concat $ 
                    mapMaybe (\day -> M.lookup day dayToLogs) 
                    [alertStartDate .. alertEndDate]
                
                -- Find matching personas in log texts
                matchingPersonas = nub $ 
                    concatMap (\log -> matchPersonas (cleansed log) (configPersonas defaultAlertConfig)) 
                    relevantLogs
                
                -- If we found exactly one persona, use it
                persona = case matchingPersonas of
                    [p] -> Just p
                    _ -> Nothing
                
            in alert { alertPersona = persona }
            
    in map addPersona alerts

-- | Format alerts as human-readable text
formatAlert :: Alert -> Text
formatAlert Alert{..} =
    let levelStr = case alertLevel of
            Critical -> "🔴 КРИТИЧЕСКИЙ"
            Warning -> "🟠 ВНИМАНИЕ"
            Notice -> "🟡 ЗАМЕТКА"
            Info -> "🔵 ИНФО"
        
        personaStr = case alertPersona of
            Just p -> "\n👤 Контекст: " <> personaName p
            Nothing -> ""
        
        dateRange = formatDay alertStartDate 
                 <> (if alertStartDate /= alertEndDate 
                     then " - " <> formatDay alertEndDate 
                     else "")
        
        recs = if null alertRecommendations 
               then "" 
               else "\n\nРекомендации:\n" 
                   <> pack (intercalate "\n" (map ("• " <>) alertRecommendations))
        
    in mconcat
        [ levelStr, 
          " ", alertTitle, 
          " (", pack (printf "%.1f" alertIntensity), ")",
          "\n📅 ", dateRange,
          personaStr,
          "\n", alertDescription,
          recs,
          "\n"
        ]

-- | Process logs and generate alerts
processAlerts :: AlertConfig -> FilePath -> IO [Alert]
processAlerts config filePath = do
    logs <- fst <$> readLogFile filePath
    let dailyEmotions = groupByDay logs
        emotionDays = M.toList dailyEmotions
        alerts = generateAlerts config emotionDays
        alertsWithContext = addPersonaContext logs alerts
    return $ sortOn (Down . alertLevel) alertsWithContext

-- Helper functions from EmotionSurge
formatDay :: Day -> Text
formatDay = pack . formatTime defaultTimeLocale "%Y-%m-%d"

-- Re-export needed functions from EmotionSurge
readLogFile :: FilePath -> IO [RINSEOutput]
readLogFile = EmotionSurge.readLogFile

groupByDay :: [RINSEOutput] -> Map Text [(Day, Double)]
groupByDay = EmotionSurge.groupByDay

consecutiveDates :: [(Day, Double)] -> [[(Day, Double)]]
consecutiveDates = EmotionSurge.consecutiveDates

-- JSON instances
instance ToJSON Persona
instance FromJSON Persona
instance ToJSON AlertLevel
instance FromJSON AlertLevel
instance ToJSON Alert
instance FromJSON Alert
instance ToJSON AlertConfig
instance FromJSON AlertConfig

{-# LANGUAGE RecordWildCards #-}
module CycleOfBecoming where

import DuneField
import Data.Maybe (listToMaybe)
import Data.List (find, sortOn)
import Data.Ord (Down(..))
import Data.Text (Text, pack, unpack, intercalate)
import Data.Time.Clock (diffUTCTime)

-- | Represents a past emotional wave with context
data PastWave = PastWave
    { pwEmotion :: Emotion
    , pwGrowth :: Float
    , pwTags :: [Text]
    , pwTimestamp :: UTCTime
    , pwContext :: Text  -- Additional context from that moment
    } deriving (Show)

-- | Represents the growth between two emotional states
data GrowthDelta = GrowthDelta
    { gdEmotionChange :: (Emotion, Emotion)
    , gdGrowthDelta :: Float
    , gdNewTags :: [Text]
    , gdTimePassed :: NominalDiffTime  -- Time between events
    } deriving (Show)

-- | Find the most recent occurrence of this emotion in agent's history
detectEmotionCycle :: Agent -> Emotion -> IO (Maybe PastWave)
detectEmotionCycle agent currentEmotion = do
    currentTime <- getCurrentTime
    let relevantWaves = filter (\w -> dwEmotion w == currentEmotion) (agentPreviousWaves agent)
    case sortOn (Down . dwTimestamp) relevantWaves of
        [] -> return Nothing
        (w:_) -> return $ Just $ waveToPastWave w currentTime

-- | Convert a DuneWave to a PastWave, calculating time delta
waveToPastWave :: DuneWave -> UTCTime -> PastWave
waveToPastWave DuneWave{..} currentTime = PastWave
    { pwEmotion = dwEmotion
    , pwGrowth = dwGrowth
    , pwTags = dwTags
    , pwTimestamp = dwTimestamp
    , pwContext = createContext dwTags
    }
  where
    createContext tags
        | "страх" `elem` tags = "В моменте страха и неопределённости"
        | "радость" `elem` tags = "В моменте счастья и лёгкости"
        | "гнев" `elem` tags = "В моменте ярости и напряжения"
        | otherwise = "В том далёком моменте"

-- | Compare past and current emotional states
compareGrowth :: PastWave -> DuneWave -> IO GrowthDelta
compareGrowth past current = do
    currentTime <- getCurrentTime
    return $ GrowthDelta
        { gdEmotionChange = (pwEmotion past, dwEmotion current)
        , gdGrowthDelta = dwGrowth current - pwGrowth past
        , gdNewTags = filter (`notElem` pwTags past) (dwTags current)
        , gdTimePassed = diffUTCTime currentTime (pwTimestamp past)
        }

-- | Generate poetic insight about the emotional cycle
cycleOfBecoming :: Agent -> DuneWave -> IO Text
cycleOfBecoming agent currentWave = do
    let currentEmotion = dwEmotion currentWave
    mPastWave <- detectEmotionCycle agent currentEmotion
    
    case mPastWave of
        Nothing -> return "Всё впервые, всё в новинку..."
        Just pastWave -> do
            delta <- compareGrowth pastWave currentWave
            return $ generateInsight pastWave currentWave delta

-- | Generate poetic insight about the growth
generateInsight :: PastWave -> DuneWave -> GrowthDelta -> Text
generateInsight past current delta@GrowthDelta{..} =
    let emotionFrom = show (fst gdEmotionChange)
        emotionTo = show (snd gdEmotionChange)
        days = truncate (gdTimePassed / 86400) :: Int
        
        growthPhrase
            | gdGrowthDelta > 0.3 = "значительно вырос"
            | gdGrowthDelta > 0.1 = "подрос"
            | gdGrowthDelta > -0.1 = "остался прежним, но другим"
            | otherwise = "вернулся к истокам"
            
        timePhrase
            | days == 0 = "Сегодня"
            | days == 1 = "Вчера"
            | days < 7 = "На этой неделе"
            | days < 30 = "В этом месяце"
            | otherwise = "Когда-то давно"
            
        newWisdom = if null (gdNewTags delta)
            then ""
            else "\nНовое в тебе: " <> intercalate ", " (gdNewTags delta)
            
    in pack $ unlines
        [ "🌀 " ++ timePhrase ++ " ты был в " ++ emotionFrom ++ "."
        , "   " ++ pwContext past
        , "→ А сейчас ты в " ++ emotionTo ++ "."
        , "   За это время ты " ++ growthPhrase ++ "."
        ] `mappend` unpack newWisdom

-- Update agent's wave history (call this when adding new waves)
updateAgentHistory :: Agent -> DuneWave -> IO Agent
updateAgentHistory agent wave = do
    let updatedWaves = wave : agentPreviousWaves agent
        -- Keep only last 100 waves for performance
        trimmedWaves = take 100 updatedWaves
    return agent { agentPreviousWaves = trimmedWaves }
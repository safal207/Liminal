{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module MusicOfTransition where

import Soundtrack (Soundtrack, generateSoundtrack, describeSoundtrack, soundtrackToMIDI)

import Data.Text (Text, pack, unpack, intercalate)
import qualified Data.Text as T
import Data.Time (UTCTime, Day, diffDays, utctDay)
import Data.List (sortOn, groupBy, nub, (\))
import Data.Ord (Down(..))
import Data.Maybe (mapMaybe, catMaybes, listToMaybe)
import Data.Aeson (ToJSON, FromJSON, object, (.=), (.:), (.:?))
import GHC.Generics (Generic)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Ratio ((%))
import Data.List.NonEmpty (NonEmpty(..), nonEmpty, toList)
import qualified Data.List.NonEmpty as NE
import Data.Function (on)
import Data.Time.Format (defaultTimeLocale, formatTime)

-- | Emotional state at a point in time
data EmotionalState = EmotionalState
    { esTimestamp :: UTCTime
    , esDominantEmotion :: Text
    , esEmotions :: Map Text Double  -- Emotion -> Score
    , esText :: Text
    , esLinguisticCues :: [Text]     -- Significant linguistic markers
    } deriving (Show, Eq, Generic)

-- | A transition between emotional states
data EmotionalTransition = EmotionalTransition
    { etId :: Text
    , etStartTime :: UTCTime
    , etEndTime :: UTCTime
    , etStates :: NonEmpty EmotionalState
    , etTransitionType :: TransitionType
    , etKeyPhrases :: [Text]
    } deriving (Show, Eq, Generic)

-- | Types of emotional transitions
data TransitionType
    = UpwardSpiral      -- Negative to positive growth
    | DownwardSpiral    -- Deterioration
    | Breakthrough      -- Sudden positive shift
    | Breakdown        -- Sudden negative shift
    | Integration      -- Resolution and acceptance
    | Oscillation      -- Back and forth without resolution
    | Grounding        -- Return to baseline
    deriving (Show, Eq, Ord, Enum, Bounded, Generic)

-- | Transition pattern with metadata
data TransitionPattern = TransitionPattern
    { tpName :: Text
    , tpDescription :: Text
    , tpEmotionSequence :: [Text]  -- Expected emotion sequence
    , tpMinLength :: Int
    , tpMaxLength :: Int
    , tpReflection :: Text
    } deriving (Show, Eq, Generic)

-- | Configuration for transition detection
data TransitionConfig = TransitionConfig
    { tcMinStates :: Int
    , tcMaxStates :: Int
    , tcMaxDaysBetween :: Int
    , tcMinEmotionScore :: Double
    } deriving (Show, Generic)

-- Default configuration
defaultConfig :: TransitionConfig
defaultConfig = TransitionConfig
    { tcMinStates = 2
    , tcMaxStates = 5
    , tcMaxDaysBetween = 7
    , tcMinEmotionScore = 0.3
    }

-- Common transition patterns
transitionPatterns :: [TransitionPattern]
transitionPatterns =
    [ TransitionPattern
        { tpName = "Восходящая спираль"
        , tpDescription = "Постепенный рост от страха к решимости"
        , tpEmotionSequence = ["страх", "сомнение", "принятие", "решимость"]
        , tpMinLength = 2
        , tpMaxLength = 5
        , tpReflection = "Ты находишь опору в хаосе. Шаг за шагом."
        }
    , TransitionPattern
        { tpName = "Прорыв"
        , tpDescription = "Внезапное озарение и сдвиг"
        , tpEmotionSequence = ["растерянность", "озарение", "ясность"]
        , tpMinLength = 2
        , tpMaxLength = 3
        , tpReflection = "Иногда свет приходит неожиданно. Заметь это."
        }
    , TransitionPattern
        { tpName = "Возвращение к себе"
        , tpDescription = "От потери к обретению почвы под ногами"
        , tpEmotionSequence = ["потеря", "поиск", "опора"]
        , tpMinLength = 2
        , tpMaxLength = 4
        , tpReflection = "Ты возвращаешься к себе — снова и снова."
        }
    ]

-- | Detect transitions in a sequence of emotional states
detectTransitions :: [EmotionalState] -> [EmotionalTransition]
detectTransitions states =
    let -- Sort by timestamp
        sorted = sortOn esTimestamp states
        
        -- Generate all possible sequences within time window
        sequences = [ (s, takeWithinWindow s) | s <- inits sorted ]
        
        -- Check each sequence against known patterns
        transitions = concatMap (uncurry checkSequence) sequences
        
    in transitions
  where
    -- Take states within the time window
    takeWithinWindow states =
        case nonEmpty states of
            Nothing -> []
            Just ne ->
                let firstTime = esTimestamp (NE.head ne)
                    isWithinWindow s = diffDays (esTimestamp s) firstTime <= tcMaxDaysBetween defaultConfig
                in take (tcMaxStates defaultConfig) $ filter isWithinWindow states
    
    -- Check a sequence against all patterns
    checkSequence _ [] = []
    checkSequence prefix (s:ss) =
        let sequence = prefix ++ [s]
            matches = mapMaybe (matchPattern sequence) transitionPatterns
        in matches ++ checkSequence sequence ss
    
    -- Match a sequence against a specific pattern
    matchPattern states pattern =
        let emotions = map (T.toLower . esDominantEmotion) states
            matchesPattern = isSubsequence (tpEmotionSequence pattern) emotions
            isValidLength = let l = length states in l >= tpMinLength pattern && l <= tpMaxLength pattern
        in if matchesPattern && isValidLength
            then Just $ createTransition states pattern
            else Nothing
    
    -- Create a transition from matched states and pattern
    createTransition states pattern =
        let start = head states
            end = last states
            transitionId = T.pack $ formatTime defaultTimeLocale "%Y%m%d-%H%M%S" (esTimestamp start)
                        <> "-" <> T.unpack (tpName pattern)
            
            -- Extract key phrases (simplified)
            keyPhrases = take 3 $ map (T.take 100 . esText) states
            
        in EmotionalTransition
            { etId = transitionId
            , etStartTime = esTimestamp start
            , etEndTime = esTimestamp end
            , etStates = NE.fromList states
            , etTransitionType = patternToType pattern
            , etKeyPhrases = keyPhrases
            }
    
    -- Map pattern to transition type
    patternToType pattern = case tpName pattern of
        "Восходящая спираль" -> UpwardSpiral
        "Прорыв" -> Breakthrough
        "Возвращение к себе" -> Grounding
        _ -> Integration

-- | Format a transition as a human-readable report
formatTransitionReport :: EmotionalTransition -> Text
formatTransitionReport et@EmotionalTransition{..} =
    let -- Format date range
        dateRange = T.pack $ formatTime defaultTimeLocale "%b %e" etStartTime
                 <> " → "
                 <> formatTime defaultTimeLocale "%b %e" etEndTime
        
        -- Get emotion sequence with weights
        emotionSequence = map (\s -> (esDominantEmotion s, 0.5)) $ toList etStates
        
        -- Generate soundtrack
        soundtrack = generateSoundtrack emotionSequence
        
        -- Get emotion sequence for display
        emotions = T.intercalate " → " $ map (T.toUpper . fst) emotionSequence
        
        -- Get transition type info
        (typeEmoji, typeName) = case etTransitionType of
            UpwardSpiral -> ("🔼", "Восходящая спираль")
            DownwardSpiral -> ("🔽", "Нисходящая спираль")
            Breakthrough -> ("✨", "Прорыв")
            Breakdown -> ("💥", "Напряжение")
            Integration -> ("🔄", "Интеграция")
            Oscillation -> ("↔️", "Колебания")
            Grounding -> ("🌱", "Заземление")
        
        -- Format key phrases
        phrases = T.intercalate "\n" $ map ("- " <>) etKeyPhrases
        
        -- Get reflection based on transition type
        reflection = case etTransitionType of
            UpwardSpiral -> "Ты находишь опору в хаосе. Шаг за шагом."
            Breakthrough -> "Озарение приходит, когда готов его принять."
            _ -> "Заметь этот момент перехода. Он важен."
        
    in T.unlines
        [ ""
        , typeEmoji <> " *" <> typeName <> "*"
        , ""
        , "🕒 " <> dateRange
        , "🧠 Эмоции: " <> emotions
        , ""
        , "🎵 " <> T.lines (describeSoundtrack soundtrack) !! 0
        , "🎼 " <> T.lines (describeSoundtrack soundtrack) !! 1
        , ""
        , "💬 Ключевые фразы:"
        , phrases
        , ""
        , "📖 Отражение:"
        , reflection
        , ""
        ]

-- | Export transition as MIDI file
exportTransitionAsMIDI :: EmotionalTransition -> FilePath -> IO ()
exportTransitionAsMIDI et@EmotionalTransition{..} path = do
    let emotionSequence = map (\s -> (esDominantEmotion s, 0.5)) $ toList etStates
        soundtrack = generateSoundtrack emotionSequence
    writeFile path (T.unpack $ soundtrackToMIDI soundtrack)

-- | Helper: Check if a list is a subsequence of another
isSubsequence :: Eq a => [a] -> [a] -> Bool
isSubsequence [] _ = True
isSubsequence _ [] = False
isSubsequence (x:xs) (y:ys)
    | x == y = isSubsequence xs ys
    | otherwise = isSubsequence (x:xs) ys

-- JSON instances for serialization
instance ToJSON EmotionalState
instance FromJSON EmotionalState
instance ToJSON EmotionalTransition
instance FromJSON EmotionalTransition
instance ToJSON TransitionType
instance FromJSON TransitionType
instance ToJSON TransitionPattern
instance FromJSON TransitionPattern

-- | Process logs and generate transition report
analyzeEmotionalJourney :: FilePath -> IO Text
analyzeEmotionalJourney logFile = do
    -- In a real implementation, this would read and parse the log file
    -- For now, we'll return a placeholder
    return $ T.unlines
        [ "# Анализ эмоционального пути"
        , ""
        , "Этот отчёт показывает ключевые переходы в твоём эмоциональном состоянии с течением времени."
        , ""
        , "*Загрузите журналы для просмотра переходов*"
        ]

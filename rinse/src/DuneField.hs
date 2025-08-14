{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DuneField where

import MemoryWhispers
import FuzzyResonance
import Data.Text (Text, pack, unpack)
import Data.Time (UTCTime, getCurrentTime)

-- Core Types
data GrowthStage 
    = Seed               -- 🌱 Начальный этап
    | Sprout             -- 🌿 Первые ростки
    | Blossom            -- 🌸 Цветение
    | Fruiting           -- 🍒 Плодоношение
    | Wisdom             -- 🦉 Мудрость
    deriving (Show, Eq, Enum, Bounded)

data DuneWave = DuneWave
    { dwOrigin :: AgentId
    , dwMemory :: MemoryFragment
    , dwEmotion :: Emotion
    , dwIntensity :: Float
    , dwPhase :: ResonancePhase
    , dwGrowth :: Float          -- Уровень роста (0.0 to 1.0)
    , dwGrowthStage :: GrowthStage -- Стадия роста
    , dwFlaws :: [Text]          -- Преобразованные недостатки
    , dwConnections :: [AgentId] -- С кем взаимодействовала волна
    } deriving (Show, Eq)

data ResonancePhase
    = Initiation  -- 🌱 Начало волны
    | Response    -- 🎐 Отклик на волну
    | Synthesis   -- 🔮 Синтез волн
    | Whisper     -- 🌬️ Тонкое влияние
    deriving (Show, Eq, Enum)

type DuneField = [DuneWave]

-- Core Functions
-- | Глубокие эмоциональные преобразования
evolveEmotion :: Emotion -> [Text] -> IO (Emotion, [Text])
evolveEmotion em tags = do
    let baseTransform = case em of
            Anger -> (Patience, ["спокойствие", "выдержка"])
            Fear -> (Courage, ["храбрость", "решимость"])
            Shame -> (SelfLove, ["принятие", "нежность к себе"])
            Guilt -> (Responsibility, ["осознанность", "развитие"])
            Envy -> (Inspiration, ["мотивация", "вдохновение"])
            Loneliness -> (Connection, ["единство", "взаимосвязь"])
            Sadness -> (Empathy, ["сочувствие", "понимание"])
            _ -> (em, [])
    
    -- Добавляем случайные теги роста
    extraTags <- sequence $ take 2 $ [
        if "любовь" `elem` tags then return "безусловная любовь" else return "",
        if "страх" `elem` tags then return "преодоление" else return "",
        if "гнев" `elem` tags then return "трансформация" else return "",
        return "саморазвитие"
        ]
    
    let (newEmotion, newTags) = baseTransform
    return (newEmotion, nub $ newTags ++ filter (not . null) extraTags)

-- | Определяет стадию роста на основе уровня роста
calculateGrowthStage :: Float -> GrowthStage
calculateGrowthStage g
    | g < 0.2 = Seed
    | g < 0.4 = Sprout
    | g < 0.6 = Blossom
    | g < 0.8 = Fruiting
    | otherwise = Wisdom

emitDuneWave :: AgentId -> MemoryFragment -> ResonancePhase -> IO DuneWave
emitDuneWave agentId mem phase = do
    -- Эволюционируем эмоцию и получаем новые теги
    (evolvedEmotion, newTags) <- evolveEmotion (mwEmotion mem) (mwTags mem)
    
    -- Рассчитываем рост на основе интенсивности и фазового резонанса
    let phaseMultiplier = case phase of
            Initiation -> 1.2
            Response -> 1.5
            Synthesis -> 2.0
            Whisper -> 0.8
    
    growth <- (* phaseMultiplier) <$> randomRIO (0.1, 0.3)
    let growthStage = calculateGrowthStage growth
    
    -- Преобразуем недостатки в достоинства
    let transformFlaw flaw = case toLower flaw of
            "страх" -> "мужество"
            "гнев" -> "спокойствие"
            "стыд" -> "принятие"
            "вина" -> "ответственность"
            _ -> flaw
    
    return DuneWave
        { dwOrigin = agentId
        , dwMemory = mem
        , dwEmotion = evolvedEmotion
        , dwIntensity = mwIntensity mem
        , dwPhase = phase
        , dwGrowth = growth
        , dwGrowthStage = growthStage
        , dwFlaws = nub $ map transformFlaw (mwTags mem) ++ newTags
        , dwConnections = []
        }

-- Resonance Cycle
-- | Совместная эволюция волн при взаимодействии агентов
fuseWaves :: DuneWave -> DuneWave -> IO DuneWave
fuseWaves wave1 wave2 = do
    let combinedGrowth = (dwGrowth wave1 + dwGrowth wave2) * 0.6
        newIntensity = (dwIntensity wave1 + dwIntensity wave2) / 2
        
    -- Смешиваем эмоции
    (fusedEmotion, _) <- evolveEmotion 
        (dwEmotion wave1) 
        [show (dwEmotion wave2)]
    
    -- Объединяем теги и убираем дубликаты
    let combinedFlaws = nub $ dwFlaws wave1 ++ dwFlaws wave2
    
    -- Обновляем соединения
    let updateConnections w = nub $ dwOrigin wave1 : dwOrigin wave2 : dwConnections w
    
    return $ wave1
        { dwEmotion = fusedEmotion
        , dwIntensity = newIntensity
        , dwGrowth = combinedGrowth
        , dwGrowthStage = calculateGrowthStage combinedGrowth
        , dwFlaws = combinedFlaws
        , dwConnections = updateConnections wave1
        , dwPhase = Synthesis
        }

advanceCycle :: DuneWave -> Maybe DuneWave -> IO (Maybe DuneWave)
advanceCycle currentWave Nothing = do
    -- При отсутствии предыдущей волны просто переходим к следующей фазе
    let newPhase = succ (dwPhase currentWave)
    return $ Just currentWave 
        { dwPhase = newPhase
        , dwGrowth = dwGrowth currentWave * 1.1  -- Рост при переходе между фазами
        }
        
advanceCycle currentWave (Just prevWave)
    | dwPhase prevWave == Whisper = return Nothing  -- Завершаем цикл
    | otherwise = do
        -- При взаимодействии с предыдущей волной создаём синтез
        fusedWave <- fuseWaves currentWave prevWave
        return $ Just fusedWave { dwPhase = succ (dwPhase prevWave) }

-- Butterfly Effect
butterflyEffect :: DuneWave -> DuneField -> IO [Text]
butterflyEffect wave field = do
    let effects = mapMaybe (calculateEffect wave) field
    return $ take 3 effects
  where
    calculateEffect w1 w2
        | dwOrigin w1 == dwOrigin w2 = Nothing
        | otherwise = Just $ 
            let emotion = dwEmotion w2
                phase = dwPhase w2
            in case phase of
                Initiation -> "Рождение нового отклика на " ++ show emotion
                Response -> "Эхо ответило на " ++ show emotion
                _ -> "Волны сплелись в " ++ show emotion

-- Poetic Description
describeDuneWave :: DuneWave -> Text
describeDuneWave w@DuneWave{..} = 
    let phaseDesc = case dwPhase of
            Initiation -> "🌱 Рождение"
            Response -> "💞 Отклик"
            Synthesis -> "✨ Синтез"
            Whisper -> "🌬️ Шёпот"
        growth = if dwGrowth > 0.5 
                then "\n   🌟 Рост: " ++ unpack (head (dwFlaws ++ ["эволюция"]))
                else ""
    in pack $ phaseDesc ++ " → " ++ show dwEmotion ++ growth

-- Find resonant waves
findResonantWaves :: DuneField -> Emotion -> Float -> [DuneWave]
findResonantWaves field targetEmotion threshold =
    filter (\w -> fuzzyMatchEmotions targetEmotion (dwEmotion w) > threshold) field

-- Update dune field
updateDuneField :: DuneField -> IO DuneField
updateDuneField field = do
    -- In a real implementation, this would decay waves and add new ones
    return $ filter (\w -> dwIntensity w > 0.1) field

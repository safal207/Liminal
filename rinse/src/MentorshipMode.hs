{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module MentorshipMode where

import DuneField
import Data.List (find, sortOn)
import Data.Ord (Down(..))
import Data.Text (Text, pack, unpack, intercalate)
import Data.Maybe (listToMaybe)
import System.Random (randomRIO)
import Control.Monad (when)
import Data.Time.Clock (getCurrentTime)

-- | A piece of wisdom shared from mentor to mentee
data WisdomShard = WisdomShard
    { wsTitle :: Text
    , wsNarrative :: Text
    , wsEmotion :: Emotion
    , wsTags :: [Text]
    , wsGrowthImpact :: Float
    }

-- | Check if an agent qualifies as a mentor
isWiseMentor :: Agent -> Bool
isWiseMentor agent = 
    agentGrowthStage agent == Wisdom && 
    length (agentExperience agent) >= 10  -- Minimum 10 experiences

-- | Find a relevant mentor for a specific wave
findRelevantMentor :: [Agent] -> DuneWave -> IO (Maybe Agent)
findRelevantMentor agents wave = do
    let potentialMentors = filter isWiseMentor agents
        currentEmotion = dwEmotion wave
        
    -- Find mentors who have experience with this emotion
    let experiencedMentors = filter (hasExperienceWith currentEmotion) potentialMentors
    
    -- Sort by experience level (descending)
    let sortedMentors = sortOn (Down . length . agentExperience) experiencedMentors
    
    -- Return the most experienced mentor, if any
    return $ listToMaybe sortedMentors
  where
    hasExperienceWith :: Emotion -> Agent -> Bool
    hasExperienceWith em agent = any ((== em) . dwEmotion) (agentExperience agent)

-- | Share wisdom from mentor to mentee
shareWisdom :: Agent -> DuneWave -> IO (Maybe WisdomShard)
shareWisdom mentor wave = do
    let relevantExperiences = filter ((== dwEmotion wave) . dwEmotion) 
                                (take 10 $ agentExperience mentor)  -- Last 10 relevant experiences
    
    if null relevantExperiences
        then return Nothing
        else do
            -- Select a random relevant experience
            idx <- randomRIO (0, length relevantExperiences - 1)
            let experience = relevantExperiences !! idx
            return $ Just $ createWisdomShard experience

-- | Create a wisdom shard from an experience
createWisdomShard :: DuneWave -> WisdomShard
createWisdomShard wave = WisdomShard
    { wsTitle = "История преображения"
    , wsNarrative = generateNarrative wave
    , wsEmotion = dwEmotion wave
    , wsTags = dwTags wave
    , wsGrowthImpact = 0.2  -- Base growth impact
    }
  where
    generateNarrative w = case dwEmotion w of
        Fear -> "Когда-то я тоже боялся, но понял, что страх — это тень возможностей. " 
                <> "Вместо бегства я начал идти ему навстречу."
        Anger -> "Гнев показал мне границы. Я научился превращать его в решимость " 
                <> "и действовать, а не разрушать."
        Sadness -> "Печаль научила меня ценить моменты радости. " 
                  <> "Она как дождь — кажется бесконечным, но всегда дарит новую жизнь."
        _ -> "Каждое переживание — это урок. Главное — уметь его услышать."

-- | Main mentorship function
guideYoungerWave :: Agent -> DuneWave -> IO DuneWave
guideYoungerWave mentor wave = do
    mWisdom <- shareWisdom mentor wave
    
    case mWisdom of
        Nothing -> return wave  -- No wisdom to share
        Just wisdom -> do
            -- Apply wisdom to the wave
            let growthBoost = wsGrowthImpact wisdom
                newGrowth = min 1.0 (dwGrowth wave + growthBoost)
                newTags = nub $ wsTags wisdom ++ dwTags wave
                
                -- Create a resonance effect
                resonanceEffect = createResonance mentor wave
                
            -- Log the mentorship
            currentTime <- getCurrentTime
            let logEntry = MentorshipLog
                    { mlMentorId = agentId mentor
                    , mlTimestamp = currentTime
                    , mlImpact = growthBoost
                    , mlEmotion = dwEmotion wave
                    }
            
            -- Return enhanced wave
            return $ wave 
                { dwGrowth = newGrowth
                , dwTags = newTags
                , dwResonance = resonanceEffect : dwResonance wave
                }

-- | Create a resonance effect from mentorship
createResonance :: Agent -> DuneWave -> Resonance
createResonance mentor wave = Resonance
    { rType = MentorshipResonance
    , rSource = agentId mentor
    , rStrength = 0.7  -- Strong resonance from mentorship
    , rTags = ["наставничество", "мудрость"] ++ dwTags wave
    }

-- | Find and apply mentorship from the wisest available agent
seekMentorship :: [Agent] -> DuneWave -> IO DuneWave
seekMentorship agents wave = do
    mBestMentor <- findRelevantMentor agents wave
    case mBestMentor of
        Nothing -> return wave  -- No suitable mentor found
        Just mentor -> do
            putStrLn $ "🧙 Найден наставник: " ++ agentName mentor
            guideYoungerWave mentor wave

-- | Data structure for tracking mentorship
data MentorshipLog = MentorshipLog
    { mlMentorId :: AgentId
    , mlTimestamp :: UTCTime
    , mlImpact :: Float
    , mlEmotion :: Emotion
    }
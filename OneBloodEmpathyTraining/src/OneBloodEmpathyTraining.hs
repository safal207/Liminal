{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module OneBloodEmpathyTraining (
    -- * Core Types
    AgentID(..),
    EmpathicModel(..),
    TrainingExample(..),
    EmotionalArc(..),
    
    -- * Model Management
    trainModel,
    saveModel,
    loadModel,
    
    -- * Response Generation
    getEmpathicResponse,
    
    -- * Training Data Management
    loadTrainingData,
    saveTrainingData,
    
    -- * Utilities
    extractEmotionalArcs,
    generateTrainingExamples
) where

import Data.Aeson (FromJSON, ToJSON, ToJSONKey, FromJSONKey, (.:), (.:?), (.=), object, encode, decode, eitherDecode)
import qualified Data.ByteString.Lazy as BL
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text, pack, unpack)
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Data.List (nub, sortBy, groupBy)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe, catMaybes, mapMaybe)
import Data.Function (on)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>), takeDirectory)
import Control.Monad (when)
import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import Data.Coerce (coerce)
import Data.Semigroup (Semigroup(..))
import Data.Monoid (Monoid(..))
import Data.Aeson.Types (FromJSON(..), ToJSON(..), Parser, Value(..), withObject, withText, typeMismatch)

-- Re-exports from ТыИЯОднойКрови
import qualified ТыИЯОднойКрови as TYOK
import MusicOfTransition (EmotionalState(..))
import EmotionLinguisticMap (LinguisticCue(..), CueType(..))

-- | Unique identifier for an agent
newtype AgentID = AgentID { unAgentID :: Text }
    deriving (Eq, Ord, Show, Generic, NFData, Hashable, ToJSON, FromJSON)

-- | Represents an emotional arc in a conversation
data EmotionalArc = EmotionalArc
    { eaStartState :: EmotionalState
    , eaEndState :: EmotionalState
    , eaTransitions :: [EmotionalState]
    , eaEffectiveness :: Double  -- 0.0 to 1.0
    , eaMetadata :: Map Text Text
    } deriving (Show, Eq, Generic, NFData)

-- | A single training example
data TrainingExample = TrainingExample
    { teAgentId :: AgentID
    , teInput :: Text  -- User's input
    , teEmotionalState :: EmotionalState
    , teResponse :: Text  -- Agent's response
    , teEffectiveness :: Double  -- 0.0 to 1.0
    , teTimestamp :: UTCTime
    , teTags :: [Text]  -- Additional tags for categorization
    } deriving (Show, Eq, Generic, NFData)

-- | The empathic model that can be trained and used for inference
data EmpathicModel = EmpathicModel
    { emAgentId :: AgentID
    , emSharedResonanceMap :: TYOK.SharedResonanceMap
    , emArchetype :: Maybe Text  -- Optional archetype (e.g., "Overthinker", "Seeker")
    , emLastTrained :: Maybe UTCTime
    , emTrainingMetrics :: Map Text Double
    } deriving (Show, Eq, Generic, NFData)

-- | Create a new, empty empathic model
emptyEmpathicModel :: AgentID -> EmpathicModel
emptyEmpathicModel agentId = EmpathicModel
    { emAgentId = agentId
    , emSharedResonanceMap = TYOK.emptyResonanceMap (unAgentID agentId)
    , emArchetype = Nothing
    , emLastTrained = Nothing
    , emTrainingMetrics = M.empty
    }

-- | Train the model on a set of training examples
trainModel :: [TrainingExample] -> EmpathicModel -> IO EmpathicModel
trainModel examples model = do
    currentTime <- getCurrentTime
    
    -- Convert training examples to emotional transitions
    let transitions = mapMaybe exampleToTransition examples
    
    -- Update the shared resonance map
    updatedSrm <- TYOK.learnFromTransitions transitions (emSharedResonanceMap model)
    
    -- Calculate training metrics
    let metrics = calculateMetrics examples updatedSrm
    
    return $ model
        { emSharedResonanceMap = updatedSrm
        , emLastTrained = Just currentTime
        , emTrainingMetrics = metrics
        }
  where
    exampleToTransition :: TrainingExample -> Maybe TYOK.EmotionalTransition
    exampleToTransition TrainingExample{..} = do
        -- Here we'd extract the emotional states from the example
        -- This is a simplified version - you'd want to extract more nuanced states
        -- from the input and response
        let startState = teEmotionalState
        -- For now, we'll create a simple transition
        Just $ TYOK.EmotionalTransition
            { TYOK.etStates = startState :| []  -- Simplified - would need more nuanced state tracking
            , TYOK.etTimestamp = teTimestamp
            , TYOK.etEffectiveness = teEffectiveness
            }

-- | Calculate training metrics
calculateMetrics :: [TrainingExample] -> TYOK.SharedResonanceMap -> Map Text Double
calculateMetrics examples srm =
    let -- Calculate average effectiveness
        avgEffectiveness = sum (map teEffectiveness examples) / fromIntegral (max 1 (length examples))
        
        -- Count unique emotional states
        uniqueStates = length $ nub $ map teEmotionalState examples
        
        -- Count responses by emotion
        emotionCounts = M.fromListWith (+)
            [ (emotion, 1)
            | example <- examples
            , (emotion, score) <- M.toList (esEmotions $ teEmotionalState example)
            , score > 0.3  -- Only count emotions with significant presence
            ]
        
        -- Get top 3 most common emotions
        topEmotions = take 3 $ sortBy (flip compare `on` snd) (M.toList emotionCounts)
    in M.fromList
        [ ("avg_effectiveness", avgEffectiveness)
        , ("unique_states", fromIntegral uniqueStates)
        , ("total_examples", fromIntegral $ length examples)
        ] `M.union` M.fromList
            [ ("top_emotion_" <> pack (show (i+1)) <> "_" <> emo, cnt)
            | (i, (emo, cnt)) <- zip [0..] topEmotions
            ]

-- | Get an empathic response for the current emotional state
getEmpathicResponse :: AgentID -> EmotionalState -> EmpathicModel -> IO (Text, EmpathicModel)
getEmpathicResponse agentId currentState model
    | emAgentId model /= agentId =
        -- If the agent ID doesn't match, return a default response
        return ("I'm not configured to respond to this agent.", model)
    | otherwise = do
        -- Use the shared resonance map to find an appropriate response
        let resonanceMap = emSharedResonanceMap model
            responses = TYOK.matchUserState currentState resonanceMap
            
        if null responses then
            -- Fallback response if no good match is found
            return ("I hear you. Would you like to tell me more about how you're feeling?", model)
        else do
            -- Select the best response (here we just take the first one)
            let response = head responses
            
            -- Update the model with this interaction
            currentTime <- getCurrentTime
            let example = TrainingExample
                    { teAgentId = agentId
                    , teInput = ""  -- We don't have the user's input here
                    , teEmotionalState = currentState
                    , teResponse = TYOK.srText response
                    , teEffectiveness = 0.5  -- Default effectiveness, would be updated based on user feedback
                    , teTimestamp = currentTime
                    , teTags = []
                    }
                
                -- Add this example to the training data
                updatedModel = model  -- Here you'd update the model with the new example
                
            return (TYOK.srText response, updatedModel)

-- | Save the model to disk
saveModel :: FilePath -> EmpathicModel -> IO ()
saveModel path model = do
    createDirectoryIfMissing True (takeDirectory path)
    BL.writeFile path (encode model)

-- | Load a model from disk
loadModel :: FilePath -> IO (Either String EmpathicModel)
loadModel path = do
    exists <- doesFileExist path
    if not exists then
        return $ Left "Model file does not exist"
    else do
        contents <- BL.readFile path
        return $ eitherDecode contents

-- | Extract emotional arcs from a sequence of training examples
extractEmotionalArcs :: [TrainingExample] -> [EmotionalArc]
extractEmotionalArcs examples =
    let -- Group examples by conversation or session
        grouped = groupBy sameSession examples
        
        -- Convert each group to an emotional arc
        arcs = mapMaybe groupToArc grouped
    in arcs
  where
    sameSession a b = teAgentId a == teAgentId b  -- Simplified - would use session ID in practice
    
    groupToArc :: [TrainingExample] -> Maybe EmotionalArc
    groupToArc [] = Nothing
    groupToArc [x] = Nothing  -- Need at least two states for an arc
    groupToArc exs@(firstEx:_) =
        let startState = teEmotionalState $ head exs
            endState = teEmotionalState $ last exs
            transitions = map teEmotionalState exs
            effectiveness = sum (map teEffectiveness exs) / fromIntegral (length exs)
        in Just $ EmotionalArc
            { eaStartState = startState
            , eaEndState = endState
            , eaTransitions = transitions
            , eaEffectiveness = effectiveness
            , eaMetadata = M.empty  -- Could add more metadata here
            }

-- | Generate training examples from emotional arcs
generateTrainingExamples :: [EmotionalArc] -> [TrainingExample]
generateTrainingExamples arcs =
    concatMap arcToExamples arcs
  where
    arcToExamples arc =
        let startState = eaStartState arc
            transitions = eaTransitions arc
            effectiveness = eaEffectiveness arc
            
            -- Create examples for each transition
            examples = zipWith (\from to ->
                TrainingExample
                    { teAgentId = AgentID "system"  -- Default agent ID
                    , teInput = ""  -- No input for generated examples
                    , teEmotionalState = from
                    , teResponse = generateResponse from to
                    , teEffectiveness = effectiveness
                    , teTimestamp = esTimestamp to  -- Use the timestamp of the 'to' state
                    , teTags = ["generated"]
                    }
                ) (startState : transitions) transitions
        in examples
    
    -- Simple response generation - would be more sophisticated in practice
    generateResponse :: EmotionalState -> EmotionalState -> Text
generateResponse from to = 
    let emotionChanges = M.mergeWithKey
            (\_ fromScore toScore -> Just (toScore - fromScore))
            (const M.empty)
            (M.map (\v -> v - 0))  -- All changes are relative to 0 (no change)
            (esEmotions from)
            (esEmotions to)
        
        -- Find the emotion that increased the most
        (maxEmotion, maxChange) = M.foldrWithKey
            (\emo change acc@(_, maxSoFar) ->
                if change > maxSoFar then (Just emo, change) else acc)
            (Nothing, 0)  -- Start with no emotion and 0 change
            emotionChanges
        
    in case maxEmotion of
        Just emo | maxChange > 0.3 -> 
            case emo of
                "joy" -> "I'm glad to hear that! What's making you feel this way?"
                "sadness" -> "I hear the sadness in your voice. Would you like to share more?"
                "anger" -> "I can sense your frustration. What's been bothering you?"
                "fear" -> "That sounds scary. What's been worrying you?"
                _ -> "I notice a change in your emotions. Tell me more about that."
        _ -> "I hear you. Would you like to elaborate on that?"

-- | Load training data from a file
loadTrainingData :: FilePath -> IO (Either String [TrainingExample])
loadTrainingData path = do
    exists <- doesFileExist path
    if not exists then
        return $ Left "Training data file does not exist"
    else do
        contents <- BL.readFile path
        return $ eitherDecode contents

-- | Save training data to a file
saveTrainingData :: FilePath -> [TrainingExample] -> IO ()
saveTrainingData path examples = do
    createDirectoryIfMissing True (takeDirectory path)
    BL.writeFile path (encode examples)

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import OneBloodEmpathyTraining
import ТыИЯОднойКрови (emptyResonanceMap)
import MusicOfTransition (EmotionalState(..))
import Data.Map (fromList)
import Data.Time (getCurrentTime)
import Data.Text (Text, pack)

-- Example emotional states
happyState :: EmotionalState
happyState = EmotionalState
    { esEmotions = fromList [("joy", 0.8), ("trust", 0.7)]
    , esText = "I'm feeling great today!"
    , esLinguisticCues = []
    , esTimestamp = undefined  -- Will be set at runtime
    }

sadState :: EmotionalState
sadState = EmotionalState
    { esEmotions = fromList [("sadness", 0.9), ("fear", 0.3)]
    , esText = "I've been feeling really down lately..."
    , esLinguisticCues = []
    , esTimestamp = undefined
    }

-- Example training examples
exampleTrainingData :: IO [TrainingExample]
exampleTrainingData = do
    now <- getCurrentTime
    let agentId = AgentID "test_agent"
    return
        [ TrainingExample
            { teAgentId = agentId
            , teInput = "I'm feeling really happy today!"
            , teEmotionalState = happyState { esTimestamp = now }
            , teResponse = "That's wonderful! What's bringing you joy today?"
            , teEffectiveness = 0.9
            , teTimestamp = now
            , teTags = ["positive"]
            }
        , TrainingExample
            { teAgentId = agentId
            , teInput = "I've been feeling really down..."
            , teEmotionalState = sadState { esTimestamp = now }
            , teResponse = "I hear you're feeling down. Would you like to talk about what's been on your mind?"
            , teEffectiveness = 0.8
            , teTimestamp = now
            , teTags = ["support"]
            }
        ]

main :: IO ()
main = do
    putStrLn "🚀 Starting OneBloodEmpathyTraining test..."
    
    -- Create a new agent
    let agentId = AgentID "test_agent"
        model = emptyEmpathicModel agentId
    
    -- Load training data
    trainingData <- exampleTrainingData
    putStrLn $ "📊 Loaded " ++ show (length trainingData) ++ " training examples"
    
    -- Train the model
    putStrLn "🧠 Training model..."
    trainedModel <- trainModel trainingData model
    
    -- Test the model
    currentTime <- getCurrentTime
    let testState = happyState { esTimestamp = currentTime }
    
    putStrLn "\n🤖 Testing response generation..."
    (response, _) <- getEmpathicResponse agentId testState trainedModel
    putStrLn $ "Input: " ++ show (esText testState)
    putStrLn $ "Response: " ++ show response
    
    -- Save the model
    let modelPath = "data/test_model.json"
    saveModel modelPath trainedModel
    putStrLn $ "💾 Model saved to " ++ modelPath
    
    -- Load the model back
    putStrLn "\n🔍 Loading model back from disk..."
    loadResult <- loadModel modelPath
    case loadResult of
        Left err -> putStrLn $ "❌ Error loading model: " ++ err
        Right loadedModel -> do
            putStrLn "✅ Model loaded successfully!"
            
            -- Test the loaded model
            let testState2 = sadState { esTimestamp = currentTime }
            (response2, _) <- getEmpathicResponse agentId testState2 loadedModel
            putStrLn $ "\nInput: " ++ show (esText testState2)
            putStrLn $ "Response: " ++ show response2
    
    putStrLn "\n✨ Test completed!"

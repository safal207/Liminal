# OneBloodEmpathyTraining

A training suite for creating empathic AI agents that learn from emotional-linguistic patterns.

## Overview

This project implements a system for training AI agents to respond empathetically by learning from emotional-linguistic patterns. It builds upon the `ТыИЯОднойКрови` module to create a shared emotional memory that can be used by multiple agents.

## Features

- **Multi-Agent Support**: Train and manage multiple AI agents with shared emotional understanding
- **Emotional Arc Analysis**: Detect and learn from emotional transitions in conversations
- **Personalized Responses**: Generate context-aware responses based on emotional state
- **Model Persistence**: Save and load trained models for consistent behavior
- **Training Data Management**: Import and export training data in JSON format

## Getting Started

### Prerequisites

- GHC 8.10 or later
- Stack
- `ТыИЯОднойКрови` module and its dependencies

### Installation

1. Clone the repository:
   ```bash
   git clone https://github.com/yourusername/resonance-liminal.git
   cd resonance-liminal/OneBloodEmpathyTraining
   ```

2. Build the project:
   ```bash
   stack build
   ```

3. Run the tests:
   ```bash
   stack test
   ```

## Usage

### Creating a New Agent

```haskell
import OneBloodEmpathyTraining
import Data.Time (getCurrentTime)

main :: IO ()
main = do
    let agentId = AgentID "my_agent"
    let model = emptyEmpathicModel agentId
    -- Train and use the model...
```

### Training the Model

```haskell
trainAgent :: IO EmpathicModel
trainAgent = do
    let agentId = AgentID "trainable_agent"
    let model = emptyEmpathicModel agentId
    
    -- Load training data
    trainingData <- loadTrainingData "data/training_examples.json"
    case trainingData of
        Left err -> error $ "Failed to load training data: " ++ err
        Right examples -> do
            putStrLn $ "Training on " ++ show (length examples) ++ " examples..."
            trainedModel <- trainModel examples model
            
            -- Save the trained model
            saveModel "data/trained_model.json" trainedModel
            return trainedModel
```

### Getting Empathic Responses

```haskell
respondToUser :: EmpathicModel -> Text -> IO (Text, EmpathicModel)
respondToUser model userInput = do
    currentTime <- getCurrentTime
    
    -- In a real application, you would analyze the user's input to determine
    -- their emotional state. For this example, we'll use a placeholder.
    let emotionalState = EmotionalState
            { esEmotions = fromList [("neutral", 1.0)]
            , esText = userInput
            , esLinguisticCues = []
            , esTimestamp = currentTime
            }
    
    -- Get an empathic response
    (response, updatedModel) <- getEmpathicResponse (emAgentId model) emotionalState model
    
    -- Update the model with this interaction
    let example = TrainingExample
            { teAgentId = emAgentId model
            , teInput = userInput
            , teEmotionalState = emotionalState
            , teResponse = response
            , teEffectiveness = 0.5  -- Would be updated based on user feedback
            , teTimestamp = currentTime
            , teTags = []
            }
    
    -- Retrain the model with the new example
    trainedModel <- trainModel [example] updatedModel
    
    return (response, trainedModel)
```

## Data Format

### Training Example Format

Training examples are stored in JSON format. Here's an example:

```json
[
  {
    "teAgentId": {"unAgentID": "example_agent"},
    "teInput": "I'm feeling really happy today!",
    "teEmotionalState": {
      "esEmotions": {"joy": 0.8, "trust": 0.7},
      "esText": "I'm feeling really happy today!",
      "esLinguisticCues": [],
      "esTimestamp": "2025-07-20T10:30:00Z"
    },
    "teResponse": "That's wonderful! What's bringing you joy today?",
    "teEffectiveness": 0.9,
    "teTimestamp": "2025-07-20T10:30:05Z",
    "teTags": ["positive", "happy"]
  }
]
```

## Architecture

The system is built around several key components:

1. **EmpathicModel**: The core data structure that holds the learned emotional patterns
2. **Training Pipeline**: Processes training examples to update the model
3. **Response Generator**: Uses the trained model to generate empathic responses
4. **Persistence Layer**: Handles saving and loading models and training data

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This project is licensed under the MIT License - see the LICENSE file for details.

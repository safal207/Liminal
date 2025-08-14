{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module ResonantButterflyMap (
    -- * Core Types
    ResonantWave(..),
    ButterflyEffect(..),
    CoAdaptationMap(..),
    WaveMetadata(..),
    
    -- * Core Functions
    emitResonantWave,
    receiveWaveAndAdapt,
    mutualAdaptationLoop,
    
    -- * Utility Functions
    calculateWaveResonance,
    blendEmotionalStates,
    
    -- * CLI Interface
    runResonanceCLI,
    
    -- * Configuration
    defaultWaveConfig,
    WaveConfig(..)
) where

import qualified ТыИЯОднойКрови as TYOK
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Text (Text, pack, unpack)
import Data.List (sortOn, groupBy, nub, maximumBy, foldl')
import Data.Ord (comparing, Down(..))
import Data.Maybe (fromMaybe, mapMaybe, catMaybes, isJust)
import Data.Time (UTCTime, getCurrentTime, diffUTCTime, addUTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import Data.Function (on)
import Data.Ratio ((%))
import Control.Monad (when, unless, foldM, forM_, replicateM)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE, withExceptT)
import Control.Monad.IO.Class (liftIO)
import System.Random (Random(..), newStdGen, randomRs, randomRIO)
import Data.Aeson (FromJSON, ToJSON, object, (.:), (.:?), (.=), (.:!), decode, encode, ToJSONKey, FromJSONKey)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.List.NonEmpty (NonEmpty(..), toList, fromList, nonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Hashable (Hashable, hashWithSalt)
import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON(..), ToJSON(..), Parser, Value(..), withObject)
import Data.UUID (UUID, nil)
import Data.UUID.V4 (nextRandom)
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess, exitFailure, ExitCode(..))
import System.FilePath (takeExtension, (</>), takeBaseName)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently, mapConcurrently_)
import qualified Data.Set as S
import Data.Scientific (toRealFloat)
import Data.Foldable (foldlM)

-- | Configuration for the resonant wave system
data WaveConfig = WaveConfig
    { wcDecayRate :: Double           -- ^ How quickly waves lose strength (0-1)
    , wcEchoDepth :: Int              -- ^ How many times a wave can bounce
    , wcReciprocityFactor :: Double   -- ^ How much to reciprocate (0-1)
    , wcMaxWaves :: Int               -- ^ Maximum concurrent waves per agent
    , wcAdaptationRate :: Double      -- ^ How quickly to adapt to new patterns (0-1)
    , wcMinResonance :: Double        -- ^ Minimum resonance to consider a match (0-1)
    , wcMaxWaveAge :: Double          -- ^ Maximum age of a wave in seconds
    , wcWaveInterval :: Double        -- ^ Time between wave emissions in seconds
    } deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Default configuration for the resonant wave system
defaultWaveConfig :: WaveConfig
defaultWaveConfig = WaveConfig
    { wcDecayRate = 0.1
    , wcEchoDepth = 3
    , wcReciprocityFactor = 0.7
    , wcMaxWaves = 5
    , wcAdaptationRate = 0.3
    , wcMinResonance = 0.4
    , wcMaxWaveAge = 300.0  -- 5 minutes
    , wcWaveInterval = 1.0  -- 1 second
    }

-- | Metadata for a resonant wave
data WaveMetadata = WaveMetadata
    { wmSourceAgent :: Text           -- ^ ID of the agent that created the wave
    , wmTimestamp :: UTCTime          -- ^ When the wave was created
    , wmPath :: [Text]               -- ^ Path the wave has taken (agent IDs)
    , wmStrength :: Double           -- ^ Current strength of the wave (0-1)
    , wmGeneration :: Int            -- ^ How many hops the wave has made
    , wmOriginalIntent :: Text       -- ^ Original emotional intent
    , wmContext :: Map Text Text     -- ^ Additional context for the wave
    } deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | A resonant wave carrying emotional information
data ResonantWave = ResonantWave
    { rwId :: UUID                   -- ^ Unique identifier for the wave
    , rwEmotionalState :: TYOK.EmotionalState  -- ^ Current emotional content
    , rwPhrase :: Text               -- ^ Textual representation of the wave
    , rwMetadata :: WaveMetadata     -- ^ Metadata about the wave
    , rwResponseTo :: Maybe UUID     -- ^ Optional wave this is responding to
    , rwEchoes :: [ResonantWave]     -- ^ Nested echoes from other agents
    } deriving (Show, Eq, Generic)

-- | The effect of a wave on the system
data ButterflyEffect = ButterflyEffect
    { beSourceWave :: ResonantWave           -- ^ The original wave
    , beResponseWaves :: [ResonantWave]      -- ^ Waves generated in response
    , beAdaptations :: [TYOK.EmotionalState] -- ^ Emotional states that were adapted
    , beNewResonances :: [Text]              -- ^ New resonance patterns created
    , beTimestamp :: UTCTime                 -- ^ When the effect occurred
    } deriving (Show, Eq, Generic)

-- | The result of adapting to a wave
data CoAdaptationMap = CoAdaptationMap
    { camAgentId :: Text                     -- ^ ID of the adapting agent
    , camOriginalMap :: TYOK.SharedResonanceMap  -- ^ Map before adaptation
    , camUpdatedMap :: TYOK.SharedResonanceMap   -- ^ Map after adaptation
    , camEffects :: [ButterflyEffect]        -- ^ Effects that caused changes
    , camNewWaves :: [ResonantWave]          -- ^ New waves generated
    , camTimestamp :: UTCTime                -- ^ When adaptation occurred
    } deriving (Show, Eq, Generic)

-- | Emit a new resonant wave based on current emotional state
emitResonantWave 
    :: Text                      -- ^ Agent ID
    -> TYOK.EmotionalState       -- ^ Current emotional state
    -> TYOK.SharedResonanceMap   -- ^ Agent's resonance map
    -> Maybe ResonantWave        -- ^ Optional wave being responded to
    -> WaveConfig                -- ^ Configuration
    -> IO (ResonantWave, [ButterflyEffect])  -- ^ (New wave, effects)
emitResonantWave agentId emotionalState resonanceMap mResponseWave config = do
    currentTime <- getCurrentTime
    waveId <- nextRandom
    
    -- Determine if this is a response to another wave
    let (responseToId, generation, strength, path) = case mResponseWave of
            Just responseWave -> 
                let metadata = rwMetadata responseWave
                    newGen = wmGeneration metadata + 1
                    decayedStrength = wmStrength metadata * (1 - wcDecayRate config)
                    newPath = wmPath metadata ++ [agentId]
                in (Just (rwId responseWave), newGen, decayedStrength, newPath)
            Nothing -> 
                (Nothing, 0, 1.0, [agentId])
    
    -- Create the wave metadata
    let metadata = WaveMetadata
            { wmSourceAgent = agentId
            , wmTimestamp = currentTime
            , wmPath = path
            , wmStrength = strength
            , wmGeneration = generation
            , wmOriginalIntent = TYOK.esText emotionalState
            , wmContext = M.empty  -- Can be extended with additional context
            }
    
    -- Create the wave
    let wave = ResonantWave
            { rwId = waveId
            , rwEmotionalState = emotionalState
            , rwPhrase = generateWavePhrase emotionalState
            , rwMetadata = metadata
            , rwResponseTo = responseToId
            , rwEchoes = []  -- Will be populated when receiving responses
            }
    
    -- If this is a response, update the original wave with this echo
    let effects = case mResponseWave of
            Just responseWave ->
                let updatedWave = responseWave { rwEchoes = wave : rwEchoes responseWave }
                in [ButterflyEffect updatedWave [] [emotionalState] [] currentTime]
            Nothing -> []
    
    return (wave, effects)

-- | Generate a phrase based on emotional state
generateWavePhrase :: TYOK.EmotionalState -> Text
generateWavePhrase state =
    let emotions = TYOK.esEmotions state
        topEmotion = fst <$> M.lookupMax emotions
        baseText = TYOK.esText state
    in case topEmotion of
        Just "joy" -> "Я чувствую волну радости: " <> baseText
        Just "sadness" -> "Сквозь печаль я вижу: " <> baseText
        Just "anger" -> "С жаром в голосе: " <> baseText
        Just "fear" -> "Тревожно шепчу: " <> baseText
        Just "trust" -> "С доверием делюсь: " <> baseText
        _ -> "Я чувствую: " <> baseText

-- | Receive a wave and adapt the resonance map
receiveWaveAndAdapt 
    :: ResonantWave                  -- ^ Incoming wave
    -> TYOK.SharedResonanceMap       -- ^ Current resonance map
    -> WaveConfig                    -- ^ Configuration
    -> IO (CoAdaptationMap, [ResonantWave])  -- ^ (Adaptation result, new waves)
receiveWaveAndAdapt wave resonanceMap config = do
    currentTime <- getCurrentTime
    let metadata = rwMetadata wave
        emotionalState = rwEmotionalState wave
        agentId = TYOK.srmUserId resonanceMap
    
    -- Check if wave is too old
    let waveAge = realToFrac $ diffUTCTime currentTime (wmTimestamp metadata)
    if waveAge > wcMaxWaveAge config
        then do
            putStrLn $ "💤 Wave " ++ show (rwId wave) ++ " is too old (" ++ show waveAge ++ "s)"
            return (emptyCoAdaptationMap agentId resonanceMap, [])
        else do
            -- Calculate resonance with current map
            let resonance = calculateWaveResonance wave resonanceMap
            
            if resonance < wcMinResonance config
                then do
                    -- If resonance is too low, maybe create a new pattern
                    putStrLn $ "🔇 Low resonance (" ++ show resonance ++ ") for wave " ++ show (rwId wave)
                    return (emptyCoAdaptationMap agentId resonanceMap, [])
                else do
                    -- Adapt the resonance map
                    putStrLn $ "🎯 Processing wave " ++ show (rwId wave) ++ " with resonance " ++ show resonance
                    adaptedMap <- adaptMapToWave wave resonanceMap config
                    
                    -- Create response waves
                    responseWaves <- if wmGeneration metadata < wcEchoDepth config
                        then createResponseWaves wave resonanceMap adaptedMap config
                        else return []
                    
                    -- Create adaptation result
                    let effect = ButterflyEffect
                            { beSourceWave = wave
                            , beResponseWaves = responseWaves
                            , beAdaptations = [emotionalState]
                            , beNewResonances = []  -- Would be populated during adaptation
                            , beTimestamp = currentTime
                            }
                        
                        coAdaptation = CoAdaptationMap
                            { camAgentId = agentId
                            , camOriginalMap = resonanceMap
                            , camUpdatedMap = adaptedMap
                            , camEffects = [effect]
                            , camNewWaves = responseWaves
                            , camTimestamp = currentTime
                            }
                    
                    return (coAdaptation, responseWaves)
  where
    emptyCoAdaptationMap agentId rm = CoAdaptationMap
        { camAgentId = agentId
        , camOriginalMap = rm
        , camUpdatedMap = rm
        , camEffects = []
        , camNewWaves = []
        , camTimestamp = currentTime
        }

-- | Calculate how well a wave resonates with a resonance map
calculateWaveResonance 
    :: ResonantWave 
    -> TYOK.SharedResonanceMap 
    -> Double
calculateWaveResonance wave resonanceMap =
    let emotionalState = rwEmotionalState wave
        nodes = M.elems (TYOK.srmNodes resonanceMap)
        
        -- Calculate similarity with each node
        similarities = map (\node -> 
            let nodeState = TYOK.rnEmotionalFingerprint node
                similarity = TYOK.measureResonance emotionalState node
            in similarity * TYOK.rnOccurrences node  -- Weight by occurrence
            ) nodes
        
        -- Normalize by total occurrences
        totalOccurrences = sum (map TYOK.rnOccurrences nodes)
    in if null similarities || totalOccurrences == 0
        then 0
        else sum similarities / fromIntegral (max 1 totalOccurrences)

-- | Adapt the resonance map based on a wave
adaptMapToWave 
    :: ResonantWave 
    -> TYOK.SharedResonanceMap 
    -> WaveConfig 
    -> IO TYOK.SharedResonanceMap
adaptMapToWave wave resonanceMap config = do
    currentTime <- getCurrentTime
    let emotionalState = rwEmotionalState wave
        metadata = rwMetadata wave
        strength = wmStrength metadata
        
        -- Create a new emotional transition from the wave
        transition = TYOK.EmotionalTransition
            { TYOK.etStates = emotionalState :| []  -- Single state for now
            , TYOK.etTimestamp = currentTime
            , TYOK.etEffectiveness = strength * wcAdaptationRate config
            }
    
    -- Learn from this transition
    TYOK.learnFromTransitions [transition] resonanceMap

-- | Create response waves based on the received wave
createResponseWaves 
    :: ResonantWave 
    -> TYOK.SharedResonanceMap 
    -> TYOK.SharedResonanceMap 
    -> WaveConfig 
    -> IO [ResonantWave]
createResponseWaves wave oldMap newMap config = do
    currentTime <- getCurrentTime
    let emotionalState = rwEmotionalState wave
        metadata = rwMetadata wave
        strength = wmStrength metadata
        
        -- Find similar emotional states in our map
        similarStates = TYOK.findSimilarStates emotionalState oldMap
        
        -- Filter by minimum resonance
        relevantStates = filter ((>= wcMinResonance config) . snd) similarStates
        
        -- Take top N most similar
        topStates = take (wcMaxWaves config) $ 
                    sortOn (Down . snd) relevantStates
    
    -- Create response waves for each similar state
    mapM (\(node, similarity) -> do
        let responseEmotionalState = TYOK.rnEmotionalFingerprint node
            -- Blend with original wave's emotion based on reciprocity
            blendedState = blendEmotionalStates 
                emotionalState 
                responseEmotionalState 
                (wcReciprocityFactor config * similarity)
            
        -- Emit a new wave in response
        (responseWave, _) <- emitResonantWave 
            (TYOK.srmUserId oldMap)  -- Our agent ID
            blendedState 
            newMap 
            (Just wave) 
            config
            
        return responseWave
        ) topStates

-- | Blend two emotional states with a given weight
blendEmotionalStates 
    :: TYOK.EmotionalState  -- ^ Source state (wave)
    -> TYOK.EmotionalState  -- ^ Target state (our response)
    -> Double               -- ^ Blend factor (0 = all target, 1 = all source)
    -> TYOK.EmotionalState  -- ^ Blended state
blendEmotionalStates source target blendFactor =
    let -- Blend emotions
        sourceEmotions = TYOK.esEmotions source
        targetEmotions = TYOK.esEmotions target
        
        blendedEmotions = M.unionWith (\s t -> s * blendFactor + t * (1 - blendFactor))
                            sourceEmotions targetEmotions
        
        -- Blend text (simple concatenation for now)
        blendedText = TYOK.esText source <> " | " <> TYOK.esText target
        
        -- Blend linguistic cues (union)
        blendedCues = nub $ TYOK.esLinguisticCues source ++ TYOK.esLinguisticCues target
        
    in target
        { TYOK.esEmotions = blendedEmotions
        , TYOK.esText = blendedText
        , TYOK.esLinguisticCues = blendedCues
        }

-- | Run the mutual adaptation loop with multiple agents
mutualAdaptationLoop 
    :: [TYOK.SharedResonanceMap]  -- ^ Initial agent maps
    -> WaveConfig                 -- ^ Configuration
    -> Int                        -- ^ Number of iterations
    -> IO [CoAdaptationMap]       -- ^ Adaptation results
mutualAdaptationLoop agentMaps config numIterations = do
    putStrLn $ "🌀 Starting mutual adaptation with " ++ show (length agentMaps) ++
              " agents for " ++ show numIterations ++ " iterations"
    
    foldM (\acc iteration -> do
        putStrLn $ "\n🔄 Iteration " ++ show (iteration + 1) ++ 
                  " of " ++ show numIterations
        
        -- Each agent takes a turn to emit waves
        results <- mapConcurrently (\agentMap -> do
            let agentId = TYOK.srmUserId agentMap
            
            -- Emit a wave based on current state
            currentTime <- getCurrentTime
            let emotionalState = sampleEmotionalState agentMap  -- Simplified
            
            (wave, _) <- emitResonantWave agentId emotionalState agentMap Nothing config
            
            -- Other agents receive and respond to the wave
            let otherAgents = filter ((/= agentId) . TYOK.srmUserId) agentMaps
            
            -- Process wave in parallel for each agent
            adaptations <- mapConcurrently (\recipientMap -> do
                (adaptation, _) <- receiveWaveAndAdapt wave recipientMap config
                return (TYOK.srmUserId recipientMap, adaptation)
                ) otherAgents
            
            -- Update agent maps with adaptations
            let updatedAgentMaps = map (\(aid, adaptation) ->
                    if aid == TYOK.srmUserId agentMap 
                        then agentMap  -- No self-update
                        else camUpdatedMap adaptation
                    ) adaptations
            
            -- Return the agent's own map and the adaptations it caused
            return (agentMap, concatMap (camEffects . snd) adaptations)
            ) agentMaps
        
        -- Extract all adaptations and update agent maps
        let allAdaptations = concatMap snd results
            updatedMaps = map fst results  -- For now, just return the original maps
            
        -- Wait before next iteration
        let intervalMs = floor (wcWaveInterval config * 1000000)  -- Convert to microseconds
        threadDelay intervalMs
        
        return (acc ++ allAdaptations)
        ) [] [0..numIterations-1]

-- | Sample an emotional state from a resonance map (simplified)
sampleEmotionalState :: TYOK.SharedResonanceMap -> TYOK.EmotionalState
sampleEmotionalState resonanceMap =
    let nodes = M.elems (TYOK.srmNodes resonanceMap)
        totalOccurrences = sum (map TYOK.rnOccurrences nodes)
    in if null nodes
        then TYOK.EmotionalState M.empty "" [] (read "1970-01-01 00:00:00 UTC")
        else do
            -- Select a node weighted by occurrence
            let weightedNodes = concatMap (\n -> replicate (TYOK.rnOccurrences n) n) nodes
            
            if null weightedNodes
                then TYOK.EmotionalState M.empty "" [] (read "1970-01-01 00:00:00 UTC")
                else do
                    idx <- randomRIO (0, length weightedNodes - 1)
                    let node = weightedNodes !! idx
                    currentTime <- getCurrentTime
                    return $ TYOK.EmotionalState
                        { TYOK.esEmotions = TYOK.rnEmotionalFingerprint node
                        , TYOK.esText = "Sampled from map: " <> TYOK.rnId node
                        , TYOK.esLinguisticCues = TYOK.rnLinguisticCues node
                        , TYOK.esTimestamp = currentTime
                        }

-- | CLI interface for the resonance system
runResonanceCLI :: IO ()
runResonanceCLI = do
    args <- getArgs
    case args of
        ["resonate", "mutual", agentMapFiles] -> do
            -- Load agent maps
            agentMaps <- mapM loadResonanceMap agentMapFiles
            
            -- Run mutual adaptation
            let config = defaultWaveConfig
            _ <- mutualAdaptationLoop agentMaps config 5  -- 5 iterations by default
            
            -- Save updated maps
            forM_ (zip agentMapFiles agentMaps) $ \(file, map') -> do
                let newFile = "resonated_" ++ takeBaseName file
                BL.writeFile newFile (encode map')
                putStrLn $ "💾 Saved resonated map to " ++ newFile
            
        ["resonate", "wave", sourceMap, targetMap] -> do
            -- Load maps
            sourceRes <- loadResonanceMap sourceMap
            targetRes <- loadResonanceMap targetMap
            
            -- Create a wave from source to target
            currentTime <- getCurrentTime
            let emotionalState = sampleEmotionalState sourceRes
                wave = ResonantWave
                    { rwId = nil  -- Will be set by emitResonantWave
                    , rwEmotionalState = emotionalState
                    , rwPhrase = "Initial wave from " ++ TYOK.srmUserId sourceRes
                    , rwMetadata = WaveMetadata
                        { wmSourceAgent = TYOK.srmUserId sourceRes
                        , wmTimestamp = currentTime
                        , wmPath = [TYOK.srmUserId sourceRes]
                        , wmStrength = 1.0
                        , wmGeneration = 0
                        , wmOriginalIntent = "Initial wave"
                        , wmContext = M.empty
                        }
                    , rwResponseTo = Nothing
                    , rwEchoes = []
                    }
            
            -- Process the wave
            (adaptation, responses) <- receiveWaveAndAdapt wave targetRes defaultWaveConfig
            
            -- Save results
            BL.writeFile "adaptation_result.json" (encode adaptation)
            BL.writeFile "response_waves.json" (encode responses)
            
            putStrLn "✅ Wave processing complete. Results saved to adaptation_result.json and response_waves.json"
            
        _ -> do
            progName <- getProgName
            putStrLn $ "Usage:"
            putStrLn $ "  " ++ progName ++ " resonate mutual AGENT_MAP1 [AGENT_MAP2 ...]"
            putStrLn $ "  " ++ progName ++ " resonate wave SOURCE_MAP TARGET_MAP"
            exitFailure

-- | Load a resonance map from file
loadResonanceMap :: FilePath -> IO TYOK.SharedResonanceMap
loadResonanceMap file = do
    content <- BL.readFile file
    case decode content of
        Just rm -> return rm
        Nothing -> error $ "Failed to parse resonance map: " ++ file

-- Add missing instances for JSON serialization
instance FromJSON TYOK.EmotionalState where
    parseJSON = withObject "EmotionalState" $ \v -> do
        emotions <- v .: "emotions"
        text <- v .: "text"
        cues <- v .: "linguisticCues"
        timestamp <- v .: "timestamp"
        return $ TYOK.EmotionalState emotions text cues timestamp

instance ToJSON TYOK.EmotionalState where
    toJSON state = object
        [ "emotions" .= TYOK.esEmotions state
        , "text" .= TYOK.esText state
        , "linguisticCues" .= TYOK.esLinguisticCues state
        , "timestamp" .= TYOK.esTimestamp state
        ]

-- Add missing instances for UUID
instance FromJSON UUID where
    parseJSON = withText "UUID" $ \t -> 
        case fromString (unpack t) of
            Just uuid -> return uuid
            Nothing -> fail "Invalid UUID format"

instance ToJSON UUID where
    toJSON = toJSON . show

-- Add missing instance for ResonantWave
instance FromJSON ResonantWave where
    parseJSON = withObject "ResonantWave" $ \v -> do
        rwId <- v .: "id"
        rwEmotionalState <- v .: "emotionalState"
        rwPhrase <- v .: "phrase"
        rwMetadata <- v .: "metadata"
        rwResponseTo <- v .: "responseTo"
        rwEchoes <- v .: "echoes"
        return $ ResonantWave{..}

instance ToJSON ResonantWave where
    toJSON wave = object
        [ "id" .= rwId wave
        , "emotionalState" .= rwEmotionalState wave
        , "phrase" .= rwPhrase wave
        , "metadata" .= rwMetadata wave
        , "responseTo" .= rwResponseTo wave
        , "echoes" .= rwEchoes wave
        ]

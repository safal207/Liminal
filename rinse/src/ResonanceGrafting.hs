{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ResonanceGrafting (
    -- * Core Types
    GraftingConfig(..),
    defaultGraftingConfig,
    
    -- * Main Functions
    selectPathsForGraft,
    calculateAffinityIndex,
    graftMap,
    
    -- * Utility Functions
    adaptTransition,
    mergeNodes,
    
    -- * CLI Integration
    graftCLI
) where

import qualified ТыИЯОднойКрови as TYOK
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Text (Text, pack, unpack)
import Data.List (sortOn, groupBy, nub, maximumBy)
import Data.Ord (comparing, Down(..))
import Data.Maybe (fromMaybe, mapMaybe, catMaybes, isJust)
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import Data.Function (on)
import Data.Ratio ((%))
import Data.Scientific (toRealFloat)
import Control.Monad (when)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE, withExceptT)
import Control.Monad.IO.Class (liftIO)
import System.Random (Random(..), newStdGen, randomRs)
import Data.Aeson (FromJSON, ToJSON, object, (.:), (.:?), (.=), (.:!), decode, encode)
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty(..), toList, fromList, nonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Hashable (Hashable, hashWithSalt)
import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON(..), ToJSON(..), Parser, Value(..), withObject)

-- | Configuration for the grafting process
data GraftingConfig = GraftingConfig
    { gcMaxPathsToGraft :: Int              -- ^ Maximum number of paths to graft
    , gcMinAffinity :: Double               -- ^ Minimum affinity score (0-1) for grafting
    , gcDecayFactor :: Double               -- ^ How much to decay weights during transfer (0-1)
    , gcPreserveSource :: Bool              -- ^ Whether to keep source-specific identifiers
    , gcAdaptationRate :: Double            -- ^ How much to adapt to target (0 = no adaptation, 1 = full)
    } deriving (Show, Eq, Generic)

instance FromJSON GraftingConfig
instance ToJSON GraftingConfig

-- | Default configuration for grafting
--   - Max 10 paths per graft
--   - Minimum 0.3 affinity required
--   - 30% decay in weights
--   - Moderate adaptation to target
--   - Don't preserve source identifiers
defaultGraftingConfig :: GraftingConfig
defaultGraftingConfig = GraftingConfig
    { gcMaxPathsToGraft = 10
    , gcMinAffinity = 0.3
    , gcDecayFactor = 0.7
    , gcPreserveSource = False
    , gcAdaptationRate = 0.5
    }

-- | Calculate affinity between two emotional states
--   Returns a value between 0 (no affinity) and 1 (perfect affinity)
calculateAffinityIndex :: TYOK.EmotionalState -> TYOK.EmotionalState -> Double
calculateAffinityIndex state1 state2 =
    let -- Calculate cosine similarity between emotion vectors
        emotions1 = TYOK.esEmotions state1
        emotions2 = TYOK.esEmotions state2
        
        -- Get all unique emotion keys
        allEmotions = M.keysSet emotions1 `M.union` M.keysSet emotions2
        
        -- Calculate dot product and magnitudes
        (dotProduct, mag1, mag2) = M.foldlWithKey' (\(dp, m1, m2) emo _ ->
            let v1 = M.findWithDefault 0 emo emotions1
                v2 = M.findWithDefault 0 emo emotions2
            in (dp + v1 * v2, m1 + v1 * v1, m2 + v2 * v2)
            ) (0, 0, 0) allEmotions
        
        -- Calculate cosine similarity (handle division by zero)
        similarity = if mag1 == 0 || mag2 == 0
            then 0
            else dotProduct / (sqrt mag1 * sqrt mag2)
        
        -- Consider linguistic cue similarity (weighted lower)
        cueSimilarity = 0.3 * linguisticCueSimilarity (TYOK.esLinguisticCues state1) (TYOK.esLinguisticCues state2)
        
    in (0.7 * similarity + cueSimilarity) `max` 0 `min` 1

-- | Calculate similarity between linguistic cues
linguisticCueSimilarity :: [TYOK.LinguisticCue] -> [TYOK.LinguisticCue] -> Double
linguisticCueSimilarity cues1 cues2 =
    if null cues1 && null cues2
        then 1.0  -- Both empty is considered a match
        else if null cues1 || null cues2
            then 0.0  -- One empty, one not
            else fromIntegral (length (cues1 `intersect` cues2)) / fromIntegral (max (length cues1) (length cues2))
  where
    intersect = filter (`elem` cues2) cues1

-- | Select transition paths from source map suitable for grafting
--   Returns a list of (path, averageAffinity) pairs, sorted by quality
selectPathsForGraft 
    :: TYOK.SharedResonanceMap  -- ^ Source map
    -> TYOK.SharedResonanceMap  -- ^ Target map
    -> GraftingConfig           -- ^ Grafting configuration
    -> IO [(NonEmpty TYOK.EmotionalState, Double)]  -- ^ Selected paths with affinity scores
selectPathsForGraft sourceMap targetMap config = do
    -- Get all possible paths in the source map
    let allPaths = extractAllPaths sourceMap
    
    -- If target map is empty, return some random paths from source
    if M.null (TYOK.srmNodes targetMap)
        then do
            gen <- newStdGen
            let selectedIndices = take (gcMaxPathsToGraft config) $ 
                    nub (randomRs (0, length allPaths - 1) gen)
                selectedPaths = map (allPaths !!) selectedIndices
            return $ map (\p -> (p, 1.0)) selectedPaths  -- Max affinity for empty target
        else do
            -- For each path, calculate average affinity with target map
            pathScores <- mapM (\path -> do
                let pathAffinities = map (\state -> 
                        maximum (0 : map (calculateAffinityIndex state) (M.elems $ TYOK.srmNodes targetMap))
                        ) (toList path)
                    avgAffinity = if null pathAffinities 
                        then 0 
                        else sum pathAffinities / fromIntegral (length pathAffinities)
                return (path, avgAffinity)
                ) allPaths
            
            -- Filter by minimum affinity and sort by score (descending)
            let filtered = filter ((>= gcMinAffinity config) . snd) pathScores
                sorted = sortOn (Down . snd) filtered
                
            -- Take top N paths
            return $ take (gcMaxPathsToGraft config) sorted

-- | Extract all possible paths in a resonance map
extractAllPaths :: TYOK.SharedResonanceMap -> [NonEmpty TYOK.EmotionalState]
extractAllPaths srm = 
    let nodes = M.elems (TYOK.srmNodes srm)
        -- For each node, start a path and follow transitions
        allPaths = concatMap (\node -> 
            extractPathsFromNode node (M.elems $ TYOK.srmNodes srm) [] [node]
            ) nodes
    in map (fmap TYOK.rnEmotionalFingerprint) $ filter ((>1) . length) allPaths
  where
    extractPathsFromNode :: TYOK.ResonanceNode -> [TYOK.ResonanceNode] -> [Text] -> [TYOK.ResonanceNode] -> [[TYOK.ResonanceNode]]
    extractPathsFromNode current allNodes visited path =
        let currentId = TYOK.rnId current
            nextNodeIds = TYOK.rnTransitions current
            visited' = currentId : visited
            
            -- Get next nodes that haven't been visited yet
            nextNodes = filter (\n -> TYOK.rnId n `notElem` visited') $ 
                        mapMaybe (\id -> find ((==id) . TYOK.rnId) allNodes) nextNodeIds
            
            -- Continue paths from next nodes
            extendedPaths = concatMap (\next -> 
                extractPathsFromNode next allNodes visited' (path ++ [next])) nextNodes
            
        in if null nextNodes 
            then [path]  -- End of path
            else extendedPaths
    
    find :: (a -> Bool) -> [a] -> Maybe a
    find _ [] = Nothing
    find p (x:xs) = if p x then Just x else find p xs

-- | Adapt a transition from source to better fit target
adaptTransition 
    :: GraftingConfig
    -> TYOK.SharedResonanceMap  -- ^ Source map
    -> TYOK.SharedResonanceMap  -- ^ Target map
    -> TYOK.EmotionalTransition -- ^ Source transition
    -> IO TYOK.EmotionalTransition  -- ^ Adapted transition
adaptTransition config _ targetMap sourceTransition = do
    currentTime <- getCurrentTime
    
    let states = toList (TYOK.etStates sourceTransition)
        targetNodes = M.elems (TYOK.srmNodes targetMap)
        
        -- Find most similar node in target map for each state
        findSimilarNode state = 
            if null targetNodes
                then Nothing
                else Just $ maximumBy (comparing (calculateAffinityIndex state . TYOK.rnEmotionalFingerprint)) targetNodes
        
        -- Calculate adaptation factors based on similarity to target
        adaptationFactors = map (\state ->
            case findSimilarNode state of
                Nothing -> gcAdaptationRate config
                Just targetNode -> 
                    let similarity = calculateAffinityIndex state (TYOK.rnEmotionalFingerprint targetNode)
                    in gcAdaptationRate config * (1 - similarity)  -- Adapt more if less similar
            ) states
        
        -- Adapt each state in the transition
        adaptedStates = zipWith (adaptState config targetMap) states adaptationFactors
        
    -- Create new transition with adapted states
    case nonEmpty adaptedStates of
        Nothing -> return sourceTransition  -- Shouldn't happen as transitions should have at least one state
        Just states' -> do
            -- Adjust timestamps to be recent
            let timeNow = currentTime
                duration = case (TYOK.etTimestamp (last states) `diffUTCTime` TYOK.etTimestamp (head states)) of
                    d | d > 0 -> d
                      | otherwise -> 60  -- Default 1 minute if timestamps are invalid
                
                -- Create new timestamps spread over the same duration but recent
                startTime = timeNow
                timeStep = realToFrac duration / fromIntegral (length states)
                newTimestamps = map (\i -> addUTCTime (fromIntegral i * timeStep) startTime) [0..]
                
                -- Update states with new timestamps
                updatedStates = zipWith (\state ts -> state { TYOK.esTimestamp = ts }) (toList states') newTimestamps
                
            return $ TYOK.EmotionalTransition
                { TYOK.etStates = fromList updatedStates
                , TYOK.etTimestamp = startTime
                , TYOK.etEffectiveness = TYOK.etEffectiveness sourceTransition * gcDecayFactor config
                }
  where
    -- Adapt a single emotional state
    adaptState :: GraftingConfig -> TYOK.SharedResonanceMap -> TYOK.EmotionalState -> Double -> TYOK.EmotionalState
    adaptState cfg targetMap state adaptationFactor =
        let targetNodes = M.elems (TYOK.srmNodes targetMap)
            
            -- Find most similar node in target map
            maybeTargetNode = if null targetNodes 
                then Nothing 
                else Just $ maximumBy (comparing (calculateAffinityIndex state . TYOK.rnEmotionalFingerprint)) targetNodes
            
            -- Blend emotions with target
            adaptedEmotions = case maybeTargetNode of
                Nothing -> TYOK.esEmotions state
                Just targetNode ->
                    let targetEmotions = TYOK.rnEmotionalFingerprint targetNode
                        blended = M.unionWith (\s t -> s * (1 - adaptationFactor) + t * adaptationFactor) 
                                    (TYOK.esEmotions state) targetEmotions
                    in blended
            
            -- Adapt linguistic cues
            adaptedCues = if gcPreserveSource cfg
                then TYOK.esLinguisticCues state
                else case maybeTargetNode of
                    Nothing -> TYOK.esLinguisticCues state
                    Just targetNode -> 
                        let targetCues = TYOK.rnLinguisticCues targetNode
                            -- Take some cues from target, some from source
                            n = floor (fromIntegral (length targetCues) * adaptationFactor) :: Int
                            (fromTarget, fromSource) = splitAt n targetCues
                            remaining = drop n (TYOK.esLinguisticCues state)
                        in take 10 (fromTarget ++ remaining)  -- Limit total number of cues
            
        in state
            { TYOK.esEmotions = adaptedEmotions
            , TYOK.esLinguisticCues = adaptedCues
            }

-- | Merge two resonance nodes during grafting
mergeNodes :: TYOK.ResonanceNode -> TYOK.ResonanceNode -> TYOK.ResonanceNode
mergeNodes sourceNode targetNode =
    let -- Average emotion weights
        blendedEmotions = M.unionWith (\s t -> (s + t) / 2) 
            (TYOK.rnEmotionalFingerprint sourceNode) 
            (TYOK.rnEmotionalFingerprint targetNode)
        
        -- Combine phrases, removing duplicates
        combinedPhrases = nub $ TYOK.rnCommonPhrases sourceNode ++ TYOK.rnCommonPhrases targetNode
        
        -- Combine linguistic cues, removing duplicates
        combinedCues = nub $ TYOK.rnLinguisticCues sourceNode ++ TYOK.rnLinguisticCues targetNode
        
        -- Combine transitions, removing duplicates
        combinedTransitions = nub $ TYOK.rnTransitions sourceNode ++ TYOK.rnTransitions targetNode
        
        -- Combine breakthroughs, removing duplicates
        combinedBreakthroughs = nub $ TYOK.rnBreakthroughs sourceNode ++ TYOK.rnBreakthroughs targetNode
        
        -- Sum occurrences
        totalOccurrences = TYOK.rnOccurrences sourceNode + TYOK.rnOccurrences targetNode
        
    in targetNode
        { TYOK.rnEmotionalFingerprint = blendedEmotions
        , TYOK.rnCommonPhrases = take 20 combinedPhrases  -- Limit number of phrases
        , TYOK.rnLinguisticCues = take 10 combinedCues   -- Limit number of cues
        , TYOK.rnTransitions = combinedTransitions
        , TYOK.rnBreakthroughs = combinedBreakthroughs
        , TYOK.rnOccurrences = totalOccurrences
        }

-- | Main grafting function - grafts patterns from source map into target map
graftMap 
    :: TYOK.SharedResonanceMap  -- ^ Source map
    -> TYOK.SharedResonanceMap  -- ^ Target map
    -> GraftingConfig           -- ^ Grafting configuration
    -> IO (TYOK.SharedResonanceMap, Int)  -- ^ (Updated target map, number of paths grafted)
graftMap sourceMap targetMap config = do
    -- Select paths to graft
    selectedPaths <- selectPathsForGraft sourceMap targetMap config
    
    if null selectedPaths
        then return (targetMap, 0)  -- Nothing to graft
        else do
            putStrLn $ "🔧 Grafting " ++ show (length selectedPaths) ++
                     " paths with minimum affinity " ++ show (gcMinAffinity config)
            
            -- Process each selected path
            (finalMap, count) <- foldM (\(currentMap, count) (path, affinity) -> do
                putStrLn $ "  🛠️  Grafting path with affinity " ++ show (round (affinity * 100) :: Int) ++
                         "% (" ++ show (length path) ++ " states)"
                
                -- Convert path to transitions and adapt them
                let transitions = createTransitionsFromPath path
                adaptedTransitions <- mapM (adaptTransition config sourceMap currentMap) transitions
                
                -- Learn from adapted transitions
                updatedMap <- TYOK.learnFromTransitions adaptedTransitions currentMap
                
                return (updatedMap, count + 1)
                ) (targetMap, 0) selectedPaths
            
            putStrLn $ "✅ Successfully grafted " ++ show count ++ " paths"
            return (finalMap, count)
  where
    -- Convert a path of states into a list of transitions
    createTransitionsFromPath :: NonEmpty TYOK.EmotionalState -> [TYOK.EmotionalTransition]
    createTransitionsFromPath states =
        let stateList = toList states
            -- Create transitions between consecutive states
            transitions = zipWith (\s1 s2 -> 
                TYOK.EmotionalTransition
                    { TYOK.etStates = s1 :| [s2]
                    , TYOK.etTimestamp = TYOK.esTimestamp s2
                    , TYOK.etEffectiveness = 0.7  -- Moderate effectiveness for grafted transitions
                    }
                ) stateList (tail stateList)
        in transitions

-- | CLI interface for grafting
--   Usage: empathy-cli graft --from source.json --to target.json --output result.json [--config config.json]
graftCLI :: [String] -> IO ()
graftCLI args = do
    -- Parse command line arguments
    let (sourcePath, targetPath, outputPath, configPath) = parseGraftArgs args
    
    -- Load configuration
    config <- case configPath of
        Just path -> do
            configData <- BL.readFile path
            case decode configData of
                Just cfg -> return cfg
                Nothing -> do
                    putStrLn $ "⚠️  Could not parse config file " ++ path ++ ", using defaults"
                    return defaultGraftingConfig
        Nothing -> return defaultGraftingConfig
    
    -- Load source and target maps
    sourceResult <- loadResonanceMap sourcePath
    targetResult <- loadResonanceMap targetPath
    
    case (sourceResult, targetResult) of
        (Left err, _) -> putStrLn $ "❌ Error loading source map: " ++ err
        (_, Left err) -> putStrLn $ "❌ Error loading target map: " ++ err
        (Right sourceMap, Right targetMap) -> do
            -- Perform grafting
            (resultMap, count) <- graftMap sourceMap targetMap config
            
            -- Save result
            BL.writeFile outputPath (encode resultMap)
            putStrLn $ "✅ Successfully grafted " ++ show count ++ " paths into " ++ outputPath
  where
    parseGraftArgs :: [String] -> (FilePath, FilePath, FilePath, Maybe FilePath)
    parseGraftArgs args = go args Nothing Nothing Nothing Nothing
      where
        go [] src tgt out cfg = 
            (fromMaybe "source.json" src, 
             fromMaybe "target.json" tgt, 
             fromMaybe "result.json" out, 
             cfg)
        go ("--from":src:rest) _ tgt out cfg = go rest (Just src) tgt out cfg
        go ("--to":tgt:rest) src _ out cfg = go rest src (Just tgt) out cfg
        go ("--output":out:rest) src tgt _ cfg = go rest src tgt (Just out) cfg
        go ("--config":cfg:rest) src tgt out _ = go rest src tgt out (Just cfg)
        go (_:rest) src tgt out cfg = go rest src tgt out cfg  -- Ignore unknown args

-- | Load a resonance map from file (helper function)
loadResonanceMap :: FilePath -> IO (Either String TYOK.SharedResonanceMap)
loadResonanceMap filePath = do
    content <- BL.readFile filePath
    return $ eitherDecode content

-- | Save a resonance map to file (helper function)
saveResonanceMap :: FilePath -> TYOK.SharedResonanceMap -> IO ()
saveResonanceMap filePath srm = do
    BL.writeFile filePath (encode srm)
    putStrLn $"✅ Saved resonance map to " ++ filePath

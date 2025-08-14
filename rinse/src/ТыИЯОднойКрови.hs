{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
module ТыИЯОднойКрови (
    -- * Core Types
    SharedResonanceMap(..),
    SuggestedResponse(..),
    ResonanceNode(..),
    
    -- * Learning Functions
    learnFromTransitions,
    
    -- * Matching Functions
    matchUserState,
    findResonantPath,
    
    -- * Persistence
    saveMap,
    loadMap,
    
    -- * Utilities
    measureResonance,
    findSimilarStates,
    
    -- * Example Responses
    defaultResponses
) where

import Data.Aeson (ToJSON, FromJSON, object, (.=), (.:), (.:?), decode, encode)
import qualified Data.ByteString.Lazy as BL
import Data.List (sortOn, groupBy, nub, (\))
import Data.Maybe (mapMaybe, catMaybes, fromMaybe)
import Data.Ord (Down(..))
import Data.Text (Text, pack, unpack, toLower)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

-- Re-exports from other modules
import MusicOfTransition (EmotionalTransition(..), EmotionalState(..))
import EmotionLinguisticMap (LinguisticCue(..), CueType(..))

-- | A node in our resonance map representing a cluster of similar emotional states
data ResonanceNode = ResonanceNode
    { rnId :: Text
    , rnEmotionalFingerprint :: Map Text Double  -- Normalized emotion scores
    , rnCommonPhrases :: [Text]                 -- Frequently occurring phrases
    , rnLinguisticCues :: [CueType]             -- Common linguistic patterns
    , rnTransitions :: [Text]                   -- IDs of connected nodes
    , rnBreakthroughs :: [Text]                 -- Responses that led to positive transitions
    , rnOccurrences :: Int                      -- How many times we've seen this pattern
    } deriving (Show, Eq, Generic)

-- | A suggested response to an emotional state
data SuggestedResponse = SuggestedResponse
    { srId :: Text
    , srText :: Text
    , srEmotionalTone :: Text                   -- e.g., "comforting", "challenging", "reflective"
    , srEffectiveness :: Double                 -- 0.0 to 1.0
    , srLastUsed :: Maybe UTCTime
    } deriving (Show, Eq, Generic)

-- | The shared resonance map that learns from user interactions
data SharedResonanceMap = SharedResonanceMap
    { srmUserId :: Text
    , srmNodes :: Map Text ResonanceNode
    , srmResponses :: Map Text SuggestedResponse
    , srmEmotionWeights :: Map Text Double     -- How much weight to give each emotion
    , srmLastUpdated :: UTCTime
    } deriving (Show, Eq, Generic)

-- | Default emotion weights (can be adjusted based on user's emotional vocabulary)
defaultEmotionWeights :: Map Text Double
defaultEmotionWeights = M.fromList
    [ ("страх", 1.2)
    , ("радость", 1.0)
    , ("грусть", 1.1)
    , ("гнев", 1.3)
    , ("доверие", 0.9)
    , ("ожидание", 0.8)
    , ("удивление", 0.7)
    , ("отвращение", 1.0)
    ]

-- | Create a new, empty resonance map
emptyResonanceMap :: Text -> SharedResonanceMap
emptyResonanceMap userId = SharedResonanceMap
    { srmUserId = userId
    , srmNodes = M.empty
    , srmResponses = M.fromList $ map (\r -> (srId r, r)) defaultResponses
    , srmEmotionWeights = defaultEmotionWeights
    , srmLastUpdated = undefined  -- Will be set when used
    }

-- | Learn from a sequence of emotional transitions
learnFromTransitions :: [EmotionalTransition] -> SharedResonanceMap -> IO SharedResonanceMap
learnFromTransitions transitions srm = do
    currentTime <- getCurrentTime
    
    let -- Extract all emotional states from transitions
        allStates = concatMap (toList . etStates) transitions
        
        -- Cluster similar states
        clusters = clusterSimilarStates allStates
        
        -- Create or update resonance nodes
        updatedNodes = foldr (\cluster nodes ->
            let fingerprint = createEmotionalFingerprint cluster
                phrases = extractCommonPhrases cluster
                cues = extractCommonCues cluster
                nodeId = T.take 8 (T.pack (show (hashFingerprint fingerprint)))
                
                -- Find or create node
                (newNode, updatedMap) = case M.lookup nodeId (srmNodes srm) of
                    Just existing -> 
                        let updatedNode = existing
                                { rnCommonPhrases = nub $ rnCommonPhrases existing ++ phrases
                                , rnLinguisticCues = nub $ rnLinguisticCues existing ++ cues
                                , rnOccurrences = rnOccurrences existing + 1
                                }
                        in (updatedNode, M.insert nodeId updatedNode nodes)
                    Nothing ->
                        let newNode = ResonanceNode
                                { rnId = nodeId
                                , rnEmotionalFingerprint = fingerprint
                                , rnCommonPhrases = phrases
                                , rnLinguisticCues = cues
                                , rnTransitions = []
                                , rnBreakthroughs = []
                                , rnOccurrences = 1
                                }
                        in (newNode, M.insert nodeId newNode nodes)
                
            in updatedMap
            ) (srmNodes srm) clusters
        
        -- Update transitions between nodes
        updatedWithTransitions = foldr (\et nodes ->
            let states = toList (etStates et)
                nodeIds = mapMaybe (\s -> findBestMatchingNode s nodes) states
                -- Create transitions between consecutive nodes
                transitions = zipWith (\a b -> (a, b)) nodeIds (tail nodeIds)
                updated = foldr (\(from, to) ns ->
                    M.adjust (\n -> n { rnTransitions = to : rnTransitions n }) from ns
                    ) nodes transitions
            in updated
            ) updatedNodes transitions
        
    return $ srm
        { srmNodes = updatedWithTransitions
        , srmLastUpdated = currentTime
        }

-- | Find responses that might resonate with the current emotional state
matchUserState :: EmotionalState -> SharedResonanceMap -> [SuggestedResponse]
matchUserState state srm =
    let -- Find the most similar node
        bestNode = findBestMatchingNode state (srmNodes srm)
        
        -- Get responses that worked for similar states
        responseIds = maybe [] rnBreakthroughs bestNode
        responses = mapMaybe (`M.lookup` srmResponses srm) responseIds
        
        -- If no specific responses, fall back to default responses
        fallbackResponses = filter (\r -> 
            srEmotionalTone r `elem` ["neutral", "supportive"]) 
            (M.elems $ srmResponses srm)
        
    in if null responses then take 3 fallbackResponses else take 3 responses

-- | Find a path from current state to a more positive state
findResonantPath :: EmotionalState -> SharedResonanceMap -> Maybe [ResonanceNode]
findResonantPath state srm =
    let startNode = findBestMatchingNode state (srmNodes srm)
        positiveNodes = filter (\n -> 
            let scores = M.elems (rnEmotionalFingerprint n)
                positiveScore = fromMaybe 0 (M.lookup "радость" (rnEmotionalFingerprint n))
                    + fromMaybe 0 (M.lookup "доверие" (rnEmotionalFingerprint n))
                negativeScore = fromMaybe 0 (M.lookup "страх" (rnEmotionalFingerprint n))
                    + fromMaybe 0 (M.lookup "гнев" (rnEmotionalFingerprint n))
            in not (null scores) && positiveScore > negativeScore
            ) (M.elems $ srmNodes srm)
        
        findPath currentId targetId visited = 
            if currentId == rnId targetId
            then Just [targetId]
            else case M.lookup currentId (srmNodes srm) of
                Nothing -> Nothing
                Just node ->
                    let nextSteps = filter (`notElem` visited) (rnTransitions node)
                        paths = mapMaybe (\next -> findPath next targetId (currentId:visited)) nextSteps
                    in case paths of
                        [] -> Nothing
                        (p:ps) -> Just (currentId : p)
        
    in case (startNode, listToMaybe positiveNodes) of
        (Just start, Just target) -> 
            let path = findPath (rnId start) target []
            in fmap (mapMaybe (`M.lookup` srmNodes srm)) path
        _ -> Nothing

-- | Save the resonance map to a file
saveMap :: FilePath -> SharedResonanceMap -> IO ()
saveMap path srm = do
    createDirectoryIfMissing True (takeDirectory path)
    BL.writeFile path (encode srm)

-- | Load a resonance map from a file
loadMap :: FilePath -> IO (Maybe SharedResonanceMap)
loadMap path = do
    contents <- BL.readFile path
    return $ decode contents

-- | Measure how well two emotional states resonate
measureResonance :: EmotionalState -> ResonanceNode -> Double
measureResonance state node =
    let stateScores = esEmotions state
        nodeScores = rnEmotionalFingerprint node
        
        -- Calculate weighted cosine similarity
        dotProduct = sum $ M.intersectionWithKey (\k a b -> 
            a * b * M.findWithDefault 1.0 k (srmEmotionWeights undefined)  -- Weights from the map
            ) stateScores nodeScores
        
        magnitude a = sqrt . sum . M.map (^2) $ a
        
    in if M.null stateScores || M.null nodeScores
        then 0.0
        else dotProduct / (magnitude stateScores * magnitude nodeScores)

-- | Find states similar to the current one
findSimilarStates :: EmotionalState -> SharedResonanceMap -> [(ResonanceNode, Double)]
findSimilarStates state srm =
    let similarities = map (\node -> (node, measureResonance state node)) 
                        (M.elems $ srmNodes srm)
    in take 5 $ sortOn (Down . snd) similarities

-- | Default responses that can be used when no learned responses are available
defaultResponses :: [SuggestedResponse]
defaultResponses =
    [ SuggestedResponse "resp_comfort_1" "Я слышу, как тебе тяжело. Хочешь рассказать об этом подробнее?" "comforting" 0.8 Nothing
    , SuggestedResponse "resp_reflect_1" "Что для тебя самое сложное в этой ситуации?" "reflective" 0.7 Nothing
    , SuggestedResponse "resp_validate_1" "Это действительно сложная ситуация. Любой бы чувствовал то же самое." "validating" 0.9 Nothing
    , SuggestedResponse "resp_challenge_1" "А если бы твой друг был в такой ситуации, что бы ты ему посоветовал?" "challenging" 0.6 Nothing
    , SuggestedResponse "resp_ground_1" "Давай сделаем глубокий вдох. Что ты чувствуешь прямо сейчас в теле?" "grounding" 0.85 Nothing
    ]

-- Helper functions
clusterSimilarStates :: [EmotionalState] -> [[EmotionalState]]
clusterSimilarStates states = 
    let threshold = 0.7  -- Similarity threshold for clustering
        fingerprint s = M.map (\v -> if v > 0.3 then 1.0 else 0.0) (esEmotions s)
        
        similarity a b =
            let a' = fingerprint a
                b' = fingerprint b
                intersection = M.intersectionWith (\x y -> if x > 0 && y > 0 then 1 else 0) a' b'
            in fromIntegral (M.size (M.filter (==1) intersection)) / fromIntegral (max 1 (M.size a'))
        
        clusterWith :: EmotionalState -> [EmotionalState] -> ([EmotionalState], [EmotionalState])
        clusterWith _ [] = ([], [])
        clusterWith x (y:ys)
            | similarity x y >= threshold =
                let (inCluster, outCluster) = clusterWith x ys
                in (y:inCluster, outCluster)
            | otherwise =
                let (inCluster, outCluster) = clusterWith x ys
                in (inCluster, y:outCluster)
        
        clusterStep [] = []
        clusterStep (x:xs) =
            let (inCluster, outCluster) = clusterWith x xs
            in (x:inCluster) : clusterStep outCluster
            
    in clusterStep states

createEmotionalFingerprint :: [EmotionalState] -> Map Text Double
createEmotionalFingerprint states =
    let allEmotions = concatMap (M.keys . esEmotions) states
        avgScore emotion = 
            let scores = mapMaybe (M.lookup emotion . esEmotions) states
            in if null scores then 0 else sum scores / fromIntegral (length scores)
    in M.fromList $ map (\e -> (e, avgScore e)) (nub allEmotions)

extractCommonPhrases :: [EmotionalState] -> [Text]
extractCommonPhrases states =
    let phrases = concatMap (T.words . esText) states
        phrasePairs = zip phrases (drop 1 phrases)
        phraseTriplets = zip3 phrases (drop 1 phrases) (drop 2 phrases)
        
        countOccurrences n = map (\g -> (head g, length g)) 
                           . groupBy (==) 
                           . sort
                           $ n
        
        singleWords = filter ((>2) . snd) $ countOccurrences phrases
        pairs = filter ((>1) . snd) $ countOccurrences phrasePairs
        triplets = filter ((>1) . snd) $ countOccurrences phraseTriplets
        
        formatPair (a,b) = a <> " " <> b
        formatTriplet (a,b,c) = a <> " " <> b <> " " <> c
        
    in take 5 $ map fst singleWords ++ map (formatPair . fst) pairs ++ map (formatTriplet . fst) triplets

extractCommonCues :: [EmotionalState] -> [CueType]
extractCommonCues = 
    nub . concatMap (map cueType . esLinguisticCues)

findBestMatchingNode :: EmotionalState -> Map Text ResonanceNode -> Maybe ResonanceNode
findBestMatchingNode state nodes =
    let similarities = map (\(id, node) -> (node, measureResonance state node)) (M.assocs nodes)
        bestMatch = maximumBy (\(_,a) (_,b) -> compare a b) similarities
    in if snd bestMatch > 0.5 then Just (fst bestMatch) else Nothing

hashFingerprint :: Map Text Double -> InthashFingerprint = 
    abs $ foldl (\h (k,v) -> h * 31 + hash k + floor (v * 100)) 
                (0x45d9f3b) 
                (M.assocs fingerprint)

-- | Simple hash function for Text
hash :: Text -> Int
hash = T.foldl' (\h c -> h * 31 + fromEnum c) 0

-- | Convert NonEmpty to list
toList :: NonEmpty a -> [a]
toList (x :| xs) = x : xs

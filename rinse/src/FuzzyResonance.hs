{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module FuzzyResonance (
    -- * Core Types
    FuzzyScore,
    EmotionSimilarity,
    TextualResonance(..),
    
    -- * Emotion Matching
    fuzzyMatchEmotions,
    emotionSimilarityMatrix,
    
    -- * Textual Analysis
    calculateTextSimilarity,
    extractEmotionalPatterns,
    
    -- * Memory Relevance
    calculateMemoryRelevance,
    findResonantMemories,
    
    -- * Integration Helpers
    rankMemoriesForAgent,
    createEmotionalEcho
) where

import MemoryWhispers
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Text (Text, toLower, words, isInfixOf)
import Data.List (sortOn, groupBy, maximumBy, nub, (\))
import Data.Ord (Down(..))
import Data.Char (isPunctuation, isSpace)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Ratio ((%))
import Data.Function (on)
import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import System.Random (randomRIO)
import qualified Data.Text as T
import Data.List.NonEmpty (NonEmpty(..), toList)
import qualified Data.List.NonEmpty as NE
import Data.Time.Clock (UTCTime, diffUTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Calendar (Day(..), fromGregorian)
import Data.Time.Calendar.OrdinalDate (sundayStartWeek)

-- | A score from 0.0 to 1.0 representing fuzzy matching strength
type FuzzyScore = Double

-- | A mapping of emotion pairs to their similarity score
type EmotionSimilarity = Map (Emotion, Emotion) FuzzyScore

-- | Represents textual resonance between two pieces of text
data TextualResonance = TextualResonance
    { trLexicalSimilarity :: FuzzyScore
    , trEmotionalPatterns :: [Text]
    , trTemporalProximity :: FuzzyScore
    } deriving (Show, Eq)

-- | Core emotion relationships (one direction, will be made symmetric)
coreEmotionRelationships :: [((Emotion, Emotion), FuzzyScore)]
coreEmotionRelationships =
    [ -- Joy connections
      ((Joy, Love), 0.85)
    , ((Joy, Inspiration), 0.75)
    , ((Joy, Gratitude), 0.8)
    , ((Joy, Serenity), 0.8)
    , ((Joy, Awe), 0.7)
    , ((Joy, Surprise), 0.6)
    , ((Joy, Anticipation), 0.5)
    , ((Joy, Trust), 0.4)
    , ((Joy, Fear), 0.1)
    , ((Joy, Sadness), 0.05)
    , ((Joy, Anger), 0.0)
    
    -- Sadness connections
    , ((Sadness, Melancholy), 0.9)
    , ((Sadness, Longing), 0.8)
    , ((Sadness, Guilt), 0.7)
    , ((Sadness, Fear), 0.5)
    , ((Sadness, Anger), 0.5)
    , ((Sadness, Trust), 0.3)
    , ((Sadness, Forgiveness), 0.6)
    
    -- Fear connections
    , ((Fear, Anxiety), 0.9)
    , ((Fear, Anger), 0.6)
    , ((Fear, Surprise), 0.5)
    , ((Fear, Trust), 0.2)
    
    -- Love connections
    , ((Love, Longing), 0.8)
    , ((Love, Trust), 0.85)
    , ((Love, Awe), 0.7)
    , ((Love, Gratitude), 0.75)
    
    -- Anger connections
    , ((Anger, Clarity), 0.4)
    , ((Anger, Surprise), 0.4)
    , ((Anger, Anticipation), 0.3)
    
    -- Other meaningful connections
    , ((Inspiration, Determination), 0.7)
    , ((Confusion, Insight), 0.65)
    , ((Loneliness, Vulnerability), 0.85)
    , ((Vulnerability, Trust), 0.6)
    , ((Guilt, Forgiveness), 0.55)
    , ((Longing, Sadness), 0.6)
    , ((Trust, Love), 0.75)
    
    -- Add more core relationships as needed
    ]

-- | All basic emotions that should have self-similarity 1.0
allBasicEmotions :: [Emotion]
allBasicEmotions =
    [ Joy, Sadness, Fear, Anger, Trust, Anticipation, Surprise, Love, Longing
    , Awe, Melancholy, Serenity, Yearning, Peaceful, Inspiration, Gratitude
    , Guilt, Forgiveness, Anxiety, Determination, Confusion, Insight
    , Loneliness, Vulnerability, Clarity, OtherEmotion ""
    ]

-- | Generate symmetric similarity matrix from core relationships
emotionSimilarityMatrix :: EmotionSimilarity
emotionSimilarityMatrix = M.fromList $ concat
    [ -- 1. Self-similarity for all emotions
      [ ((e, e), 1.0) | e <- allBasicEmotions ]
      
      -- 2. Core relationships (both directions)
    , [ (pair, score) | (pair@(e1, e2), score) <- coreEmotionRelationships ]
    , [ ((e2, e1), score) | ((e1, e2), score) <- coreEmotionRelationships, e1 /= e2 ]
      
      -- 3. Default low similarity for unrelated pairs
    , [ ((e1, e2), 0.3) | e1 <- allBasicEmotions, e2 <- allBasicEmotions
                        , e1 /= e2
                        , not (M.member (e1, e2) (M.fromList coreEmotionRelationships))
                        , not (M.member (e2, e1) (M.fromList coreEmotionRelationships)) ]
    ]

-- | Poetic emotional tags for memory classification
poeticTags :: [Text]
poeticTags =
    [ "поиск опоры"
    , "внутренний свет"
    , "стыд, но нежно"
    , "тихая радость"
    , "лёгкая грусть"
    , "вспышка гнева"
    , "тёплые воспоминания"
    , "холодный страх"
    , "ожидание чуда"
    , "принятие себя"
    , "нежность к миру"
    , "одиночество в толпе"
    , "светлая печаль"
    , "лёгкое волнение"
    , "глубокий покой"
    ]

-- | Get a random poetic tag
randomPoeticTag :: IO Text
randomPoeticTag = do
    idx <- randomRIO (0, length poeticTags - 1)
    return $ poeticTags !! idx

-- | Get tags that match current emotion and tone
suggestPoeticTags :: Emotion -> MoodTone -> IO [Text]
suggestPoeticTags emotion tone = do
    -- In a real implementation, this would use semantic similarity
    -- For now, we'll just return random tags with some filtering
    let emotionTags = case emotion of
            Joy -> ["тихая радость", "тёплые воспоминания", "нежность к миру"]
            Sadness -> ["лёгкая грусть", "светлая печаль", "одиночество в толпе"]
            Fear -> ["холодный страх", "лёгкое волнение"]
            _ -> []
            
        toneTags = case tone of
            Tender -> ["нежность к миру", "тёплые воспоминания"]
            Melancholic -> ["светлая печаль", "лёгкая грусть"]
            _ -> []
            
    -- Combine and deduplicate
    allTags = nub $ emotionTags ++ toneTags
    
    -- Ensure we have at least 3 tags
    if length allTags >= 3 
        then return $ take 3 allTags
        else do
            extraNeeded = 3 - length allTags
            extra <- replicateM extraNeeded randomPoeticTag
            return $ allTags ++ extra

-- | Calculate fuzzy match between two emotions
fuzzyMatchEmotions :: Emotion -> Emotion -> FuzzyScore
fuzzyMatchEmotions e1 e2
    | e1 == e2 = 1.0  -- Perfect match
    | otherwise = fromMaybe 0.3 $ do  -- Default to 0.3 for unknown pairs
        lookup1 <- M.lookup (e1, e2) emotionSimilarityMatrix
        lookup2 <- M.lookup (e2, e1) emotionSimilarityMatrix
        return $ max lookup1 lookup2

-- | Calculate text similarity between two phrases
calculateTextSimilarity :: Text -> Text -> FuzzyScore
calculateTextSimilarity t1 t2
    | T.norm t1 == T.norm t2 = 1.0  -- Exact match
    | T.toLower t1 `T.isInfixOf` T.toLower t2 || T.toLower t2 `T.isInfixOf` T.toLower t1 = 0.8  -- One contains the other
    | otherwise = 0.3  -- Basic similarity for now (could be enhanced with NLP)

-- | Extract emotional patterns from text
extractEmotionalPatterns :: Text -> [Text]
extractEmotionalPatterns text =
    let words = T.words $ T.toLower text
        emotionalWords = filter (\w -> T.length w > 3) words  -- Simple heuristic
    in take 5 $ nub emotionalWords  -- Return up to 5 unique emotional words

-- | Calculate temporal proximity between two timestamps (0.0 to 1.0)
calculateTemporalProximity :: UTCTime -> UTCTime -> FuzzyScore
calculateTemporalProximity t1 t2 =
    let diff = abs $ diffUTCTime t1 t2
        -- Scale: 1.0 if same day, ~0.5 if same week, ~0.1 if same month, etc.
        days = realToFrac (diff / 86400) :: Double  -- Convert seconds to days
    in max 0 (1 - (days / 30))  -- Linear decay over 30 days

-- | Calculate overall relevance of a memory to current state
calculateMemoryRelevance 
    :: MemoryFragment  -- ^ Current state
    -> MemoryFragment  -- ^ Memory to evaluate
    -> FuzzyScore     -- ^ Relevance score (0.0 to 1.0)
calculateMemoryRelevance current memory =
    let -- Emotion similarity (0.0 to 1.0)
        emotionScore = fuzzyMatchEmotions (mwEmotion current) (mwEmotion memory)
        
        -- Text similarity (0.0 to 1.0)
        textScore = calculateTextSimilarity (mwPhrase current) (mwPhrase memory)
        
        -- Temporal proximity (0.0 to 1.0)
        timeScore = calculateTemporalProximity (mwTimestamp current) (mwTimestamp memory)
        
        -- Shared tags (0.0 to 1.0)
        sharedTags = filter (`elem` mwTags current) (mwTags memory)
        tagScore = if null (mwTags current) || null (mwTags memory)
                  then 0.5  -- Neutral score if either has no tags
                  else fromIntegral (length sharedTags) / fromIntegral (max 1 (min (length $ mwTags current) (length $ mwTags memory)))
        
        -- Weighted average of all factors
        weights = [ (emotionScore, 0.4)    -- Emotion is most important
                  , (textScore, 0.3)       -- Then text similarity
                  , (timeScore, 0.2)       -- Then recency
                  , (tagScore, 0.1)        -- Then tags
                  ]
    in sum [score * weight | (score, weight) <- weights] / sum (map snd weights)

-- | Suggest memories that could be shared based on emotional state
suggestSharedMemory 
    :: StoryStream      -- ^ Agent's story context
    -> Emotion          -- ^ Target emotion to match
    -> Double           -- ^ Minimum relevance threshold
    -> IO [MemoryFragment]  -- ^ Suggested memories for sharing
suggestSharedMemory story targetEmotion minRelevance = do
    -- Get all memories that match the emotion above threshold
    let relevantMemories = filter (\m -> 
            let emotionScore = fuzzyMatchEmotions targetEmotion (mwEmotion m)
            in emotionScore >= minRelevance
            ) (ssFragments story)
    
    -- Sort by emotional intensity and recency
    currentTime <- getCurrentTime
    let scoredMemories = map (\m -> 
            let recency = 1 / (1 + realToFrac (diffUTCTime currentTime (mwTimestamp m) / 86400))
                score = mwIntensity m * 0.7 + recency * 0.3
            in (score, m)
            ) relevantMemories
            
    -- Return top 3 most relevant memories
    return $ map snd $ take 3 $ sortOn (Down . fst) scoredMemories

-- | Whisper a memory to another agent
whisperTo 
    :: AgentId           -- ^ Sender agent ID
    -> AgentId           -- ^ Recipient agent ID
    -> MemoryFragment    -- ^ Memory to share
    -> StoryStream       -- ^ Sender's story
    -> IO (MemoryFragment, MemoryFragment)  -- ^ (Echoed memory, Received memory)
whisperTo fromAgent toAgent memory senderStory = do
    currentTime <- getCurrentTime
    
    -- Create an echoed version for the sender
    let echoedMemory = memory 
            { mwShared = True
            , mwRecipientHint = Just toAgent
            , mwTags = "shared" : mwTags memory
            }
    
    -- Create a received version for the recipient with adapted tone
    receivedMemory <- createEmotionalEcho memory memory senderStory
    let adaptedMemory = receivedMemory
            { mwShared = False
            , mwRecipientHint = Just fromAgent
            , mwTags = "received" : mwTags memory
            , mwTimestamp = currentTime
            }
    
    return (echoedMemory, adaptedMemory)

-- | Find memories that resonate with current state
findResonantMemories 
    :: MemoryFragment    -- ^ Current state
    -> [MemoryFragment]  -- ^ All memories to search
    -> Int              -- ^ Maximum number of results
    -> Double           -- ^ Minimum relevance threshold (0.0 to 1.0)
    -> IO [MemoryFragment]  -- ^ Most resonant memories with echoes
findResonantMemories current memories n minRelevance = do
    -- Calculate relevance for all memories
    let withRelevance = map (\m -> (calculateMemoryRelevance current m, m)) memories
        
        -- Filter by minimum relevance and sort
        relevant = sortOn (Down . fst) $ filter ((>= minRelevance) . fst) withRelevance
        
        -- Take top N
        topN = take n relevant
        
    -- For each relevant memory, potentially create an echo
    echoedMemories <- mapM (\(score, mem) -> do
        -- Higher relevance = higher chance of creating an echo
        shouldEcho <- randomRIO (0, 1) < (score * 0.8)  -- Max 80% chance
        if shouldEcho
            then do
                -- Create a story context for the echo
                let storyCtx = StoryStream
                        { ssAgentId = fromMaybe "unknown" (mwRecipientHint current)
                        , ssFragments = [mem]
                        , ssCurrentTone = if score > 0.7 then Tender else Reflective
                        , ssSharedWith = []
                        }
                -- Create and return the echo
                echo <- createEmotionalEcho mem current storyCtx
                return [mem, echo]  -- Return both original and echo
            else return [mem]  -- Return just the original
        ) topN
        
    -- Flatten the list and take up to n memories
    return $ take n $ concat echoedMemories

-- | Rank memories for an agent based on current state and story context
rankMemoriesForAgent 
    :: MemoryFragment    -- ^ Current state
    -> StoryStream      -- ^ Current story context
    -> [MemoryFragment]  -- ^ Agent's memories
    -> IO [(MemoryFragment, FuzzyScore, [Text])]  -- ^ Memories with relevance scores and suggested tags
rankMemoriesForAgent current story memories = do
    -- Calculate base relevance for all memories
    let withRelevance = map (\m -> 
            let relevance = calculateMemoryRelevance current m
            in (m, relevance)
            ) memories
        
        -- Sort by relevance
        sorted = sortOn (Down . snd) withRelevance
        
    -- Add suggested tags based on emotion and story tone
    withTags <- mapM (\(mem, score) -> do
        tags <- suggestPoeticTags (mwEmotion mem) (ssCurrentTone story)
        return (mem, score, take 3 tags)  -- Limit to 3 tags
        ) sorted
        
    return withTags

-- | Adjust tone of a phrase based on target emotion and mood
adjustTone :: Text -> Emotion -> MoodTone -> IO Text
adjustTone phrase targetEmotion targetTone = do
    -- In a real implementation, this would use NLP to adjust the tone
    -- For now, we'll just add a prefix/suffix based on emotion/tone
    let emotionPrefix = case targetEmotion of
            Joy -> "С лёгкостью вспоминаю, как "
            Sadness -> "С лёгкой грустью вспоминаю, как "
            Fear -> "С трепетом вспоминаю, как "
            Anger -> "С жаром вспоминаю, как "
            Love -> "С любовью вспоминаю, как "
            _ -> "Вспоминаю, как "
            
        toneSuffix = case targetTone of
            Tender -> "... и в этом была особая нежность."
            Melancholic -> "... и в этом была своя печальная красота."
            Playful -> "... и как же это было забавно!"
            Intimate -> "... это останется между нами."
            _ -> "... это было особенное мгновение."
            
        -- Ensure first letter is lowercase for smooth concatenation
        adjustedPhrase = case T.uncons phrase of
            Just (firstChar, rest) -> T.cons (toLower firstChar) rest
            Nothing -> phrase
            
    -- 50% chance to add prefix, 30% suffix, 20% both
    choice <- randomRIO (1, 10)
    return $ case () of
        _ | choice <= 5 -> T.concat [emotionPrefix, adjustedPhrase]
          | choice <= 8 -> T.concat [phrase, " ", toneSuffix]
          | otherwise   -> T.concat [emotionPrefix, adjustedPhrase, " ", toneSuffix]

-- | Create an emotional echo - a modified memory fragment that resonates with current state
createEmotionalEcho 
    :: MemoryFragment  -- ^ Original memory
    -> MemoryFragment  -- ^ Current state
    -> StoryStream    -- ^ Current story context
    -> IO MemoryFragment  -- ^ Echoed memory
createEmotionalEcho memory current story = do
    -- Add some randomness to make it feel more organic
    intensityVariance <- randomRIO (0.8, 1.2)
    
    -- Determine target emotion (blend of current and memory emotions)
    let emotionBlend = if mwIntensity current > 0.7 
                      then 0.7  -- Strong current emotion dominates
                      else 0.3  -- More balanced blend
        
        -- Blend emotions based on intensity
        targetEmotion = if randomRIO (0, 1) < 0.7  -- 70% chance to blend
                       then mwEmotion current  -- Current emotion
                       else mwEmotion memory   -- Original emotion
    
    -- Adjust the phrase to match target emotion and story tone
    adjustedPhrase <- adjustTone (mwPhrase memory) targetEmotion (ssCurrentTone story)
    
    -- Suggest poetic tags based on emotion and tone
    suggestedTags <- suggestPoeticTags targetEmotion (ssCurrentTone story)
    
    -- Create the echoed memory
    currentTime <- getCurrentTime
    
    return MemoryFragment
        { mwTimestamp = currentTime
        , mwEmotion = targetEmotion
        , mwPhrase = adjustedPhrase
        , mwShared = True  -- Echoed memories are always shared
        , mwRecipientHint = mwRecipientHint current
        , mwIntensity = min 1.0 (mwIntensity memory * intensityVariance)
        , mwTags = nub $ suggestedTags ++
                  [if mwIntensity memory > 0.7 then "интенсивно" else "лёгкий",
                   "эхо"]
        }

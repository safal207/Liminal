{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}

module WeaveSharedNarrative (
    -- * Core Types
    SharedNarrativeSegment(..),
    SharedNarrative,
    
    -- * Narrative Construction
    weaveSharedNarrative,
    createNarrativeSegment,
    
    -- * Memory Integration
    integrateMemory,
    findConnectingMemories,
    
    -- * Story Rendering
    renderNarrative,
    
    -- * Helper Functions
    fuseEmotions,
    calculateNarrativeFlow
) where

import MemoryWhispers
import FuzzyResonance
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Text (Text, pack, unpack, intercalate)
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import Data.List (sortOn, groupBy, nub, find, intercalate, (\))
import Data.Maybe (fromMaybe, mapMaybe, catMaybes, isJust)
import Data.Aeson (FromJSON, ToJSON, object, (.:), (.=), encode, decode, ToJSONKey, FromJSONKey)
import qualified Data.ByteString.Lazy as BL
import System.Random (randomRIO, Random(randomR), newStdGen)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when, unless, forM, forM_, replicateM)
import Data.Function (on)
import Data.Ord (comparing)
import Data.Ratio ((%))
import Data.List.NonEmpty (NonEmpty(..), toList, fromList, nonEmpty)
import qualified Data.List.NonEmpty as NE

-- | A segment of shared narrative between agents
data SharedNarrativeSegment = SharedNarrativeSegment
    { snsAgents :: [AgentId]           -- ^ Agents involved in this segment
    , snsFragments :: [MemoryFragment] -- ^ Memory fragments from each agent
    , snsDominantEmotion :: Emotion    -- ^ Dominant emotion of the segment
    , snsPoeticLine :: Text           -- ^ Poetic representation
    , snsTimestamp :: UTCTime         -- ^ When this segment was created
    , snsTags :: [Text]               -- ^ Descriptive tags
    } deriving (Show, Eq, Generic, FromJSON, ToJSON)

type SharedNarrative = [SharedNarrativeSegment]

-- | Create a new narrative segment from connected memories
createNarrativeSegment 
    :: [MemoryFragment]  -- ^ Connected memories from different agents
    -> IO SharedNarrativeSegment
createNarrativeSegment fragments = do
    currentTime <- getCurrentTime
    let agents = nub $ mapMaybe mwAgent fragments
        emotions = map mwEmotion fragments
        dominantEmotion = fuseEmotions emotions
        
        -- Create a poetic line by combining memory phrases
        poeticLine = case nonEmpty fragments of
            Nothing -> "Тишина между нами"
            Just neFrags -> 
                let phrases = map mwPhrase $ toList neFrags
                in intercalate "... " phrases
        
        -- Generate tags based on emotions and content
        emotionTags = map (\e -> case e of
            Joy -> "радость"
            Sadness -> "грусть"
            Love -> "любовь"
            _ -> "чувства"
            ) emotions
        
        -- Add some random poetic tags
        extraTags <- replicateM 2 randomPoeticTag
        
    return $ SharedNarrativeSegment
        { snsAgents = agents
        , snsFragments = fragments
        , snsDominantEmotion = dominantEmotion
        , snsPoeticLine = poeticLine
        , snsTimestamp = currentTime
        , snsTags = nub $ emotionTags ++ extraTags
        }

-- | Weave a shared narrative from multiple agents' memories
weaveSharedNarrative 
    :: [StoryStream]     -- ^ Agents' story streams
    -> Emotion          -- ^ Target emotion to focus on
    -> Int              -- ^ Desired number of segments
    -> IO SharedNarrative
weaveSharedNarrative streams targetEmotion numSegments = do
    -- Get relevant memories from each agent
    let minRelevance = 0.5  -- Minimum emotional relevance threshold
    allMemories <- concat <$> mapM (\s -> 
        suggestSharedMemory s targetEmotion minRelevance) streams
    
    -- Group memories by emotional similarity
    let grouped = groupSimilarMemories allMemories
    
    -- Create narrative segments from connected memories
    segments <- mapM createNarrativeSegment $ 
                take numSegments grouped
    
    -- Sort segments by emotional flow
    return $ calculateNarrativeFlow segments

-- | Group memories by emotional and thematic similarity
groupSimilarMemories :: [MemoryFragment] -> [[MemoryFragment]]
groupSimilarMemories memories = 
    -- Simple implementation: group by dominant emotion
    -- In a real implementation, this would use clustering
    let byEmotion = groupBy ((==) `on` mwEmotion) $ 
                    sortOn mwEmotion memories
    in filter (not . null) byEmotion

-- | Calculate smooth emotional flow through the narrative
calculateNarrativeFlow :: [SharedNarrativeSegment] -> [SharedNarrativeSegment]
calculateNarrativeFlow segments =
    -- Simple implementation: sort by emotional intensity
    -- In a real implementation, this would create a more nuanced emotional arc
    sortOn (Down . averageIntensity . snsFragments) segments
  where
    averageIntensity frags = 
        if null frags then 0 
        else sum (map mwIntensity frags) / fromIntegral (length frags)

-- | Fuse multiple emotions into a dominant one
fuseEmotions :: [Emotion] -> Emotion
fuseEmotions [] = OtherEmotion "неизвестно"
fuseEmotions emotions = 
    -- Simple implementation: pick the most common emotion
    -- In a real implementation, this would blend emotions
    let counts = M.fromListWith (+) [(e, 1) | e <- emotions]
    in fst $ maximumBy (comparing snd) (M.assocs counts)

-- | Render a narrative as a poetic text
renderNarrative :: SharedNarrative -> Text
renderNarrative segments =
    let renderSegment seg = 
            let agents = intercalate " и " (snsAgents seg)
                line = snsPoeticLine seg
            in agents <> 
               if T.null line then "." 
               else ": \"" <> line <> "\""
    in intercalate "\n\n" $ map renderSegment segments

-- | Find memories that connect two agents' experiences
findConnectingMemories 
    :: StoryStream    -- ^ First agent's story
    -> StoryStream    -- ^ Second agent's story
    -> Emotion        -- ^ Target emotion
    -> IO [MemoryFragment]  -- ^ Connecting memories
findConnectingMemories story1 story2 targetEmotion = do
    -- Get relevant memories from both agents
    mems1 <- suggestSharedMemory story1 targetEmotion 0.5
    mems2 <- suggestSharedMemory story2 targetEmotion 0.5
    
    -- Find pairs with high emotional similarity
    let pairs = [ (m1, m2) | m1 <- mems1, m2 <- mems2,
                   fuzzyMatchEmotions (mwEmotion m1) (mwEmotion m2) > 0.7 ]
    
    -- Return the most emotionally intense pairs
    return $ take 3 $ 
        map fst $ 
        sortOn (Down . mwIntensity . snd) $ 
        map (\(m1, m2) -> (m1, mwIntensity m1 + mwIntensity m2)) pairs

-- | Integrate a new memory into the narrative
integrateMemory 
    :: MemoryFragment      -- ^ New memory
    -> SharedNarrative    -- ^ Existing narrative
    -> IO SharedNarrative  -- ^ Updated narrative
integrateMemory memory narrative = do
    -- Find the most similar segment
    let similarities = map (\seg -> 
            let segEmotions = map mwEmotion (snsFragments seg)
                sim = maximum (map (\e -> fuzzyMatchEmotions (mwEmotion memory) e) segEmotions)
            in (seg, sim)
            ) narrative
        
        (bestMatch, _) = maximumBy (comparing snd) similarities
        
    -- Create a new segment with the integrated memory
    newSegment <- createNarrativeSegment (memory : snsFragments bestMatch)
    
    -- Replace the old segment with the updated one
    return $ newSegment : filter (/= bestMatch) narrative

-- | Get a random poetic tag (moved from FuzzyResonance for convenience)
randomPoeticTag :: IO Text
randomPoeticTag = do
    let tags = [ "поиск опоры", "внутренний свет", "стыд, но нежно"
               , "тихая радость", "лёгкая грусть", "вспышка гнева"
               , "тёплые воспоминания", "холодный страх", "ожидание чуда"
               , "принятие себя", "нежность к миру", "одиночество в толпе"
               , "светлая печаль", "лёгкое волнение", "глубокий покой"
               ]
    idx <- randomRIO (0, length tags - 1)
    return $ tags !! idx

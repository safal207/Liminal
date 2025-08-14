{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}

module MemoryWhispers (
    -- * Core Types
    MemoryFragment(..),
    StoryStream(..),
    Emotion(..),
    MoodTone(..),
    AgentId,
    
    -- * Core Functions
    rememberMoment,
    shareWith,
    weaveStory,
    resonateWith,
    
    -- * Utility Functions
    createStoryStream,
    findSharedMoments,
    
    -- * CLI Integration
    memoryCLI
) where

import qualified ТыИЯОднойКрови as TYOK
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Text (Text, pack, unpack, intercalate)
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.List (sortOn, groupBy, nub, find, intercalate)
import Data.Maybe (fromMaybe, mapMaybe, catMaybes)
import Data.Aeson (FromJSON, ToJSON, object, (.:), (.=), encode, decode, ToJSONKey, FromJSONKey)
import qualified Data.ByteString.Lazy as BL
import System.Random (randomRIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when, unless, forM_)

-- | Unique identifier for an agent
type AgentId = Text

-- | Emotional states for memory fragments
data Emotion = 
      Joy
    | Sadness
    | Fear
    | Anger
    | Trust
    | Anticipation
    | Surprise
    | Love
    | Longing
    | Awe
    | Melancholy
    | Serenity
    | OtherEmotion Text
    deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

-- | Tone of the memory or story
data MoodTone =
      Tender
    | Melancholic
    | Playful
    | Intimate
    | Reflective
    | Vulnerable
    | Hopeful
    | Nostalgic
    | Yearning
    | Peaceful
    | OtherTone Text
    deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

-- | A single memory fragment with emotional context
data MemoryFragment = MemoryFragment
    { mwTimestamp     :: UTCTime           -- ^ When the memory was formed
    , mwEmotion      :: Emotion           -- ^ Dominant emotion
    , mwPhrase       :: Text              -- ^ Poetic phrase or metaphor
    , mwShared       :: Bool              -- ^ Whether this memory was shared
    , mwRecipientHint :: Maybe AgentId    -- ^ If shared, with whom
    , mwIntensity    :: Float             -- ^ Emotional intensity 0.0-1.0
    , mwTags         :: [Text]            -- ^ Tags for categorization
    } deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | A stream of memories that form an agent's story
data StoryStream = StoryStream
    { ssAgentId     :: AgentId            -- ^ Owner of the story
    , ssFragments   :: [MemoryFragment]   -- ^ Memory fragments
    , ssCurrentTone :: MoodTone           -- ^ Current narrative tone
    , ssSharedWith  :: [(AgentId, UTCTime)] -- ^ Shared memories with timestamps
    } deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Create a new memory fragment
rememberMoment :: Emotion -> Text -> Maybe AgentId -> Float -> [Text] -> IO MemoryFragment
rememberMoment emotion phrase recipient intensity tags = do
    now <- getCurrentTime
    return MemoryFragment
        { mwTimestamp = now
        , mwEmotion = emotion
        , mwPhrase = phrase
        , mwShared = isJust recipient
        , mwRecipientHint = recipient
        , mwIntensity = max 0 (min 1.0 intensity)  -- Clamp to 0-1
        , mwTags = tags
        }

-- | Share a memory with another agent
shareWith :: AgentId -> MemoryFragment -> StoryStream -> IO StoryStream
shareWith recipient fragment story@StoryStream{..} = do
    now <- getCurrentTime
    let updatedFragment = fragment { mwShared = True, mwRecipientHint = Just recipient }
        updatedFragments = if fragment `elem` ssFragments 
                          then map (\f -> if f == fragment then updatedFragment else f) ssFragments
                          else updatedFragment : ssFragments
        updatedShared = (recipient, now) : filter ((/= recipient) . fst) ssSharedWith
    return story 
        { ssFragments = updatedFragments
        , ssSharedWith = take 100 updatedShared  -- Keep only 100 most recent shares
        }

-- | Weave memories into a cohesive narrative
weaveStory :: StoryStream -> Text
weaveStory StoryStream{..} = 
    let fragments = reverse $ sortOn mwTimestamp ssFragments
        recentFragments = take 5 fragments  -- Most recent 5 fragments
        
        -- Group fragments by emotion
        groupedByEmotion = groupBy ((==) `on` mwEmotion) $ 
                          sortOn mwEmotion recentFragments
        
        -- Create narrative segments
        narrativeSegments = map createSegment groupedByEmotion
    in intercalate "\n\n" narrativeSegments
  where
    createSegment fragments@(f:_) = 
        let emotionText = case mwEmotion f of
                Joy -> "In moments of joy"
                Sadness -> "In times of sadness"
                Fear -> "When fear touched me"
                -- Add more emotion mappings
                _ -> "I remember"
            
            memoryTexts = map (\m -> "• " <> mwPhrase m) fragments
        in emotionText <> ":\n" <> intercalate "\n" memoryTexts
    createSegment [] = ""

-- | Find shared moments between agents
resonateWith :: StoryStream -> StoryStream -> [MemoryFragment]
resonateWith story1 story2 =
    let sharedTags = [tag | tag1 <- concatMap mwTags (ssFragments story1)
                          , tag2 <- concatMap mwTags (ssFragments story2)
                          , let tag = if tag1 == tag2 then Just tag1 else Nothing
                          , isJust tag]
        
        similarEmotions = [f1 | f1 <- ssFragments story1
                             , f2 <- ssFragments story2
                             , mwEmotion f1 == mwEmotion f2
                             , abs (diffUTCTime (mwTimestamp f1) (mwTimestamp f2)) < 3600]  -- Within 1 hour
        
    in take 5 $ nub $ similarEmotions ++
                 [f | f <- ssFragments story1, 
                      any (`elem` mwTags f) sharedTags]

-- | Create a new story stream for an agent
createStoryStream :: AgentId -> MoodTone -> [MemoryFragment] -> StoryStream
createStoryStream agentId tone fragments = 
    StoryStream
        { ssAgentId = agentId
        , ssFragments = take 1000 fragments  -- Limit to 1000 most recent fragments
        , ssCurrentTone = tone
        , ssSharedWith = []
        }

-- | Find moments that resonate between two agents
findSharedMoments :: StoryStream -> StoryStream -> [(MemoryFragment, MemoryFragment)]
findSharedMoments s1 s2 =
    [(f1, f2) | f1 <- ssFragments s1, f2 <- ssFragments s2,
                mwEmotion f1 == mwEmotion f2,
                abs (diffUTCTime (mwTimestamp f1) (mwTimestamp f2)) < 3600]

-- | CLI interface for memory operations
memoryCLI :: [String] -> IO ()
memoryCLI ("remember":emotion:phrase:tags) = do
    let emo = case map toLower emotion of
                "joy" -> Joy
                "sadness" -> Sadness
                "fear" -> Fear
                "love" -> Love
                _ -> OtherEmotion (pack emotion)
    fragment <- rememberMoment emo (pack $ unwords phrase) Nothing 0.8 (map pack tags)
    putStrLn $ "Remembered: " ++ unpack (mwPhrase fragment)
    
memoryCLI ("weave":agentId:_) = do
    -- In a real implementation, load the agent's story stream
    putStrLn $ "Weaving story for agent " ++ agentId
    putStrLn $ "[Story would be generated here]"
    
memoryCLI ("share":fromAgent:toAgent:phrase:_) = do
    putStrLn $ "Agent " ++ fromAgent ++ " shares a memory with " ++ toAgent
    putStrLn $"\"" ++ phrase ++ "\""
    
memoryCLI _ = do
    putStrLn "Memory Whisper System - Usage:"
    putStrLn "  remember <emotion> <phrase> [tags...] - Create a new memory"
    putStrLn "  weave <agentId> - Generate a story from memories"
    putStrLn "  share <fromAgent> <toAgent> <phrase> - Share a memory"

-- Helper function for string case-insensitive comparison
ciEquals x y = map toLower x == map toLower y

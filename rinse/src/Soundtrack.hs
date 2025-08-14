{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
module Soundtrack where

import Data.Text (Text, pack, unpack, intercalate)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as M
import Data.Aeson (ToJSON, FromJSON, object, (.=), (.:), (.:?))
import GHC.Generics (Generic)
import Data.List (sortOn, groupBy, maximumBy)
import Data.Ord (comparing)
import Data.Ratio ((%))

-- | Represents a musical note or sound element
data SoundElement = SoundElement
    { seName :: Text         -- Human-readable name (e.g., "low hum")
    , seTone :: Text         -- Symbolic representation (e.g., "A4")
    , seDuration :: Int      -- Relative duration (1 = whole note, 4 = quarter note)
    , seVolume :: Int        -- 0-127
    , seTimbre :: Text       -- Instrument/sound quality
    } deriving (Show, Eq, Generic)

-- | Represents a complete soundtrack
data Soundtrack = Soundtrack
    { stElements :: [SoundElement]  -- Sequence of sounds
    , stTempo :: Tempo             -- Overall tempo
    , stMode :: Mode               -- Musical mode
    , stKey :: Text                -- Musical key
    , stDescription :: Text        -- Human-readable description
    } deriving (Show, Eq, Generic)

-- | Musical tempo
data Tempo
    = Grave        -- Very slow and solemn
    | Largo        -- Broadly
    | Adagio       -- Slow and stately
    | Andante      -- Walking pace
    | Moderato     -- Moderately
    | Allegro      -- Fast, quickly
    | Presto       -- Very fast
    deriving (Show, Eq, Ord, Enum, Bounded, Generic)

-- | Musical mode
data Mode = Major | Minor | Dorian | Phrygian | Lydian | Mixolydian | Locrian
    deriving (Show, Eq, Ord, Enum, Bounded, Generic)

-- | Emotion to sound mapping
emotionToSound :: Text -> Double -> SoundElement
emotionToSound emotion weight =
    let baseSound = M.findWithDefault defaultSound (T.toLower emotion) emotionSoundMap
        -- Adjust volume based on weight (0.0-1.0 -> 40-127)
        volume = 40 + round (weight * 87)
    in baseSound { seVolume = volume }
  where
    defaultSound = SoundElement "silence" "REST" 4 0 "silence"

-- | Base mapping of emotions to sound elements
emotionSoundMap :: Map Text SoundElement
emotionSoundMap = M.fromList
    [ ("страх",       SoundElement "low hum"          "D2" 2 80 "drone")
    , ("тревога",     SoundElement "trembling string" "F#3" 4 90 "string")
    , ("гнев",        SoundElement "sharp hit"       "A2" 1 110 "percussion")
    , ("грусть",      SoundElement "fading echo"     "D4" 8 70 "piano")
    , ("радость",     SoundElement "high bell"       "C5" 8 100 "bell")
    , ("удивление",   SoundElement "chime"           "E5" 4 95 "glockenspiel")
    , ("отвращение",  SoundElement "dissonance"      "G#2" 2 85 "bass")
    , ("доверие",     SoundElement "warm pad"        "A3" 4 75 "pad")
    , ("ожидание",    SoundElement "rising tone"     "E4" 4 85 "synth")
    , ("решимость",   SoundElement "drum beat"       "BassDrum" 2 100 "drums")
    , ("спокойствие", SoundElement "soft pad"        "G3" 8 60 "strings")
    , ("вдохновение", SoundElement "sparkle"         "C6" 16 90 "celesta")
    ]

-- | Determine tempo based on emotional intensity
determineTempo :: [Double] -> Tempo
determineTempo weights
    | null weights = Moderato
    | otherwise = case avgWeight of
        w | w > 0.8  -> Allegro
          | w > 0.6  -> Moderato
          | w > 0.4  -> Andante
          | w > 0.2  -> Adagio
          | otherwise -> Largo
  where
    avgWeight = sum weights / fromIntegral (length weights)

-- | Determine musical mode based on emotions
determineMode :: [Text] -> Mode
determineMode emotions
    | any (`elem` ["радость", "вдохновение", "доверие"]) emotions = Major
    | any (`elem` ["грусть", "одиночество", "тоска"]) emotions = Minor
    | any (`elem` ["страх", "тревога"]) emotions = Phrygian
    | any (`elem` ["решимость", "смелость"]) emotions = Mixolydian
    | otherwise = Dorian

-- | Generate a soundtrack for a sequence of emotions
generateSoundtrack :: [(Text, Double)] -> Soundtrack
generateSoundtrack emotionWeights =
    let (emotions, weights) = unzip emotionWeights
        elements = map (uncurry emotionToSound) emotionWeights
        tempo = determineTempo weights
        mode = determineMode emotions
        
        -- Simple algorithm to determine key based on most intense emotion
        key = case fst (maximumBy (comparing snd) emotionWeights) of
            "радость"     -> "C"
            "грусть"      -> "A"
            "гнев"        -> "D"
            "страх"       -> "E"
            "решимость"   -> "G"
            "вдохновение" -> "F"
            _             -> "C"
            
        description = case mode of
            Major -> "Uplifting and bright progression"
            Minor -> "Reflective and emotional journey"
            Dorian -> "Mysterious and evolving path"
            Phrygian -> "Tense and dramatic movement"
            Mixolydian -> "Strong and determined advance"
            _ -> "Unique emotional landscape"
            
    in Soundtrack
        { stElements = elements
        , stTempo = tempo
        , stMode = mode
        , stKey = key
        , stDescription = description
        }

-- | Convert a soundtrack to a human-readable description
describeSoundtrack :: Soundtrack -> Text
describeSoundtrack Soundtrack{..} =
    let elementsDesc = T.intercalate " → " $ map seName stElements
        modeDesc = case stMode of
            Major -> "major"
            Minor -> "minor"
            _ -> T.toLower (pack $ show stMode)
    in T.unlines
        [ "🎵 Soundtrack: " <> elementsDesc
        , "🎼 Key: " <> stKey <> " " <> modeDesc
        , "🎚  Tempo: " <> T.toLower (pack $ show stTempo)
        , "💭 " <> stDescription
        ]

-- | Generate a simple MIDI-like representation (for potential export)
soundtrackToMIDI :: Soundtrack -> Text
soundtrackToMIDI Soundtrack{..} =
    let header = unlines
            [ "MFile 1 1 480"  -- MIDI format 1, 1 track, 480 ticks per quarter note
            , "MTrk"
            , "0 Tempo " <> tshow (tempoToBPM stTempo)
            , "0 TimeSig 4/4 24 8"
            , "0 KeySig " <> stKey <> if stMode == Major then " maj" else " min"
            ]
        
        notes = zipWith toMIDI [0..] stElements
        
        toMIDI tick SoundElement{..} =
            let pitch = if seName == "silence" 
                       then "" 
                       else tshow tick <> " On ch=1 n=" <> seTone <> " v=" <> tshow seVolume <> "\n"
                offTick = tick + seDuration * 120  -- 120 ticks per quarter note
                off = if seName == "silence" 
                     then "" 
                     else tshow offTick <> " Off ch=1 n=" <> seTone <> " v=0\n"
            in pitch <> off
        
        tempoToBPM = \case
            Grave -> 40
            Largo -> 50
            Adagio -> 70
            Andante -> 90
            Moderato -> 110
            Allegro -> 140
            Presto -> 180
    
    in T.pack header <> T.unlines notes

-- Helper function
{-# INLINE tshow #-}
tshow :: Show a => a -> Text
tshow = pack . show

-- JSON instances
instance ToJSON SoundElement
instance FromJSON SoundElement
instance ToJSON Soundtrack
instance FromJSON Soundtrack
instance ToJSON Tempo
instance FromJSON Tempo
instance ToJSON Mode
instance FromJSON Mode

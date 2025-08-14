{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module EmotionLinguisticMap where

import Data.Text (Text, toLower, pack, unpack, isInfixOf, isSuffixOf, isPrefixOf)
import qualified Data.Text as T
import Data.Char (isPunctuation, isSpace, isUpper)
import Data.List (sortOn, groupBy, nub, find, isInfixOf)
import Data.Ord (Down(..))
import Data.Maybe (mapMaybe, catMaybes, listToMaybe, fromMaybe)
import Data.Aeson (ToJSON, FromJSON, Value(..), object, (.=), (.:), (.:?))
import GHC.Generics (Generic)
import Text.Regex.PCRE ((=~))
import qualified Data.Text.Encoding as TE
import Data.Ratio ((%))
import Data.List.NonEmpty (NonEmpty(..), nonEmpty, toList)
import qualified Data.List.NonEmpty as NE
import Data.Function (on)

-- | Type representing the intensity of a linguistic cue (0.0 to 1.0)
type CueIntensity = Double

-- | Types of linguistic cues that can be detected
data CueType
    = Fragmentation       -- Incomplete thoughts, abrupt endings
    | OverPunctuation     -- Excessive !!!, ???, ..., etc.
    | Negation           -- Negative self-talk
    | OverCorrection     -- Frequent edits, [sic], corrections
    | PowerPhrase       -- Empowering language
    | ResignedPhrase    -- Defeatist or passive language
    | Repetition        -- Repeated phrases or words
    | CognitiveLoad     -- Typos, errors in high-emotion sections
    | GrowthLanguage    -- Signs of reflection and growth
    | Urgency          -- Words indicating stress or pressure
    deriving (Show, Eq, Ord, Enum, Bounded, Generic)

-- | A detected linguistic cue with context
data LinguisticCue = LinguisticCue
    { cueType :: CueType
    , matchedText :: Text
    , intensity :: CueIntensity
    , context :: Maybe Text  -- Surrounding sentence or phrase
    } deriving (Show, Eq, Generic)

-- | Configuration for cue detection
data DetectionConfig = DetectionConfig
    { minContextWords :: Int
    , maxContextWords :: Int
    , minMatchLength :: Int
    , maxMatchesPerType :: Int
    } deriving (Show, Generic)

-- Default configuration
defaultConfig :: DetectionConfig
defaultConfig = DetectionConfig
    { minContextWords = 3
    , maxContextWords = 15
    , minMatchLength = 2
    , maxMatchesPerType = 5
    }

-- | Analyze text and return detected linguistic cues
analyzeLinguisticCues :: Text -> [LinguisticCue]
analyzeLinguisticCues text =
    let -- Split into sentences while preserving original text
        sentences = splitIntoSentences text
        
        -- Apply all detectors to each sentence
        allCues = concatMap (detectCuesInSentence sentences) sentences
        
        -- Group by cue type and take top N most intense per type
        grouped = groupBy ((==) `on` cueType) $ sortOn cueType allCues
        topCues = concatMap (take (maxMatchesPerType defaultConfig) . sortOn (Down . intensity)) grouped
        
    in topCues

-- | Split text into sentences while preserving original text
splitIntoSentences :: Text -> [Text]
splitIntoSentences = 
    filter (not . T.null) . T.split (flip elem ['.', '!', '?', '…', ';', ':'])

-- | Get context around a match
getContext :: Int -> Int -> [Text] -> Text -> Maybe Text
getContext minW maxW sentences match =
    let -- Find the sentence containing the match
        containing = find (T.isInfixOf match) sentences
        
        -- Get surrounding sentences if needed
        getWindow s = 
            let idx = fromMaybe 0 $ findIndex (== s) sentences
                start = max 0 (idx - 1)
                end = min (length sentences - 1) (idx + 1)
            in T.intercalate " " $ take (end - start + 1) (drop start sentences)
            
    in case containing of
        Nothing -> Nothing
        Just s  -> 
            let words = T.words s
                matchWords = T.words match
                matchStart = fromMaybe 0 $ findIndex (T.isInfixOf (head matchWords)) words
                start = max 0 (matchStart - minW)
                end = min (length words - 1) (matchStart + length matchWords + maxW)
                contextWords = take (end - start) (drop start words)
            in if length contextWords >= minW
               then Just $ T.unwords contextWords
               else getWindow s

-- | Detect all cue types in a sentence
detectCuesInSentence :: [Text] -> Text -> [LinguisticCue]
detectCuesInSentence sentences s =
    let lower = toLower s
        words = T.words s
        
        -- Punctuation patterns
        ellipsis = "…" `T.isInfixOf` s || "..." `T.isInfixOf` s
        multiExcl = T.count "!!" s > 1 || T.count "!?" s > 0
        multiQ = T.count "??" s > 0
        
        -- Emotional language patterns
        negativePhrases = 
            [ ("я не могу", 0.8)
            , ("я боюсь", 0.9)
            , ("я плох", 0.7)
            , ("я неудачник", 1.0)
            , ("всегда так", 0.6)
            , ("никогда не", 0.7)
            ]
            
        growthPhrases =
            [ ("я начинаю", 0.7)
            , ("я научусь", 0.9)
            , ("я смогу", 0.8)
            , ("я выбираю", 0.8)
            , ("я решаю", 0.7)
            ]
            
        -- Detect repetitions
        wordFreq = map (\g -> (head g, length g)) $ group $ sort $ filter (not . isCommonWord) words
        repetitions = filter ((>2) . snd) wordFreq
        
        -- Detect cognitive load (typos, mixed case, etc.)
        hasTypos = any (\w -> T.any isUpper (T.drop 1 w)) words
        mixedCase = any (/= toLower) s && any (/= toUpper) s
        
        -- Generate cues
        cues = 
            [ (Fragmentation, ellipsis, "...", 0.6)
            , (OverPunctuation, multiExcl, "!!", 0.7)
            , (OverPunctuation, multiQ, "??", 0.5)
            , (CognitiveLoad, hasTypos && length words > 5, s, 0.5)
            , (CognitiveLoad, mixedCase && length words > 3, s, 0.4)
            ]
            
        -- Add negative and growth phrases
        negCues = map (\(p, i) -> (Negation, p `T.isInfixOf` lower, p, i)) negativePhrases
        growthCues = map (\(p, i) -> (GrowthLanguage, p `T.isInfixOf` lower, p, i)) growthPhrases
        repCues = map (\(w, c) -> (Repetition, c > 2, w, min 1.0 (fromIntegral c * 0.3))) repetitions
        
        allCues = cues ++ negCues ++ growthCues ++ repCues
        
        -- Create LinguisticCue values
        toCue (cType, True, match, int) = 
            let ctx = getContext 3 5 sentences match
            in Just $ LinguisticCue cType match int ctx
        toCue _ = Nothing
            
    in catMaybes $ map toCue allCues

-- | Common words to ignore in repetition detection
isCommonWord :: Text -> Bool
isCommonWord w = lower `elem` commonWords
  where
    lower = toLower w
    commonWords = ["и", "в", "не", "на", "я", "что", "это", "а", "но", "у", "за", "по", "от", "из"]

-- | Summarize detected cues into a human-readable text
summarizeCues :: [LinguisticCue] -> Text
summarizeCues cues =
    if null cues
    then "No significant linguistic patterns detected."
    else T.intercalate "\n\n" $ map summarizeCueType grouped
  where
    grouped = groupBy ((==) `on` cueType) $ sortOn cueType cues
    
    summarizeCueType [] = ""
    summarizeCueType cs@(c:_) =
        let typeName = case cueType c of
                Fragmentation -> "Fragmented thoughts"
                OverPunctuation -> "Emotional punctuation"
                Negation -> "Negative self-talk"
                OverCorrection -> "Self-corrections"
                PowerPhrase -> "Empowering language"
                ResignedPhrase -> "Resigned language"
                Repetition -> "Repetitive phrases"
                CognitiveLoad -> "Signs of cognitive load"
                GrowthLanguage -> "Growth language"
                Urgency -> "Urgent language"
                
            examples = T.intercalate ", " $ map matchedText $ take 3 cs
            count = length cs
            
        in mconcat
            [ "**"
            , T.pack typeName
            , "** ("
            , T.pack (show count)
            , " instances)\n"
            , "Examples: "
            , examples
            , if count > 3 then "..." else ""
            ]

-- JSON instances for serialization
instance ToJSON CueType where
    toJSON = String . \case
        Fragmentation -> "fragmentation"
        OverPunctuation -> "over_punctuation"
        Negation -> "negation"
        OverCorrection -> "over_correction"
        PowerPhrase -> "power_phrase"
        ResignedPhrase -> "resigned_phrase"
        Repetition -> "repetition"
        CognitiveLoad -> "cognitive_load"
        GrowthLanguage -> "growth_language"
        Urgency -> "urgency"

instance FromJSON CueType where
    parseJSON (String s) = case s of
        "fragmentation" -> return Fragmentation
        "over_punctuation" -> return OverPunctuation
        "negation" -> return Negation
        "over_correction" -> return OverCorrection
        "power_phrase" -> return PowerPhrase
        "resigned_phrase" -> return ResignedPhrase
        "repetition" -> return Repetition
        "cognitive_load" -> return CognitiveLoad
        "growth_language" -> return GrowthLanguage
        "urgency" -> return Urgency
        _ -> fail $ "Unknown CueType: " ++ T.unpack s
    parseJSON _ = fail "Expected string for CueType"

instance ToJSON LinguisticCue where
    toJSON (LinguisticCue cueType matchedText intensity context) = object
        [ "type" .= cueType
        , "matched_text" .= matchedText
        , "intensity" .= intensity
        , "context" .= context
        ]

instance FromJSON LinguisticCue where
    parseJSON = withObject "LinguisticCue" $ \v -> LinguisticCue
        <$> v .: "type"
        <*> v .: "matched_text"
        <*> v .: "intensity"
        <*> v .:? "context"

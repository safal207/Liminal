{-# LANGUAGE OverloadedStrings #-}
module FuzzyEmotion where

import InsightStats (EmotionStats(..))
import Data.Text (Text, pack, unpack)
import Text.Printf (printf)

-- | Calculate membership value based on count
countMembership :: Int -> Double
countMembership count
    | count >= 10 = 1.0
    | count >= 5  = 0.6
    | count >= 2  = 0.3
    | otherwise   = 0.1

-- | Calculate membership value based on average weight
avgWeightMembership :: Double -> Double
avgWeightMembership w
    | w >= 3.5  = 1.0
    | w >= 2.5  = 0.7
    | w >= 1.5  = 0.4
    | otherwise = 0.1

-- | Calculate fuzzy influence score for a single emotion
calculateInfluence :: EmotionStats -> (Text, Double, Text)
calculateInfluence stat@(EmotionStats name count _ avgW) =
    let countScore = 0.6 * countMembership count
        weightScore = 0.4 * avgWeightMembership avgW
        totalScore = countScore + weightScore
        interpretation = interpretInfluence totalScore
    in (name, totalScore, interpretation)

-- | Convert fuzzy score to human-readable interpretation
interpretInfluence :: Double -> Text
interpretInfluence score
    | score >= 0.8 = "очень сильное влияние"
    | score >= 0.5 = "заметное влияние"
    | score >= 0.2 = "слабое влияние"
    | otherwise    = "почти незаметно"

-- | Fuzzy interpretation of emotion statistics
fuzzyInterpret :: [EmotionStats] -> [(Text, Double, Text)]
fuzzyInterpret = map calculateInfluence

-- | Format fuzzy interpretation as a table
formatFuzzyResults :: [(Text, Double, Text)] -> String
formatFuzzyResults results =
    let header = printf "%-15s %10s %25s" "Эмоция" "Влияние" "Интерпретация"
        separator = replicate 60 '-'
        formatRow (emotion, score, desc) = 
            printf "%-15s %9.2f %25s" (unpack emotion) score (unpack desc)
        rows = map formatRow results
    in unlines $ header : separator : rows

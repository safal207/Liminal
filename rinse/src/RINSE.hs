module RINSE where

import Types
import Database
import Logger
import Data.Text (Text, pack, unpack, isInfixOf)
import qualified Data.Text as T
import qualified Text.Regex.TDFA as RE
import Data.Vector (Vector, fromList)
import Data.List (nub, sortBy)
import Data.Ord (comparing)
import Data.Function (on)
import Data.Time (UTCTime)
import Control.Monad (when)
import System.Random (randomRIO)

-- 🧠 Главная функция: очистка, анализ, сохранение
processExperience :: RINSEInput -> IO RINSEOutput
processExperience (RINSEInput exp ts) = do
  let cleaned = cleanExperience exp
  let (insight, tags) = extractInsightAndTags cleaned
  clarity <- calculateClarity cleaned
  let output = RINSEOutput cleaned insight tags clarity ts
  saveRINSEOutput output
  logRINSEOutput output
  return output

-- 🧹 Очистка текста от шума
cleanExperience :: Text -> Text
cleanExperience exp =
  let patterns = ["[[:punct:]]+", "[[:space:]]+", "\\b\\w{1,2}\\b"]
  in foldl (\acc p -> regexReplace p " " acc) exp patterns

-- 🧠 Извлечение инсайта и тегов
extractInsightAndTags :: Text -> (Text, Vector EmotionTag)
extractInsightAndTags exp =
  let sentences = splitSentences exp
      insights = map extractKeyPhrase sentences
      tags = classifyEmotions exp
  in (if null insights then exp else head insights, fromList $ sortBy (flip (compare `on` weight)) tags)

-- 🔍 Классификация эмоций
classifyEmotions :: Text -> [EmotionTag]
classifyEmotions exp =
  let keywords =
        [ ("страх", ["страх", "боязнь", "страшно", "беспокойство"])
        , ("радость", ["радость", "счастье", "восторг", "удовольствие"])
        , ("грусть", ["грусть", "печаль", "тоска", "мрачно"])
        , ("решимость", ["решимость", "уверенность", "самоуверенность", "уверен"])
        ]
      matches = map (\(emotion, words) -> (emotion, countMatches words exp)) keywords
      filtered = filter (\(_, count) -> count > 0) matches
  in map (\(e, w) -> EmotionTag e w) filtered

-- 📊 Расчёт ясности
calculateClarity :: Text -> IO Double
calculateClarity exp = do
  let ws = T.words exp
      wordCount = length ws
      uniqueWords = length $ nub ws
      sentenceCount = max 1 (length $ splitSentences exp)
      avgSentenceLength = fromIntegral wordCount / fromIntegral sentenceCount
      clarityScore = (fromIntegral uniqueWords / fromIntegral wordCount) * avgSentenceLength
  return $ clarityScore * (if clarityScore > 0.7 then 1.0 else 0.5)

-- 🧰 Вспомогательные

regexReplace :: String -> String -> Text -> Text
regexReplace pattern replacement text =
  let input = unpack text
      replaced = input RE.=~ pattern RE.// replacement
  in pack replaced

splitSentences :: Text -> [Text]
splitSentences = T.splitOn "."

extractKeyPhrase :: Text -> Text
extractKeyPhrase = pack . take 50 . unpack

countMatches :: [Text] -> Text -> Int
countMatches words text = length $ filter (`T.isInfixOf` text) words

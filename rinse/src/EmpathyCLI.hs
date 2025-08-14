{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import qualified ТыИЯОднойКрови as TYOK
import qualified OneBloodEmpathyTraining as OBET
import MusicOfTransition (EmotionalState(..))
import Data.Aeson (FromJSON, eitherDecode, decode, encode)
import qualified Data.ByteString.Lazy as BL
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text, pack, unpack)
import qualified Data.Text.IO as T
import Data.List (intercalate)
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess, exitFailure, ExitCode(..))
import System.FilePath (takeExtension)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Time (getCurrentTime)
import Control.Monad (when, unless)
import Control.Exception (try, SomeException)
import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON(..), ToJSON(..), withObject, (.:), (.:?))
import Data.Scientific (toRealFloat)

-- | Command line commands
data Command 
    = Train { logFile :: FilePath, userId :: String }
    | Suggest { mapFile :: FilePath, emotion :: String }
    | Save { inputMap :: FilePath, outputMap :: FilePath }
    | Load { mapFile :: FilePath }
    | Help
    deriving (Show, Eq)

-- | Parse command line arguments
parseArgs :: [String] -> IO Command
parseArgs [] = return Help
parseArgs ("help" : _) = return Help
parseArgs ("train" : logFile : userId : _) = return $ Train logFile userId
parseArgs ("suggest" : mapFile : emotion : _) = return $ Suggest mapFile emotion
parseArgs ("save" : input : output : _) = return $ Save input output
parseArgs ("load" : mapFile : _) = return $ Load mapFile
parseArgs _ = do
    putStrLn "❌ Неверные аргументы. Используйте 'help' для справки."
    exitFailure

-- | Display help message
showHelp :: IO ()
showHelp = do
    progName <- getProgName
    putStrLn $ unlines
        [ "Использование: " ++ progName ++ " КОМАНДА [АРГУМЕНТЫ...]"
        , ""
        , "Команды:"
        , "  train <лог.jsonl> <userId>  Обучить SharedResonanceMap из файла логов"
        , "  suggest <map.json> <emotion>  Предложить отклики на эмоцию"
        , "  save <входная_карта> <выходная_карта>  Сохранить обученную карту"
        , "  load <map.json>  Загрузить и проверить карту"
        , "  help  Показать эту справку"
        ]

-- | Load a resonance map from file
loadResonanceMap :: FilePath -> IO (Either String TYOK.SharedResonanceMap)
loadResonanceMap filePath = do
    putStrLn $ "🔍 Загрузка резонансной карты из " ++ filePath
    content <- BL.readFile filePath
    return $ eitherDecode content

-- | Save a resonance map to file
saveResonanceMap :: FilePath -> TYOK.SharedResonanceMap -> IO ()
saveResonanceMap filePath srm = do
    putStrLn $ "💾 Сохранение резонансной карты в " ++ filePath
    BL.writeFile filePath (encode srm)

-- | Train a resonance map from log file
trainFromLogs :: FilePath -> String -> IO ()
trainFromLogs logFile userId' = do
    putStrLn $ "🎓 Обучение на логах из " ++ logFile
    
    -- Check file extension
    unless (takeExtension logFile == ".jsonl") $
        putStrLn "⚠️  Предупреждение: ожидается файл с расширением .jsonl"
    
    -- Load and parse logs
    logContent <- BL.readFile logFile
    let logs = map (eitherDecode . BL.fromStrict) $ BL.split (fromIntegral (fromEnum '\n')) logContent
    
    case sequence logs of
        Left err -> do
            putStrLn $ "❌ Ошибка при разборе логов: " ++ err
            exitFailure
        Right transitions -> do
            putStrLn $ "📊 Загружено " ++ show (length transitions) ++ " переходов"
            
            -- Create or load existing map
            currentTime <- getCurrentTime
            let initialMap = TYOK.emptyResonanceMap (pack userId')
            
            -- Train the model
            putStrLn "🧠 Обучение модели..."
            trainedMap <- TYOK.learnFromTransitions transitions initialMap
            
            -- Save the trained map
            let outputFile = "trained_map_" ++ userId' ++ ".json"
            saveResonanceMap outputFile trainedMap
            putStrLn $ "✅ Обучение завершено. Карта сохранена в " ++ outputFile

-- | Suggest responses for an emotion
suggestResponses :: FilePath -> String -> IO ()
suggestResponses mapFile' emotion' = do
    result <- loadResonanceMap mapFile'
    case result of
        Left err -> do
            putStrLn $ "❌ Ошибка загрузки карты: " ++ err
            exitFailure
        Right srm -> do
            -- Create a test emotional state with the given emotion
            currentTime <- getCurrentTime
            let testState = EmotionalState
                    { esEmotions = M.singleton (pack emotion') 0.8
                    , esText = pack $ "Тестовое состояние для эмоции: " ++ emotion'
                    , esLinguisticCues = []
                    , esTimestamp = currentTime
                    }
            
            -- Get suggested responses
            let responses = TYOK.matchUserState testState srm
            
            -- Display results
            if null responses
                then putStrLn "🤷 Не найдено подходящих откликов для данной эмоции."
                else do
                    putStrLn "🔍 Предложенные отклики:"
                    mapM_ (T.putStrLn . ("- " <>) . TYOK.srText) (take 3 responses)  -- Show top 3 responses

-- | Main function
main :: IO ()
main = do
    args <- getArgs
    cmd <- parseArgs args
    
    case cmd of
        Train logFile' userId' -> do
            trainFromLogs logFile' userId'
            
        Suggest mapFile' emotion' -> do
            suggestResponses mapFile' emotion'
            
        Save inputMap' outputMap' -> do
            result <- loadResonanceMap inputMap'
            case result of
                Left err -> do
                    putStrLn $ "❌ Ошибка загрузки карты: " ++ err
                    exitFailure
                Right srm -> do
                    saveResonanceMap outputMap' srm
                    putStrLn $ "✅ Карта успешно сохранена в " ++ outputMap'
                    
        Load mapFile' -> do
            result <- loadResonanceMap mapFile'
            case result of
                Left err -> do
                    putStrLn $ "❌ Ошибка загрузки карты: " ++ err
                    exitFailure
                Right srm -> do
                    let nodeCount = M.size (TYOK.srmNodes srm)
                        respCount = M.size (TYOK.srmResponses srm)
                    putStrLn $ "✅ Карта успешно загружена:"
                    putStrLn $ "   - Узлов: " ++ show nodeCount
                    putStrLn $ "   - Ответов: " ++ show respCount
                    putStrLn $ "   - Пользователь: " ++ unpack (TYOK.srmUserId srm)
                    
        Help -> showHelp

-- | Helper function to convert a list to a non-empty list
toNonEmpty :: [a] -> Maybe (NonEmpty a)
toNonEmpty [] = Nothing
toNonEmpty (x:xs) = Just (x :| xs)

-- | Helper function to convert a non-empty list to a list
fromNonEmpty :: NonEmpty a -> [a]
fromNonEmpty (x :| xs) = x : xs

-- | JSON instance for EmotionalTransition
instance FromJSON TYOK.EmotionalTransition where
    parseJSON = withObject "EmotionalTransition" $ \v -> do
        states <- v .: "states"
        timestamp <- v .: "timestamp"
        effectiveness <- v .:? "effectiveness" .!= 0.5
        case toNonEmpty states of
            Nothing -> fail "states cannot be empty"
            Just neStates -> return TYOK.EmotionalTransition
                { TYOK.etStates = neStates
                , TYOK.etTimestamp = timestamp
                , TYOK.etEffectiveness = effectiveness
                }

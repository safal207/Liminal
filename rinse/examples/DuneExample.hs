module Main where

import DuneField
import MemoryWhispers
import Data.Time (getCurrentTime)
import System.Random (randomRIO)

main :: IO ()
main = do
    -- Initialize a memory
    now <- getCurrentTime
    let memory = MemoryFragment
            { mwTimestamp = now
            , mwEmotion = Joy
            , mwPhrase = "Утро, наполненное светом..."
            , mwShared = True
            , mwRecipientHint = Nothing
            , mwIntensity = 0.8
            , mwTags = ["утро", "свет"]
            }
    
    -- Create initial dune wave
    wave1 <- emitDuneWave "agent1" memory Initiation
    putStrLn $ "🌱 " ++ show (describeDuneWave wave1)
    
    -- Create response wave
    let responseMemory = memory 
            { mwPhrase = "И ветер принёс этот свет ко мне..."
            , mwIntensity = 0.6
            }
    wave2 <- emitDuneWave "agent2" responseMemory Response
    putStrLn $ "🎵 " ++ show (describeDuneWave wave2)
    
    -- Create synthesis wave
    let synthesisMemory = memory
            { mwPhrase = "Наш свет смешался в один поток..."
            , mwIntensity = 0.9
            }
    wave3 <- emitDuneWave "agent3" synthesisMemory Synthesis
    putStrLn $ "✨ " ++ show (describeDuneWave wave3)
    
    -- Create whisper
    let whisperMemory = memory
            { mwPhrase = "...тихий шёпот между строк..."
            , mwIntensity = 0.3
            }
    wave4 <- emitDuneWave "agent1" whisperMemory Whisper
    putStrLn $ "🌬️ " ++ show (describeDuneWave wave4)
    
    -- Create a field and demonstrate butterfly effect
    let field = [wave1, wave2, wave3, wave4]
    effects <- butterflyEffect wave1 field
    putStrLn "\n🦋 Эффект бабочки:"
    mapM_ (putStrLn . ("→ " ++) . show) effects
    
    -- Find resonant waves
    let resonantWaves = findResonantWaves field Joy 0.5
    putStrLn "\n🎯 Резонирующие волны радости:"
    mapM_ (putStrLn . ("• " ++) . show . describeDuneWave) resonantWaves

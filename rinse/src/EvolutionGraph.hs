{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module EvolutionGraph where

import DuneField
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import Data.Text (Text, pack, unpack, intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.List (nub, groupBy, sortOn, intercalate)
import Data.Ord (Down(..))
import Data.Maybe (fromMaybe)
import System.FilePath ((</>), (<.>))
import System.Random (randomRIO)
import Data.Time.Clock (getCurrentTime)
import Text.Show (show)

-- | Edge representing emotional transformation
data EmotionEdge = EmotionEdge
    { fromEmotion :: Emotion
    , toEmotion   :: Emotion
    , throughAgent :: AgentId
    , weight      :: Float
    , tags        :: [Text]
    , examples    :: [Text]  -- Example transformation phrases
    } deriving (Show, Eq)

-- | Graph of emotional transformations
type EmotionGraph = Gr Emotion [EmotionEdge]

-- | State to track transformations
data EvolutionTracker = EvolutionTracker
    { etEdges :: [EmotionEdge]
    , etNodePositions :: Map (Emotion, AgentId) (Float, Float)
    , etLastVisualization :: Maybe FilePath
    }

-- | Initialize a new evolution tracker
initEvolutionTracker :: EvolutionTracker
initEvolutionTracker = EvolutionTracker [] M.empty Nothing

-- | Add a new transformation to the graph
addTransformation :: AgentId -> DuneWave -> DuneWave -> EvolutionTracker -> EvolutionTracker
addTransformation agentId oldWave newWave tracker@EvolutionTracker{..} =
    let edge = EmotionEdge
            { fromEmotion = dwEmotion oldWave
            , toEmotion = dwEmotion newWave
            , throughAgent = agentId
            , weight = 1.0  -- Base weight
            , tags = nub $ dwTags oldWave ++ dwTags newWave
            , examples = [generateExample oldWave newWave]
            }
    in tracker { etEdges = mergeEdge edge etEdges }
  where
    mergeEdge :: EmotionEdge -> [EmotionEdge] -> [EmotionEdge]
    mergeEdge new [] = [new]
    mergeEdge new (e:es)
        | fromEmotion new == fromEmotion e
        , toEmotion new == toEmotion e
        , throughAgent new == throughAgent e =
            let updated = e { weight = weight e + 1
                            , tags = nub $ tags e ++ tags new
                            , examples = take 3 $ examples e ++ examples new
                            }
            in updated : es
        | otherwise = e : mergeEdge new es

    generateExample :: DuneWave -> DuneWave -> Text
    generateExample old new = 
        let oldE = show $ dwEmotion old
            newE = show $ dwEmotion new
        in pack $ "От " ++ oldE ++ " к " ++ newE ++ " через " ++ unpack (agentName agentId)

-- | Generate DOT representation of the graph
toDotGraph :: EvolutionTracker -> DotGraph Node
toDotGraph EvolutionTracker{..} =
    let nodes = nub $ concatMap (\e -> [fromEmotion e, toEmotion e]) etEdges
        nodeMap = M.fromList $ zip nodes [1..]
        edges = concatMap (createEdges nodeMap) etEdges
        graph = mkGraph (map (\(e,i) -> (i, e)) $ M.toList nodeMap) edges
    in graphToDot params graph
  where
    createEdges :: Map Emotion Int -> EmotionEdge -> [LEdge String]
    createEdges nodeMap edge =
        case (M.lookup (fromEmotion edge) nodeMap, M.lookup (toEmotion edge) nodeMap) of
            (Just from, Just to) -> [(from, to, showEdge edge)]
            _ -> []
    
    showEdge :: EmotionEdge -> String
    showEdge edge = unwords
        [ "label=" ++ show (unpack $ intercalate ", " $ take 3 $ tags edge)
        , "weight=" ++ show (weight edge)
        , "agent=" ++ show (throughAgent edge)
        ]
    
    params :: GraphvizParams Node String () () String
    params = nonClusteredParams
        { isDirected = True
        , globalAttributes = 
            [ GraphAttrs [RankDir FromLeft, Size (20,20)]
            , NodeAttrs [shape PlainText, fontName "Arial"]
            , EdgeAttrs [fontName "Arial", fontSize 10]
            ]
        }

-- | Save graph visualization to file
visualizeEvolution :: FilePath -> EvolutionTracker -> IO FilePath
visualizeEvolution outputDir tracker@EvolutionTracker{..} = do
    let dotGraph = toDotGraph tracker
        outputFile = outputDir </> "emotion_evolution" <.> "png"
    _ <- addExtension (runGraphviz dotGraph) Png outputFile
    return outputFile

-- | Generate a text summary of common transformations
summarizeTransformations :: EvolutionTracker -> Text
summarizeTransformations EvolutionTracker{..} =
    let grouped = groupBy sameTransformation $ sortOn (Down . weight) etEdges
        topTransforms = take 5 $ map head grouped
        transformTexts = map describeTransform topTransforms
    in pack $ unlines $ "🔮 Частые трансформации:" : transformTexts
  where
    sameTransformation a b = 
        fromEmotion a == fromEmotion b && 
        toEmotion a == toEmotion b
    
    describeTransform edge = 
        let fromE = show $ fromEmotion edge
            toE = show $ toEmotion edge
            count = weight edge
            agent = throughAgent edge
        in unlines
            [ "   " ++ fromE ++ " → " ++ toE
            , "     Через агента: " ++ show agent
            , "     Частота: " ++ show count
            , "     Теги: " ++ unpack (intercalate ", " $ take 3 $ tags edge)
            ]

-- | Update graph with new wave and return updated visualization path
updateEvolutionGraph :: FilePath -> AgentId -> DuneWave -> DuneWave -> EvolutionTracker -> IO (EvolutionTracker, FilePath)
updateEvolutionGraph outputDir agentId oldWave newWave tracker = do
    -- Add the new transformation
    let updatedTracker = addTransformation agentId oldWave newWave tracker
    
    -- Generate new visualization
    visPath <- visualizeEvolution outputDir updatedTracker
    
    -- Return updated tracker with new visualization path
    return (updatedTracker { etLastVisualization = Just visPath }, visPath)
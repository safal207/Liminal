module Database where

import Types
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Neo4j
import Database.Redis
import Control.Monad.IO.Class (liftIO)
import Control.Exception (try, SomeException)

-- Neo4j connection settings
data Neo4jConfig = Neo4jConfig {
    neo4jHost :: Text,
    neo4jPort :: Int,
    neo4jUser :: Text,
    neo4jPass :: Text
}

defaultNeo4jConfig :: Neo4jConfig
defaultNeo4jConfig = Neo4jConfig {
    neo4jHost = "localhost",
    neo4jPort = 7687,
    neo4jUser = "neo4j",
    neo4jPass = "password"
}

-- Redis connection settings
data RedisConfig = RedisConfig {
    redisHost :: Text,
    redisPort :: Int
}

defaultRedisConfig :: RedisConfig
defaultRedisConfig = RedisConfig {
    redisHost = "localhost",
    redisPort = 6379
}

-- Save RINSE result to Neo4j
dataToNeo4j :: RINSEOutput -> IO ()
dataToNeo4j output = do
    let config = defaultNeo4jConfig
    let query = "CREATE (e:Experience {" ++
                "rawExperience: $rawExperience, " ++
                "cleansed: $cleansed, " ++
                "insight: $insight, " ++
                "clarity: $clarity, " ++
                "timestamp: $timestamp}) " ++
                "FOREACH (tag IN $tags | " ++
                "  CREATE (e)-[:HAS_TAG]->(t:Tag {name: tag}))"

    result <- try $ do
        conn <- connect (neo4jHost config) (neo4jPort config)
        authenticate conn (neo4jUser config) (neo4jPass config)
        runQuery conn query params
    case result of
        Left (e :: SomeException) -> print $ "Database error: " ++ show e
        Right _ -> return ()
    where
        params = Parameters {
            "rawExperience" = rawExperience output,
            "cleansed" = cleansed output,
            "insight" = insight output,
            "tags" = tags output,
            "clarity" = clarity output,
            "timestamp" = timestamp output
        }

-- Save RINSE result to Redis
dataToRedis :: RINSEOutput -> IO ()
dataToRedis output = do
    let config = defaultRedisConfig
    result <- try $ do
        conn <- connect defaultConnectInfo
        runRedis conn $ do
            set ("experience:" ++ show (timestamp output)) (show output)
            expire ("experience:" ++ show (timestamp output)) 2592000 -- 30 days
    case result of
        Left (e :: SomeException) -> print $ "Redis error: " ++ show e
        Right _ -> return ()

-- Save RINSE result to both databases
saveRINSEOutput :: RINSEOutput -> IO ()
saveRINSEOutput output = do
    dataToNeo4j output
    dataToRedis output
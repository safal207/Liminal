module Types where

import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Vector (Vector)

data RINSEInput = RINSEInput
  { rawExperience :: Text
  , timestamp :: UTCTime
  } deriving (Show, Eq)

instance ToJSON RINSEInput where
  toJSON (RINSEInput exp ts) =
    object [ "rawExperience" .= exp
            , "timestamp" .= ts ]

instance FromJSON RINSEInput where
  parseJSON = withObject "RINSEInput" $ \v ->
    RINSEInput <$> v .: "rawExperience"
              <*> v .: "timestamp"

data EmotionTag = EmotionTag
  { emotion :: Text
  , weight :: Int
  } deriving (Show, Eq)

instance ToJSON EmotionTag where
  toJSON (EmotionTag e w) = object ["emotion" .= e, "weight" .= w]

data RINSEOutput = RINSEOutput
  { cleansed :: Text
  , insight :: Text
  , tags :: Vector EmotionTag
  , clarity :: Double
  , timestamp :: UTCTime
  } deriving (Show, Eq)

instance ToJSON RINSEOutput where
  toJSON (RINSEOutput c i t cl ts) =
    object [ "cleansed" .= c
            , "insight" .= i
            , "tags" .= t
            , "clarity" .= cl
            , "timestamp" .= ts ]

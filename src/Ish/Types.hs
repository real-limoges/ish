{-# LANGUAGE OverloadedStrings #-}

module Ish.Types
  ( Score
  , mkScore
  , unScore
  , MoodDimension (..)
  , MoodEntry (..)
  , FuzzyLabel (..)
  , MoodCluster (..)
  , AnalysisResult (..)
  ) where

import Data.Aeson
  ( FromJSON (..), ToJSON (..), FromJSONKey (..), ToJSONKey (..)
  , FromJSONKeyFunction (..), object, withObject, withScientific, withText
  , (.=), (.:)
  )
import Data.Aeson.Types (toJSONKeyText)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Time.Calendar (Day)

-- | A score clamped to [0, 1].
newtype Score = Score { unScore :: Double }
  deriving newtype (Eq, Ord, Show)

-- | Smart constructor that clamps to [0, 1].
mkScore :: Double -> Maybe Score
mkScore d
  | d >= 0 && d <= 1 = Just (Score d)
  | otherwise         = Nothing

instance ToJSON Score where
  toJSON (Score d) = toJSON d

instance FromJSON Score where
  parseJSON = withScientific "Score" $ \s ->
    let d = realToFrac s
    in case mkScore d of
         Just sc -> pure sc
         Nothing -> fail $ "Score out of range [0,1]: " <> show d

-- | The five mood dimensions.
data MoodDimension
  = Energy
  | Valence
  | Anxiety
  | Focus
  | Sociability
  deriving stock (Eq, Ord, Show, Enum, Bounded)

dimensionToText :: MoodDimension -> Text
dimensionToText Energy      = "energy"
dimensionToText Valence     = "valence"
dimensionToText Anxiety     = "anxiety"
dimensionToText Focus       = "focus"
dimensionToText Sociability = "sociability"

dimensionFromText :: Text -> Maybe MoodDimension
dimensionFromText "energy"      = Just Energy
dimensionFromText "valence"     = Just Valence
dimensionFromText "anxiety"     = Just Anxiety
dimensionFromText "focus"       = Just Focus
dimensionFromText "sociability" = Just Sociability
dimensionFromText _             = Nothing

instance ToJSON MoodDimension where
  toJSON = toJSON . dimensionToText

instance FromJSON MoodDimension where
  parseJSON = withText "MoodDimension" $ \t ->
    case dimensionFromText t of
      Just d  -> pure d
      Nothing -> fail $ "Unknown MoodDimension: " <> show t

instance ToJSONKey MoodDimension where
  toJSONKey = toJSONKeyText dimensionToText

instance FromJSONKey MoodDimension where
  fromJSONKey = FromJSONKeyTextParser $ \t ->
    case dimensionFromText t of
      Just d  -> pure d
      Nothing -> fail $ "Unknown MoodDimension key: " <> show t

-- | A single mood entry from the database.
data MoodEntry = MoodEntry
  { entryDate       :: Day
  , entryDimensions :: Map MoodDimension Score
  } deriving stock (Eq, Show)

instance ToJSON MoodEntry where
  toJSON e = object
    [ "date"       .= entryDate e
    , "dimensions" .= entryDimensions e
    ]

instance FromJSON MoodEntry where
  parseJSON = withObject "MoodEntry" $ \v -> MoodEntry
    <$> v .: "date"
    <*> v .: "dimensions"

-- | A fuzzy label with its membership degree.
data FuzzyLabel = FuzzyLabel
  { labelName       :: Text
  , labelMembership :: Score
  } deriving stock (Eq, Show)

instance ToJSON FuzzyLabel where
  toJSON l = object
    [ "label"      .= labelName l
    , "membership" .= labelMembership l
    ]

instance FromJSON FuzzyLabel where
  parseJSON = withObject "FuzzyLabel" $ \v -> FuzzyLabel
    <$> v .: "label"
    <*> v .: "membership"

-- | A cluster of mood entries.
data MoodCluster = MoodCluster
  { clusterName     :: Text
  , clusterCentroid :: Map MoodDimension Score
  , clusterSize     :: Int
  , clusterLabels   :: [FuzzyLabel]
  } deriving stock (Eq, Show)

instance ToJSON MoodCluster where
  toJSON c = object
    [ "name"     .= clusterName c
    , "centroid" .= clusterCentroid c
    , "size"     .= clusterSize c
    , "labels"   .= clusterLabels c
    ]

instance FromJSON MoodCluster where
  parseJSON = withObject "MoodCluster" $ \v -> MoodCluster
    <$> v .: "name"
    <*> v .: "centroid"
    <*> v .: "size"
    <*> v .: "labels"

-- | The result of a fuzzy analysis over mood entries.
data AnalysisResult = AnalysisResult
  { analysisClusters :: [MoodCluster]
  , analysisSummary  :: [FuzzyLabel]
  } deriving stock (Eq, Show)

instance ToJSON AnalysisResult where
  toJSON r = object
    [ "clusters" .= analysisClusters r
    , "summary"  .= analysisSummary r
    ]

instance FromJSON AnalysisResult where
  parseJSON = withObject "AnalysisResult" $ \v -> AnalysisResult
    <$> v .: "clusters"
    <*> v .: "summary"

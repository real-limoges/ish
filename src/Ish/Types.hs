module Ish.Types (
    MoodDimension (..),
    MoodEntry (..),
    Gap (..),
    FuzzyLabel (..),
    MoodCluster (..),
    AnalysisResult (..),
) where

import Data.Aeson (
    FromJSON (..),
    FromJSONKey (..),
    FromJSONKeyFunction (..),
    ToJSON (..),
    ToJSONKey (..),
    object,
    withObject,
    withText,
    (.:),
    (.=),
 )
import Data.Aeson.Types (toJSONKeyText)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Hazy (Degree)

data MoodDimension
    = Sleep
    | Anxiety
    | Sensitivity
    | Outlook
    | MentalSpeed
    deriving stock (Eq, Ord, Show, Enum, Bounded)

dimensionToText :: MoodDimension -> Text
dimensionToText Sleep = "sleep"
dimensionToText Anxiety = "anxiety"
dimensionToText Sensitivity = "sensitivity"
dimensionToText Outlook = "outlook"
dimensionToText MentalSpeed = "mentalSpeed"

dimensionFromText :: Text -> Maybe MoodDimension
dimensionFromText "sleep" = Just Sleep
dimensionFromText "anxiety" = Just Anxiety
dimensionFromText "sensitivity" = Just Sensitivity
dimensionFromText "outlook" = Just Outlook
dimensionFromText "mentalSpeed" = Just MentalSpeed
dimensionFromText _ = Nothing

instance ToJSON MoodDimension where
    toJSON = toJSON . dimensionToText

instance FromJSON MoodDimension where
    parseJSON = withText "MoodDimension" $ \t ->
        case dimensionFromText t of
            Just d -> pure d
            Nothing -> fail $ "Unknown MoodDimension: " <> show t

instance ToJSONKey MoodDimension where
    toJSONKey = toJSONKeyText dimensionToText

instance FromJSONKey MoodDimension where
    fromJSONKey = FromJSONKeyTextParser $ \t ->
        case dimensionFromText t of
            Just d -> pure d
            Nothing -> fail $ "Unknown MoodDimension key: " <> show t

data MoodEntry = MoodEntry
    { entryDate :: Day
    , entryDimensions :: Map MoodDimension Double
    }
    deriving stock (Eq, Show)

instance ToJSON MoodEntry where
    toJSON e =
        object
            [ "date" .= entryDate e
            , "dimensions" .= entryDimensions e
            ]

instance FromJSON MoodEntry where
    parseJSON = withObject "MoodEntry" $ \v ->
        MoodEntry
            <$> v .: "date"
            <*> v .: "dimensions"

-- | A fuzzy label with its membership degree.
data FuzzyLabel = FuzzyLabel
    { labelName :: Text
    , labelMembership :: Degree
    }
    deriving stock (Eq, Show)

instance ToJSON FuzzyLabel where
    toJSON l =
        object
            [ "label" .= labelName l
            , "membership" .= labelMembership l
            ]

instance FromJSON FuzzyLabel where
    parseJSON = withObject "FuzzyLabel" $ \v ->
        FuzzyLabel
            <$> v .: "label"
            <*> v .: "membership"

-- | A cluster of mood entries.
data MoodCluster = MoodCluster
    { clusterName :: Text
    , clusterCentroid :: Map MoodDimension Double
    , clusterSize :: Int
    , clusterLabels :: [FuzzyLabel]
    }
    deriving stock (Eq, Show)

instance ToJSON MoodCluster where
    toJSON c =
        object
            [ "name" .= clusterName c
            , "centroid" .= clusterCentroid c
            , "size" .= clusterSize c
            , "labels" .= clusterLabels c
            ]

instance FromJSON MoodCluster where
    parseJSON = withObject "MoodCluster" $ \v ->
        MoodCluster
            <$> v .: "name"
            <*> v .: "centroid"
            <*> v .: "size"
            <*> v .: "labels"

data Gap = Gap
    { gapStart :: Day
    , gapLength :: Int
    , gapBefore :: Day
    , gapAfter :: Day
    }
    deriving stock (Eq, Show)

instance ToJSON Gap where
    toJSON g =
        object
            [ "start" .= gapStart g
            , "length" .= gapLength g
            , "before" .= gapBefore g
            , "after" .= gapAfter g
            ]

instance FromJSON Gap where
    parseJSON = withObject "Gap" $ \v ->
        Gap
            <$> v .: "start"
            <*> v .: "length"
            <*> v .: "before"
            <*> v .: "after"

-- | The result of a fuzzy analysis over mood entries.
data AnalysisResult = AnalysisResult
    { analysisClusters :: [MoodCluster]
    , analysisSummary :: [FuzzyLabel]
    }
    deriving stock (Eq, Show)

instance ToJSON AnalysisResult where
    toJSON r =
        object
            [ "clusters" .= analysisClusters r
            , "summary" .= analysisSummary r
            ]

instance FromJSON AnalysisResult where
    parseJSON = withObject "AnalysisResult" $ \v ->
        AnalysisResult
            <$> v .: "clusters"
            <*> v .: "summary"

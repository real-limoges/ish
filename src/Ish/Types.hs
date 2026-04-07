module Ish.Types (
    MoodDimension (..),
    dimensionToText,
    MoodEntry (..),
    Gap (..),
    FuzzyLabel (..),
    MoodCluster (..),
    AnalysisResult (..),
    TermDef (..),
    VarDef (..),
    MembershipFuncDefs (..),
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
    | Speed
    deriving stock (Eq, Ord, Show, Enum, Bounded)

dimensionToText :: MoodDimension -> Text
dimensionToText Sleep = "sleep"
dimensionToText Anxiety = "anxiety"
dimensionToText Sensitivity = "sensitivity"
dimensionToText Outlook = "outlook"
dimensionToText Speed = "speed"

dimensionFromText :: Text -> Maybe MoodDimension
dimensionFromText "sleep" = Just Sleep
dimensionFromText "anxiety" = Just Anxiety
dimensionFromText "sensitivity" = Just Sensitivity
dimensionFromText "outlook" = Just Outlook
dimensionFromText "speed" = Just Speed
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

-- | termParams is (left foot, peak, right foot) of a triangle.
data TermDef = TermDef
    { termName :: Text
    , termParams :: (Double, Double, Double)
    }
    deriving stock (Eq, Show)

instance ToJSON TermDef where
    toJSON t =
        object
            [ "name" .= termName t
            , "params" .= let (a, b, c) = termParams t in [a, b, c]
            ]

instance FromJSON TermDef where
    parseJSON = withObject "TermDef" $ \v -> do
        n <- v .: "name"
        ps <- v .: "params"
        case ps of
            [a, b, c] -> pure $ TermDef n (a, b, c)
            _ -> fail "params must be a 3-element array [left, peak, right]"

data VarDef = VarDef
    { varName :: Text
    , varBounds :: (Double, Double)
    , varTerms :: [TermDef]
    }
    deriving stock (Eq, Show)

instance ToJSON VarDef where
    toJSON v =
        object
            [ "name" .= varName v
            , "bounds" .= let (lo, hi) = varBounds v in [lo, hi]
            , "terms" .= varTerms v
            ]

instance FromJSON VarDef where
    parseJSON = withObject "VarDef" $ \v -> do
        n <- v .: "name"
        bs <- v .: "bounds"
        ts <- v .: "terms"
        case bs of
            [lo, hi] -> pure $ VarDef n (lo, hi) ts
            _ -> fail "bounds must be a 2-element array [lo, hi]"

data MembershipFuncDefs = MembershipFuncDefs
    { mfdInputs :: [VarDef]
    , mfdOutputs :: [VarDef]
    }
    deriving stock (Eq, Show)

instance ToJSON MembershipFuncDefs where
    toJSON m =
        object
            [ "inputs" .= mfdInputs m
            , "outputs" .= mfdOutputs m
            ]

instance FromJSON MembershipFuncDefs where
    parseJSON = withObject "MembershipFuncDefs" $ \v ->
        MembershipFuncDefs
            <$> v .: "inputs"
            <*> v .: "outputs"

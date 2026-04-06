module Ish.Analysis.Fuzzify (
    moodInputVars,
    moodOutputVars,
    moodFIS,
    fuzzifyEntries,
) where

import Data.List (transpose)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import DataFrame (DataFrame)
import DataFrame qualified as D
import Hazy (
    FIS (..),
    FuzzyRule (..),
    FuzzySet (..),
    InferenceMethod (Mamdani),
    LinguisticVar (..),
    evaluate,
    triangular,
    )

-- | Input linguistic variables — one per MoodDimension.
--
-- Normal ranges: anxiety/sensitivity 1–2, outlook/speed 4–6.
moodInputVars :: Map Text LinguisticVar
moodInputVars = Map.fromList
    [ ("sleep",       mkVar "sleep"       (0, 15) [(0,0,7.5), (5,7.5,10), (7.5,15,15)])
    , ("anxiety",     mkVar "anxiety"     (0, 5)  [(0,0,1), (0.5,1.5,3), (2,5,5)])
    , ("sensitivity", mkVar "sensitivity" (0, 5)  [(0,0,1), (0.5,1.5,3), (2,5,5)])
    , ("outlook",     mkVar "outlook"     (0, 10) [(0,0,4), (3,5,7), (6,10,10)])
    , ("speed",       mkVar "speed"       (0, 10) [(0,0,4), (3,5,7), (6,10,10)])
    ]

-- | Output linguistic variables — higher-level mood states derived from
-- the 5 input dimensions.
moodOutputVars :: Map Text LinguisticVar
moodOutputVars = Map.fromList
    [ ("wellbeing",  mkVar "wellbeing"  (0, 10) [(0,0,4), (3,5,7), (6,10,10)])
    , ("activation", mkVar "activation" (0, 10) [(0,0,4), (3,5,7), (6,10,10)])
    ]

-- | The complete Fuzzy Inference System for mood analysis.
moodFIS :: FIS
moodFIS = FIS
    { fisName = "mood"
    , fisInputs = moodInputVars
    , fisOutputs = moodOutputVars
    , fisRules = moodRules
    , fisMethod = Mamdani
    }

moodRules :: [FuzzyRule]
moodRules =
    [ FuzzyRule [("sleep","high"), ("anxiety","low")]  [("wellbeing","high")]
    , FuzzyRule [("sleep","low"),  ("anxiety","high")] [("wellbeing","low")]
    , FuzzyRule [("outlook","high")]                   [("wellbeing","high")]
    , FuzzyRule [("outlook","low")]                    [("wellbeing","low")]
    , FuzzyRule [("speed","high"), ("sensitivity","high")] [("activation","high")]
    , FuzzyRule [("speed","low")]                      [("activation","low")]
    ]

-- | Fuzzify a date-spine DataFrame, adding wellbeing and activation columns
-- from the FIS while keeping all raw columns.
fuzzifyEntries :: DataFrame -> DataFrame
fuzzifyEntries df =
    let dimNames = ["sleep", "anxiety", "sensitivity", "outlook", "speed"]
        dimCols = map (\d -> D.columnAsList (D.col d :: D.Expr (Maybe Double)) df) dimNames
        evalResults = map (tryEval dimNames) (transpose dimCols)
        wellbeingCol = map (>>= Map.lookup "wellbeing") evalResults
        activationCol = map (>>= Map.lookup "activation") evalResults
     in D.insertColumn "activation" (D.fromList activationCol)
            $ D.insertColumn "wellbeing" (D.fromList wellbeingCol)
            $ df
  where
    tryEval names vals =
        case sequence vals of
            Nothing -> Nothing
            Just vs -> Just (evaluate moodFIS (Map.fromList (zip names vs)))

mkVar :: Text -> (Double, Double) -> [(Double, Double, Double)] -> LinguisticVar
mkVar name bounds [(la,lb,lc), (ma,mb,mc), (ha,hb,hc)] =
    LinguisticVar
        { lvName  = name
        , lvBounds = bounds
        , lvTerms = Map.fromList
            [ ("low",    FuzzySet "low"    (triangular la lb lc) bounds)
            , ("medium", FuzzySet "medium" (triangular ma mb mc) bounds)
            , ("high",   FuzzySet "high"   (triangular ha hb hc) bounds)
            ]
        }

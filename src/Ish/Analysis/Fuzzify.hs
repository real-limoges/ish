module Ish.Analysis.Fuzzify (
    defaultMembershipFuncDefs,
    buildVars,
    buildFIS,
    moodInputVars,
    moodOutputVars,
    moodFIS,
    fuzzifyEntries,
    fuzzifyEntriesWith,
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

import Ish.Types (MembershipFuncDefs (..), TermDef (..), VarDef (..))

defaultMembershipFuncDefs :: MembershipFuncDefs
defaultMembershipFuncDefs =
    MembershipFuncDefs
        { mfdInputs =
            [ VarDef "sleep" (0, 15) [TermDef "low" (0, 0, 7.5), TermDef "medium" (5, 7.5, 10), TermDef "high" (7.5, 15, 15)]
            , VarDef "anxiety" (0, 5) [TermDef "low" (0, 0, 1), TermDef "medium" (0.5, 1.5, 3), TermDef "high" (2, 5, 5)]
            , VarDef "sensitivity" (0, 5) [TermDef "low" (0, 0, 1), TermDef "medium" (0.5, 1.5, 3), TermDef "high" (2, 5, 5)]
            , VarDef "outlook" (0, 10) [TermDef "low" (0, 0, 4), TermDef "medium" (3, 5, 7), TermDef "high" (6, 10, 10)]
            , VarDef "speed" (0, 10) [TermDef "low" (0, 0, 4), TermDef "medium" (3, 5, 7), TermDef "high" (6, 10, 10)]
            ]
        , mfdOutputs =
            [ VarDef "wellbeing" (0, 10) [TermDef "low" (0, 0, 4), TermDef "medium" (3, 5, 7), TermDef "high" (6, 10, 10)]
            , VarDef "activation" (0, 10) [TermDef "low" (0, 0, 4), TermDef "medium" (3, 5, 7), TermDef "high" (6, 10, 10)]
            ]
        }

-- | Bridges JSON-serializable VarDefs to Hazy's LinguisticVar (which contains opaque MembershipFn closures).
buildVars :: [VarDef] -> Map Text LinguisticVar
buildVars = Map.fromList . map toVar
  where
    toVar (VarDef name bounds terms) =
        ( name
        , LinguisticVar
            { lvName = name
            , lvBounds = bounds
            , lvTerms =
                Map.fromList
                    [ (termName t, FuzzySet (termName t) (triangular a b c) bounds)
                    | t <- terms
                    , let (a, b, c) = termParams t
                    ]
            }
        )

buildFIS :: MembershipFuncDefs -> FIS
buildFIS defs =
    FIS
        { fisName = "mood"
        , fisInputs = buildVars (mfdInputs defs)
        , fisOutputs = buildVars (mfdOutputs defs)
        , fisRules = moodRules
        , fisMethod = Mamdani
        }

moodInputVars :: Map Text LinguisticVar
moodInputVars = buildVars (mfdInputs defaultMembershipFuncDefs)

moodOutputVars :: Map Text LinguisticVar
moodOutputVars = buildVars (mfdOutputs defaultMembershipFuncDefs)

moodFIS :: FIS
moodFIS = buildFIS defaultMembershipFuncDefs

moodRules :: [FuzzyRule]
moodRules =
    [ FuzzyRule [("sleep", "high"), ("anxiety", "low")] [("wellbeing", "high")]
    , FuzzyRule [("sleep", "low"), ("anxiety", "high")] [("wellbeing", "low")]
    , FuzzyRule [("outlook", "high")] [("wellbeing", "high")]
    , FuzzyRule [("outlook", "low")] [("wellbeing", "low")]
    , FuzzyRule [("speed", "high"), ("sensitivity", "high")] [("activation", "high")]
    , FuzzyRule [("speed", "low")] [("activation", "low")]
    ]

fuzzifyEntries :: DataFrame -> DataFrame
fuzzifyEntries = fuzzifyEntriesWith moodFIS

fuzzifyEntriesWith :: FIS -> DataFrame -> DataFrame
fuzzifyEntriesWith fis df =
    let dimNames = ["sleep", "anxiety", "sensitivity", "outlook", "speed"]
        dimCols = map (\d -> D.columnAsList (D.col d :: D.Expr (Maybe Double)) df) dimNames
        evalResults = map (tryEval dimNames) (transpose dimCols)
        wellbeingCol = map (>>= Map.lookup "wellbeing") evalResults
        activationCol = map (>>= Map.lookup "activation") evalResults
     in D.insertColumn "activation" (D.fromList activationCol) $
            D.insertColumn
                "wellbeing"
                (D.fromList wellbeingCol)
                df
  where
    tryEval names vals =
        case sequence vals of
            Nothing -> Nothing
            Just vs -> Just (evaluate fis (Map.fromList (zip names vs)))

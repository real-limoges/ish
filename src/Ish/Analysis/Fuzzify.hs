module Ish.Analysis.Fuzzify (
    defaultMembershipFuncDefs,
    buildVars,
    buildFIS,
    moodInputVars,
    moodOutputVars,
    moodFIS,
    fuzzifyEntries,
    fuzzifyEntriesWith,
    suggestMembershipFuncDefs,
) where

import Data.List (sort, transpose)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
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

{- | Derive candidate term shapes for each input variable from the present
values in the given DataFrame. Variable bounds are preserved from the
supplied MembershipFuncDefs (they match the SQLite CHECK constraints and
shouldn't be moved), as are the output-variable definitions. Terms are
rebuilt per-input from column percentiles so the fuzzy lens tracks the
actual distribution of logged values rather than textbook defaults.
-}
suggestMembershipFuncDefs :: MembershipFuncDefs -> DataFrame -> MembershipFuncDefs
suggestMembershipFuncDefs current df =
    current{mfdInputs = map suggestOne (mfdInputs current)}
  where
    suggestOne v =
        let sorted = sort (presentValues (varName v) df)
         in v{varTerms = suggestTerms (varBounds v) sorted}

presentValues :: Text -> DataFrame -> [Double]
presentValues name df =
    catMaybes (D.columnAsList (D.col name :: D.Expr (Maybe Double)) df)

{- | Percentile-anchored triangular terms. With no samples, fall back to a
simple even three-way split so the suggestion is still well-formed.
-}
suggestTerms :: (Double, Double) -> [Double] -> [TermDef]
suggestTerms (lo, hi) sorted
    | null sorted =
        let mid = (lo + hi) / 2
         in [ TermDef "low" (lo, lo, mid)
            , TermDef "medium" (lo, mid, hi)
            , TermDef "high" (mid, hi, hi)
            ]
    | otherwise =
        let p17 = percentile 0.17 sorted
            p33 = percentile 0.33 sorted
            p50 = percentile 0.50 sorted
            p67 = percentile 0.67 sorted
            p83 = percentile 0.83 sorted
         in [ TermDef "low" (lo, lo, p33)
            , TermDef "medium" (p17, p50, p83)
            , TermDef "high" (p67, hi, hi)
            ]

{- | Nearest-rank percentile on an already-sorted, non-empty list.
The caller guards the empty case.
-}
percentile :: Double -> [Double] -> Double
percentile p sorted =
    let n = length sorted
        idx = max 0 (min (n - 1) (floor (p * fromIntegral (n - 1))))
     in sorted !! idx

module Ish.Analysis.DataFrame (
    fillMissingDates,
    identifyGaps,
    extractPresentRows,
    extractEntries,
) where

import Data.List (transpose)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing, mapMaybe)
import Data.Text qualified as Text
import Data.Time.Calendar (Day)
import Data.Vector qualified as V
import DataFrame qualified as D

import Ish.Types (Gap (..), MoodDimension (..), MoodEntry (..))

-- | Date-spine DataFrame: one row per day min–max, Nothing for absent days.
fillMissingDates :: [MoodEntry] -> D.DataFrame
fillMissingDates [] = D.empty
fillMissingDates (e : es) =
    let (minDay, maxDay, byDate) = summarize e es
        spine = [minDay .. maxDay]

        lookupDim dim day =
            Map.lookup day byDate >>= Map.lookup dim . entryDimensions

        (sleepCol, anxCol, sensCol, outlCol, msCol) =
            foldr
                ( \day (ss, as, se, os, ms) ->
                    ( lookupDim Sleep day : ss
                    , lookupDim Anxiety day : as
                    , lookupDim Sensitivity day : se
                    , lookupDim Outlook day : os
                    , lookupDim Speed day : ms
                    )
                )
                ([], [], [], [], [])
                spine
     in D.fromNamedColumns
            [ ("date", D.fromList (map (Text.pack . show) spine))
            , ("sleep", D.fromList sleepCol)
            , ("anxiety", D.fromList anxCol)
            , ("sensitivity", D.fromList sensCol)
            , ("outlook", D.fromList outlCol)
            , ("speed", D.fromList msCol)
            ]

{- | Find all gaps — groupBy runs, zip3 for before/after context.
Safe to zip3 because the spine always starts and ends on a present day.
-}
identifyGaps :: [MoodEntry] -> [Gap]
identifyGaps [] = []
identifyGaps [_] = []
identifyGaps (e : es) =
    let (minDay, maxDay, byDate) = summarize e es
        annotated = [(day, Map.lookup day byDate) | day <- [minDay .. maxDay]]
        runs = NE.groupBy (\(_, a) (_, b) -> isNothing a == isNothing b) annotated
        triplets = zip3 runs (drop 1 runs) (drop 2 runs)
     in [ Gap
            { gapStart = fst (NE.head gap)
            , gapLength = length gap
            , gapBefore = fst (NE.last before)
            , gapAfter = fst (NE.head after)
            }
        | (before, gap, after) <- triplets
        , isNothing (snd (NE.head gap))
        ]

summarize ::
    MoodEntry ->
    [MoodEntry] ->
    (Day, Day, Map.Map Day MoodEntry)
summarize first rest =
    let (mn, mx, byDate) = foldl' step (d, d, seed) rest
     in (mn, mx, byDate)
  where
    d = entryDate first
    seed = Map.singleton d first
    step (mn, mx, byDate) x =
        ( min mn (entryDate x)
        , max mx (entryDate x)
        , Map.insert (entryDate x) x byDate
        )

-- | Extract present rows as vectors for FCM, plus a row-index-to-Day map.
--
-- Only rows where all 5 dimensions are present are included.
extractPresentRows :: D.DataFrame -> (V.Vector (V.Vector Double), Map.Map Int Day)
extractPresentRows df =
    let dimNames = ["sleep", "anxiety", "sensitivity", "outlook", "speed"] :: [Text.Text]
        dimCols = map (getColMaybes df) dimNames
        dateTexts = getColTexts df "date"
        rows = zip dateTexts (transpose dimCols)
        present =
            [ (V.fromList vs, readDay dt)
            | (dt, vals) <- rows
            , Just vs <- [sequence vals]
            ]
        (vecs, days) = unzip present
     in (V.fromList vecs, Map.fromList (zip [0 ..] days))

-- | Reconstruct MoodEntries from a date-spine DataFrame (only present rows).
extractEntries :: D.DataFrame -> [MoodEntry]
extractEntries df =
    let dimNames = ["sleep", "anxiety", "sensitivity", "outlook", "speed"] :: [Text.Text]
        dimTypes = [Sleep, Anxiety, Sensitivity, Outlook, Speed]
        dimCols = map (getColMaybes df) dimNames
        dateTexts = getColTexts df "date"
        rows = zip dateTexts (transpose dimCols)
     in mapMaybe (buildEntry dimTypes) rows
  where
    buildEntry dims (dt, vals) = do
        vs <- sequence vals
        pure
            MoodEntry
                { entryDate = readDay dt
                , entryDimensions = Map.fromList (zip dims vs)
                }

getColMaybes :: D.DataFrame -> Text.Text -> [Maybe Double]
getColMaybes df name = D.columnAsList (D.col name :: D.Expr (Maybe Double)) df

getColTexts :: D.DataFrame -> Text.Text -> [Text.Text]
getColTexts df name = D.columnAsList (D.col name :: D.Expr Text.Text) df

readDay :: Text.Text -> Day
readDay = read . Text.unpack

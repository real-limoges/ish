module Ish.Analysis.DataFrame (
    fillMissingDates,
    identifyGaps,
) where

import Data.List (foldl', groupBy)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import Data.Text qualified as Text
import Data.Time.Calendar (Day)
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
                    , lookupDim MentalSpeed day : ms
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
            , ("mental_speed", D.fromList msCol)
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
        runs = groupBy (\(_, a) (_, b) -> isNothing a == isNothing b) annotated
        triplets = zip3 runs (tail runs) (drop 2 runs)
     in [ Gap
            { gapStart = fst (head gap)
            , gapLength = length gap
            , gapBefore = fst (last before)
            , gapAfter = fst (head after)
            }
        | (before, gap, after) <- triplets
        , isNothing (snd (head gap))
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

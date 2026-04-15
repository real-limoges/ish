module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import Ish.Analysis.ClusterSpec (clusterTests)
import Ish.Analysis.DataFrameSpec (dataFrameTests)
import Ish.Analysis.FuzzifySpec (fuzzifyTests)
import Ish.Analysis.FuzzySpec (fuzzyTests)
import Ish.Analysis.GapsSpec (gapsTests)
import Ish.Analysis.MamdaniSpec (mamdaniTests)
import Ish.DbSpec (dbTests)
import Ish.TypesSpec (typesTests)

main :: IO ()
main =
    defaultMain $
        testGroup
            "Ish"
            [ dbTests
            , typesTests
            , dataFrameTests
            , fuzzifyTests
            , clusterTests
            , gapsTests
            , mamdaniTests
            , fuzzyTests
            ]

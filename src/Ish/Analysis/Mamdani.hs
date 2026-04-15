module Ish.Analysis.Mamdani (
    runMamdani,
    ruleDefToFuzzyRule,
    traceToResponse,
) where

import Hazy (
    FuzzyRule (..),
    InferenceTrace (..),
    mamdaniTrace,
 )

import Ish.Analysis.Fuzzify (buildFIS)
import Ish.Types (
    MamdaniRequest (..),
    MamdaniResponse (..),
    RuleDef (..),
 )

-- | Pure driver: take a MamdaniRequest, build a one-shot FIS from its MFs
--   and rules, run Hazy's traced Mamdani inference, and reshape the trace
--   into the wire-format response.
runMamdani :: MamdaniRequest -> MamdaniResponse
runMamdani req =
    let rules = map ruleDefToFuzzyRule (mrRules req)
        fis = buildFIS rules (mrDefs req)
        trace = mamdaniTrace fis (mrValues req)
     in traceToResponse trace

ruleDefToFuzzyRule :: RuleDef -> FuzzyRule
ruleDefToFuzzyRule r = FuzzyRule (ruleDefIf r) (ruleDefThen r)

traceToResponse :: InferenceTrace -> MamdaniResponse
traceToResponse t =
    MamdaniResponse
        { mrsInputDegrees = traceInputDegrees t
        , mrsRuleStrengths = traceRuleStrengths t
        , mrsOutputCurves = traceOutputCurves t
        , mrsCrisp = traceCrisp t
        }

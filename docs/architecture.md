# Ish Servant Architecture Guide

## Context

Ish is a Servant-based mood tracking API. The DB is pre-seeded by `data/load.py` with ~1,073 days of mood data across 5 dimensions (Sleep, Anxiety, Sensitivity, Outlook, Speed) spanning 2022–present. The API serves that data and runs fuzzy analysis over it using the `hazy` library. The data pipeline is effectively read-only for mood entries; the one piece of mutable runtime state is the membership-function definitions (editable via `POST /membership-functions`).

---

## The Data Pipeline (End to End)

```
load.py  -->  SQLite (5 REAL columns)  -->  fetchEntries conn mFrom mTo
                                                   |
                                             [MoodEntry]
                                                   |
                                           fillMissingDates       (Analysis/DataFrame.hs)
                                                   |
                                           DataFrame (Maybe Double per column, gaps = Nothing)
                                                   |
                           +-----------------------+------------------+----------------+
                           |                       |                  |                |
                    fuzzifyEntries           clusterMoodData     analyzeGaps      (raw passthrough)
                    (Analysis/Fuzzify)       (Analysis/Cluster)  (Analysis/Gaps)        |
                           |                       |                  |                |
                    +wellbeing/activation     ClusterResult       GapAnalysis      [MoodEntry]
                           |                       |                  |                |
                           v                       v                  v                v
                   analyzeMoodEntries /       POST /cluster       GET /gaps       GET /data
                   clusterEntries             GET /analysis/clusters              GET /entries
                   (Analysis/Fuzzy.hs)
                           |
                   AnalysisResult / [MoodCluster]
                           |
                   GET /analysis, GET /analysis/clusters
```

Every data-derived endpoint (`/entries`, `/data`, `/analysis`, `/analysis/clusters`, `/cluster`, `/gaps`, `/membership-functions/suggest`) accepts optional `from` and `to` query params (`YYYY-MM-DD`). They map to `Maybe Day` and are forwarded to `fetchEntries conn mFrom mTo`; either bound may be omitted to leave that side open, and both bounds are inclusive. `/health`, `GET /membership-functions`, `POST /membership-functions`, and `POST /inference/mamdani` take no dates — they're config or stateless inference, not data scoped.

Independent of the data pipeline, `GET /membership-functions` reads `envMembershipFns :: IORef MembershipFuncDefs` and `POST /membership-functions` writes it. Every analysis handler that runs the pipeline calls `currentFis :: AppM FIS` (in `Analysis/Server.hs`) which reads the ref and runs `buildFIS` once per request, so edits propagate to `/analysis`, `/analysis/clusters`, `/cluster`, and `/gaps` on the next call. `POST /membership-functions/suggest` returns a percentile-anchored candidate derived from the current DataFrame without mutating the ref — the caller applies it via `POST /membership-functions` if accepted.

---

## Ish Types (src/Ish/Types.hs)

| Type | Shape | Notes |
|------|-------|-------|
| `MoodDimension` | `Sleep \| Anxiety \| Sensitivity \| Outlook \| Speed` | Enum, Bounded. JSON as lowercase text; used as `ToJSONKey`/`FromJSONKey`. |
| `MoodEntry` | `{ entryDate :: Day, entryDimensions :: Map MoodDimension Double }` | One per day. |
| `Gap` | `{ gapStart :: Day, gapLength :: Int, gapBefore :: Day, gapAfter :: Day }` | A run of absent days with neighboring present-day context. |
| `FuzzyLabel` | `{ labelName :: Text, labelMembership :: Degree }` | A named fuzzy conclusion with its strength. `Degree` is re-exported from Hazy. |
| `MoodCluster` | `{ clusterName :: Text, clusterCentroid :: Map MoodDimension Double, clusterSize :: Int, clusterLabels :: [FuzzyLabel] }` | A group of similar entries. |
| `AnalysisResult` | `{ analysisClusters :: [MoodCluster], analysisSummary :: [FuzzyLabel] }` | Top-level analysis response. |
| `TermDef` | `{ termName :: Text, termParams :: (Double, Double, Double) }` | Triangular term `(left foot, peak, right foot)`. JSON-serializable shape for editing. |
| `VarDef` | `{ varName :: Text, varBounds :: (Double, Double), varTerms :: [TermDef] }` | JSON-serializable linguistic variable. Bridges to Hazy's `LinguisticVar` via `buildVars`. |
| `MembershipFuncDefs` | `{ mfdInputs :: [VarDef], mfdOutputs :: [VarDef] }` | The mutable runtime state held in `envMembershipFns :: IORef MembershipFuncDefs`. |
| `RuleDef` | `{ ruleDefIf :: [(Text, Text)], ruleDefThen :: [(Text, Text)] }` | JSON-serializable Mamdani rule. Each side is a list of `(varName, termName)` pairs. Bridges to Hazy's `FuzzyRule`. |
| `MamdaniRequest` | `{ mrDefs :: MembershipFuncDefs, mrRules :: [RuleDef], mrValues :: Map Text Double }` | Body of `POST /inference/mamdani`. Caller supplies MFs, rules, and crisp input values for one stateless inference. |
| `MamdaniResponse` | `{ mrsInputDegrees :: Map Text (Map Text Double), mrsRuleStrengths :: [Double], mrsOutputCurves :: Map Text [(Double, Double)], mrsCrisp :: Map Text Double }` | Mirrors Hazy's `InferenceTrace`. Output curves are `[(x, y)]` pairs ready to splat into d3 scales. |

---

## Hazy's Actual API

Hazy is a pure fuzzy logic library. `import Hazy` re-exports everything below.

### Core Types (`Hazy.Core.Types`)

```haskell
type Degree = Double                          -- membership degree in [0,1]
type MembershipFn = Double -> Degree          -- just a function
data FuzzySet = FuzzySet
    { fsName     :: Text
    , fsMf       :: MembershipFn
    , fsUniverse :: (Double, Double)           -- domain bounds
    }
clampDegree :: Double -> Degree               -- clamp to [0,1]
```

### Membership Functions (`Hazy.Core.Membership`)

All return `MembershipFn` (i.e., `Double -> Degree`):

```haskell
triangular  :: Double -> Double -> Double -> MembershipFn            -- (a, peak, c)
trapezoidal :: Double -> Double -> Double -> Double -> MembershipFn  -- (a, b, c, d)
gaussian    :: Double -> Double -> MembershipFn                      -- (mu, sigma)
sigmoid     :: Double -> Double -> MembershipFn                      -- (center, slope)
```

### T-Norms / S-Norms (`Hazy.Core.TNorm`)

```haskell
class TNorm t where tnorm :: t -> Degree -> Degree -> Degree  -- fuzzy AND
class SNorm s where snorm :: s -> Degree -> Degree -> Degree  -- fuzzy OR

data MinMax       -- TNorm: min,       SNorm: max
data Product      -- TNorm: a*b,       SNorm: a+b-a*b
data Lukasiewicz  -- TNorm: max(0,a+b-1), SNorm: min(1,a+b)
```

### Operators (`Hazy.Core.Operators`)

Combine `FuzzySet`s into new `FuzzySet`s:

```haskell
fuzzyAnd  :: TNorm t => t -> FuzzySet -> FuzzySet -> FuzzySet
fuzzyOr   :: SNorm s => s -> FuzzySet -> FuzzySet -> FuzzySet
fuzzyNot  :: FuzzySet -> FuzzySet      -- complement: 1 - mf
very      :: FuzzySet -> FuzzySet      -- concentration: mf^2
somewhat  :: FuzzySet -> FuzzySet      -- dilation: sqrt(mf)
```

### Defuzzification (`Hazy.Core.Defuzzify`)

```haskell
data DefuzzMethod = Centroid | Bisector | MeanOfMaximum
                  | SmallestOfMax | LargestOfMax
                  | Custom ([(FuzzySet, Degree)] -> Double)

defuzzify :: DefuzzMethod -> [(FuzzySet, Degree)] -> Double
```

Takes a list of `(FuzzySet, firingDegree)` pairs and collapses to a crisp value. Uses 200-point sampling over the combined universe.

### Inference (`Hazy.Inference`)

The main abstraction — a complete Fuzzy Inference System:

```haskell
data LinguisticVar = LinguisticVar
    { lvName   :: Text
    , lvTerms  :: Map Text FuzzySet       -- e.g., "low" -> FuzzySet, "high" -> FuzzySet
    , lvBounds :: (Double, Double)
    }

data FuzzyRule = FuzzyRule
    { ruleAntecedent :: [(Text, Text)]    -- [(varName, termName)] e.g., [("energy","high")]
    , ruleConsequent :: [(Text, Text)]    -- [(outVarName, termName)]
    }

data InferenceMethod = Mamdani | Sugeno

data FIS = FIS
    { fisName    :: Text
    , fisInputs  :: Map Text LinguisticVar
    , fisOutputs :: Map Text LinguisticVar
    , fisRules   :: [FuzzyRule]
    , fisMethod  :: InferenceMethod
    }

evaluate :: FIS -> Map Text Double -> Map Text Double
```

`evaluate` is the whole pipeline: fuzzify inputs -> fire rules (min t-norm for AND) -> aggregate -> defuzzify (Centroid for Mamdani, weighted average for Sugeno). Pure function, no IO.

**Mamdani** clips consequent fuzzy sets by rule firing strength, then defuzzifies via Centroid.
**Sugeno** (zero-order) takes weighted average of consequent midpoints.

---

## How Hazy Maps to Ish

The real bridge between ish's data and hazy lives in `src/Ish/Analysis/Fuzzify.hs`. `src/Ish/Analysis/Fuzzy.hs` is the thin orchestrator above it — it composes `fuzzifyEntries` + `clusterMoodData` into `analyzeMoodEntries` / `clusterEntries`.

### Current path (implemented)

```
DataFrame (raw columns: Maybe Double per dimension)
    ↓ fuzzifyEntries reads MembershipFuncDefs from the AppEnv IORef
    ↓ evaluates per-row membership degrees column-by-column via hazy's MembershipFn
DataFrame (+ derived wellbeing, activation columns while keeping raw)
    ↓ clusterMoodData (wraps Hazy's Fuzzy C-Means)
ClusterResult { resultClusters :: [MoodCluster], centers, membership matrix }
```

Hazy's FCM is what ish is using for clustering today — `clusterMoodData` in `src/Ish/Analysis/Cluster.hs` is a thin wrapper around it. Centroids come back as points in the fuzzified space and are translated into `MoodCluster`s (name, centroid, size, labels).

### Stateless Mamdani inference (`POST /inference/mamdani`)

Hazy exposes `evaluate :: FIS -> Map Text Double -> Map Text Double` (and a traced variant `mamdaniTrace`) for Mamdani/Sugeno inference. Ish wires this as a stateless one-shot endpoint in `src/Ish/Analysis/Mamdani.hs` (`runMamdani`): the caller supplies a `MamdaniRequest` with `MembershipFuncDefs`, `[RuleDef]`, and a `Map Text Double` of input values, and gets back a `MamdaniResponse` carrying input degrees, rule firing strengths, output curves (as `[(x, y)]` arrays for direct d3 consumption), and crisp defuzzified outputs.

No `FIS` is held in `AppEnv` — each request builds a one-shot FIS via `buildFIS` from the request body. There is no rule-driven analysis over the stored time series; that would require persistent `LinguisticVar`s, persistent `FuzzyRule`s, and a handler that evaluates them across the DataFrame, none of which currently exist. The endpoint is the surface that the LiveView "rules sandbox" interacts with — it lets the frontend explore rule shapes without needing the server to remember anything between calls.

---

## Servant Wiring

| Layer | File | What It Does |
|-------|------|--------------|
| Types | `src/Ish/Types.hs` | All domain types with JSON instances, including `MembershipFuncDefs` |
| DB | `src/Ish/Db.hs` | SQLite queries, `rowToEntry` maps columns to `MoodDimension` |
| App Monad | `src/Ish/App.hs` | `AppM = ReaderT AppEnv Handler`; `AppEnv` holds Config, Connection, and `IORef MembershipFuncDefs` |
| Entry Routes | `src/Ish/Entries/Api.hs` | `GET /entries` (optional `from`/`to` `Maybe Day` query params) |
| Entry Handlers | `src/Ish/Entries/Server.hs` | Fetches via `Ish.Db.fetchEntries conn mFrom mTo` (single function; both bounds optional and inclusive) |
| Analysis Routes | `src/Ish/Analysis/Api.hs` | `GET /health`; data-derived endpoints `GET /analysis`, `GET /analysis/clusters`, `GET /data`, `POST /cluster`, `GET /gaps`, `POST /membership-functions/suggest` all accept optional `from`/`to` `Maybe Day` query params; `GET /membership-functions`, `POST /membership-functions`, and `POST /inference/mamdani` take no dates |
| Analysis Handlers | `src/Ish/Analysis/Server.hs` | Wires each route to the matching function; `currentFis :: AppM FIS` reads `envMembershipFns` and `buildFIS`s once per request so edits flow through every analysis handler |
| DataFrame | `src/Ish/Analysis/DataFrame.hs` | `fillMissingDates` — builds the date-spine `DataFrame` (`Maybe Double` per column, gaps preserved) |
| Fuzzification | `src/Ish/Analysis/Fuzzify.hs` | `fuzzifyEntries` + `defaultMembershipFuncDefs` — column-by-column membership evaluation via hazy |
| Clustering | `src/Ish/Analysis/Cluster.hs` | `clusterMoodData`, `ClusterConfig`, `ClusterResult` — wraps Hazy's FCM |
| Gap Analysis | `src/Ish/Analysis/Gaps.hs` | `identifyGaps`, `analyzeGaps`, `GapAnalysis` (before/after transitions, imputed memberships) |
| Orchestrator | `src/Ish/Analysis/Fuzzy.hs` | `analyzeMoodEntries` / `clusterEntries` — compose fuzzify + cluster and summarize |
| Mamdani | `src/Ish/Analysis/Mamdani.hs` | `runMamdani` — pure stateless driver: builds a one-shot `FIS` from a `MamdaniRequest`, calls Hazy's `mamdaniTrace`, reshapes the trace into `MamdaniResponse` |
| Combined API | `src/Ish/Api.hs` | `AnalysisApi :<|> EntriesApi`, `hoistServer` bridges `AppM ~> Handler` |
| Main | `app/Main.hs` | Reads `PORT` / `ISH_PORT` / `ISH_DB_PATH`, seeds the `MembershipFuncDefs` `IORef`, starts Warp |

**Servant notes:**
- `:<|>` ordering in server MUST match the API type
- Query params (`from`/`to`) are `Maybe` — pattern match on presence
- All handlers return `AppM SomeType`, not `Handler`
- `app/Main.hs` resolves the port in the order `PORT` (Cloud Run) → `ISH_PORT` → `8080`

---

## Key Files

| File | Role |
|------|------|
| `src/Ish/Types.hs` | All data types — `MoodDimension`, `MoodEntry`, `FuzzyLabel`, `MoodCluster`, `AnalysisResult`, `MembershipFuncDefs` |
| `src/Ish/Db.hs` | SQLite operations, `rowToEntry` maps columns to `MoodDimension` |
| `src/Ish/Analysis/DataFrame.hs` | `fillMissingDates` — the sparse-time-series backbone; every downstream step starts here |
| `src/Ish/Analysis/Fuzzify.hs` | **Where the hazy bridge actually lives** — `fuzzifyEntries`, `defaultMembershipFuncDefs`, `suggestMembershipFuncDefs` (percentile-anchored), and `buildFIS` / `buildMoodFIS` for the Mamdani endpoint |
| `src/Ish/Analysis/Cluster.hs` | Wraps Hazy's FCM into `clusterMoodData` |
| `src/Ish/Analysis/Gaps.hs` | Gap entities, before/after transitions, fuzzy imputation across gaps |
| `src/Ish/Analysis/Fuzzy.hs` | Orchestrator: `analyzeMoodEntries` / `clusterEntries` compose fuzzify + cluster |
| `src/Ish/Analysis/Mamdani.hs` | Stateless one-shot Mamdani driver behind `POST /inference/mamdani` |
| `src/Ish/Analysis/Server.hs` | Handler layer — wired for every `AnalysisApi` route |
| `data/load.py` | CSV-to-SQLite loader (run first) |
| `data/ish_data.csv` | Raw mood data |

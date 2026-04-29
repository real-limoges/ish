# ish

A Haskell [Servant](https://hackage.haskell.org/package/servant) API that serves mood exploration data backed by fuzzy math.

`ish` is one of three cooperating layers:

- **[Hazy](https://github.com/real-limoges/hazy)** — pure Haskell fuzzy-logic library (membership functions, t-norms, defuzzification, Fuzzy C-Means). No domain knowledge, no IO. Pulled in via `cabal.project` as a `source-repository-package`; bump the tag there to upgrade.
- **ish** (this repo) — Servant application. Owns all mood-domain knowledge: the 5 dimensions (Sleep 0–15, Anxiety 0–5, Sensitivity 0–5, Outlook 0–10, Speed 0–10), sparse time-series handling, gap semantics, fuzzification config, and clustering orchestration. Calls Hazy for math.
- **Phoenix LiveView** — separate Elixir frontend that consumes this API.

The layering is strict: never leak mood-domain logic into Hazy, never put fuzzy math primitives into ish. New math goes to Hazy first.

## Requirements

- GHC **9.12.2**
- cabal **3.14**
- Python 3 (only for the one-time data load)

CI runs in the `haskell:9.12.2` Docker container.

## Quick start

```sh
# 1. Build
cabal update
cabal build all

# 2. Populate the SQLite database from the CSV (one time)
python3 data/load.py

# 3. Run the server
cabal run ish
```

The server listens on `PORT` (Cloud Run convention) → `ISH_PORT` → `8080`. The DB path is `ISH_DB_PATH`, defaulting to `data/ish.db`. The DB must exist before the server can answer queries — `data/load.py` ingests `data/ish_data.csv` into it. The Dockerfile runs the loader at image-build time and then deletes the CSV.

## Endpoints

All data-derived endpoints accept optional `from` / `to` query params (`YYYY-MM-DD`, inclusive, either side may be omitted).

| Method | Path | Notes |
|---|---|---|
| `GET`  | `/health` | Liveness check |
| `GET`  | `/entries` | Raw mood entries |
| `GET`  | `/data` | Sparse date-spine with raw values |
| `GET`  | `/analysis` | Full fuzzify → cluster → summarize pipeline |
| `GET`  | `/analysis/clusters` | Cluster list from default config |
| `POST` | `/cluster` | Run FCM with caller-supplied `ClusterConfig` (k, m) |
| `GET`  | `/gaps` | Gap analysis with before/after transitions |
| `GET`  | `/membership-functions` | Current `MembershipFuncDefs` |
| `POST` | `/membership-functions` | Update `MembershipFuncDefs` (live for next request) |
| `POST` | `/membership-functions/suggest` | Percentile-anchored suggestion derived from current data |
| `POST` | `/inference/mamdani` | Stateless Mamdani inference |

## Architecture

Entry point is `app/Main.hs`: it reads env vars, opens the SQLite connection, seeds an `IORef` of `MembershipFuncDefs` with `defaultMembershipFuncDefs`, builds an `AppEnv`, and hands it to `Ish.Api.app` under Warp.

The app monad is `AppM = ReaderT AppEnv Handler` (see `src/Ish/App.hs`). Every handler returns `AppM a`, and `Ish.Api` uses `hoistServer` with `runAppM` to bridge `AppM ~> Handler` for Servant. The combined API is `AnalysisApi :<|> EntriesApi` in `src/Ish/Api.hs` — the server-side `:<|>` ordering must match the API-type ordering exactly, or Servant will type-check but silently misroute.

| Route module | Handler module | Endpoints |
|---|---|---|
| `Ish.Entries.Api` | `Ish.Entries.Server` | `GET /entries` (optional `from`/`to`) |
| `Ish.Analysis.Api` | `Ish.Analysis.Server` | `/health`, plus the data-derived analysis routes (all accepting `from`/`to`) and the membership-function config routes |

The data pipeline inside the analysis handlers:

1. `Ish.Db.fetchEntries conn mFrom mTo` → `[MoodEntry]`. The two `Maybe Day` bounds are inclusive; either may be `Nothing` to leave that side open.
2. `Ish.Analysis.DataFrame.fillMissingDates` builds a date-spine `DataFrame` from min-date to max-date with `Maybe Double` columns. Absent days stay `Nothing` and are never silently imputed — gaps are first-class data.
3. `Ish.Analysis.Fuzzify.fuzzifyEntries` adds derived `wellbeing` / `activation` columns by evaluating Hazy membership functions against the current `MembershipFuncDefs` (mutable via the `IORef` in `AppEnv`, so the editor endpoints update at runtime).
4. `Ish.Analysis.Cluster.clusterMoodData` passes the fuzzified frame to Hazy's FCM with a `ClusterConfig` (k, m) and returns centers + membership matrix.
5. `Ish.Analysis.Gaps.identifyGaps` derives gap entities (start, length, last-present-before, first-present-after).

## Project layout

```
app/Main.hs                   -- env, DB, IORef, Warp boot
src/Ish/Api.hs                -- combined AnalysisApi :<|> EntriesApi
src/Ish/App.hs                -- AppM, AppEnv, runAppM
src/Ish/Db.hs                 -- fetchEntries, rowToEntry
src/Ish/Types.hs              -- MoodEntry, MoodCluster, MembershipFuncDefs, ...
src/Ish/Entries/{Api,Server}.hs
src/Ish/Analysis/{Api,Server}.hs
src/Ish/Analysis/DataFrame.hs -- fillMissingDates (date spine, gap-preserving)
src/Ish/Analysis/Fuzzify.hs   -- fuzzifyEntries, defaultMembershipFuncDefs, suggestMembershipFuncDefs
src/Ish/Analysis/Cluster.hs   -- clusterMoodData (wraps Hazy's FCM)
src/Ish/Analysis/Gaps.hs      -- identifyGaps, analyzeGaps, GapAnalysis
src/Ish/Analysis/Fuzzy.hs     -- analyzeMoodEntries, clusterEntries (orchestrator)
src/Ish/Analysis/Mamdani.hs   -- runMamdani
data/load.py                  -- CSV → SQLite loader (source of truth for schema)
data/ish_data.csv             -- raw mood data
test/Main.hs                  -- tasty + tasty-hunit
```

## Tests

```sh
cabal test --test-show-details=direct
cabal test --test-options="-p <substring>"   # filter by tasty path
```

## Conventions and gotchas

- SQLite schema is defined by `data/load.py` — the loader is the source of truth, there is no migration system. Range constraints are enforced there; `Ish.Db.rowToEntry` assumes the columns exist with those names.
- `cabal.project` has `allow-newer` pins and disables `crypton` in `wai-app-static` — these exist because the GHC 9.12 / base 4.21 combination breaks several upstream bounds.
- `*.db` and `data/ish.db` are gitignored. Never commit a populated database.

## Deployment

GitHub Actions → Cloud Run, via the reusable workflow at `real-limoges/real-complex/.github/workflows/deploy-service.yml`. The `Deploy` workflow calls `ci.yml` first, so a red CI blocks deploy.

## Guiding principles

- **Emergence over prescription.** Don't predefine mood categories or write rules from assumptions. Let structure surface from the data.
- **Gaps are data, not noise.** Missing days carry information. Make them visible, never silently impute.
- **Separation of concerns.** Hazy is reusable fuzzy math. ish owns the mood domain. LiveView owns presentation. If logic is in the wrong layer, move it.
- **Soft boundaries everywhere.** Every day belongs to multiple clusters; visualizations should always show degrees, not hard assignments.

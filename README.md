# ish

A Haskell [Servant](https://hackage.haskell.org/package/servant) API that serves mood exploration data backed by fuzzy math.

`ish` is one of three cooperating layers:

- **[Hazy](https://github.com/real-limoges/hazy)** — pure Haskell fuzzy-logic library (membership functions, t-norms, defuzzification, Fuzzy C-Means). No domain knowledge, no IO. Pulled in via `cabal.project` as a `source-repository-package`.
- **ish** (this repo) — Servant application. Owns all mood-domain knowledge: the 5 dimensions (Sleep 0–15, Anxiety 0–5, Sensitivity 0–5, Outlook 0–10, Speed 0–10), sparse time-series handling, gap semantics, fuzzification config, and clustering orchestration.
- **Phoenix LiveView** — separate Elixir frontend that consumes this API.

## Requirements

- GHC **9.12.2**
- cabal **3.14**
- Python 3 (only for the one-time data load)

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

The server listens on `PORT` (Cloud Run convention) → `ISH_PORT` → `8080`. The DB path is `ISH_DB_PATH`, defaulting to `data/ish.db`.

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

## Tests

```sh
cabal test --test-show-details=direct
cabal test --test-options="-p <substring>"   # filter by tasty path
```

## Documentation

- [`docs/architecture.md`](docs/architecture.md) — Servant wiring table, type catalogue, Hazy API reference, data pipeline diagram.
- [`docs/GUIDE.md`](docs/GUIDE.md) — Project roadmap (remaining Phases 4–6) and guiding principles.
- [`CLAUDE.md`](CLAUDE.md) — Conventions and gotchas for working in the repo.

## Deployment

GitHub Actions → Cloud Run, via the reusable workflow at `real-limoges/real-complex/.github/workflows/deploy-service.yml`. The `Deploy` workflow calls `ci.yml` first, so a red CI blocks deploy. The Dockerfile runs `data/load.py` at image-build time and then deletes the CSV.

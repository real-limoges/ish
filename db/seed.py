#!/usr/bin/env python3
"""
Seed the ish mood database with 365 days of realistic mood data.

Usage:
    python3 db/seed.py [db_path]     (default: db/ish.db)

The generator produces correlated, drift-based data with:
  - Weekly cycles  (weekends differ from weekdays)
  - Gradual drift  (mood baselines shift over weeks)
  - Random shocks  (occasional bad/great days)
  - Dimension correlations (energy↔focus, anxiety↔valence inverse)
"""

import math
import random
import sqlite3
import sys
from datetime import date, timedelta
from pathlib import Path

DB_PATH = sys.argv[1] if len(sys.argv) > 1 else "db/ish.db"
SCHEMA  = Path(__file__).parent / "schema.sql"
DAYS    = 365
START   = date(2025, 3, 15)
SEED    = 42

def clamp(v: float) -> float:
    return max(0.0, min(1.0, v))

def generate_entries(n_days: int, start: date, rng: random.Random):
    """Generate n_days of mood entries with realistic patterns."""
    # Baselines — where each dimension tends to hover
    base = {"energy": 0.55, "valence": 0.60, "anxiety": 0.35, "focus": 0.50, "sociability": 0.45}

    # Slow-moving drift per dimension
    drift = {k: 0.0 for k in base}

    entries = []
    for i in range(n_days):
        d = start + timedelta(days=i)
        dow = d.weekday()  # 0=Mon ... 6=Sun
        is_weekend = dow >= 5

        # --- Drift: random walk that mean-reverts ---
        for dim in base:
            drift[dim] += rng.gauss(0, 0.03)
            drift[dim] *= 0.93  # mean-revert toward 0

        # --- Seasonal cycle (gentle sine over the year) ---
        year_phase = (i / 365) * 2 * math.pi
        seasonal = {
            "energy":      0.06 * math.sin(year_phase),          # higher in summer
            "valence":     0.08 * math.sin(year_phase + 0.3),
            "anxiety":    -0.05 * math.sin(year_phase),          # lower in summer
            "focus":       0.03 * math.cos(year_phase),
            "sociability": 0.07 * math.sin(year_phase + 0.5),
        }

        # --- Weekly cycle ---
        weekly = {
            "energy":       0.08 if is_weekend else -0.02,
            "valence":      0.06 if is_weekend else  0.00,
            "anxiety":     -0.10 if is_weekend else  0.03,
            "focus":       -0.05 if is_weekend else  0.04,
            "sociability":  0.12 if is_weekend else -0.03,
        }

        # --- Monday blues ---
        if dow == 0:
            weekly["energy"]  -= 0.06
            weekly["valence"] -= 0.05
            weekly["anxiety"] += 0.05

        # --- Friday lift ---
        if dow == 4:
            weekly["valence"]     += 0.04
            weekly["sociability"] += 0.06

        # --- Random shock (≈5% of days) ---
        shock = {k: 0.0 for k in base}
        if rng.random() < 0.05:
            # Bad day or great day
            direction = rng.choice([-1, 1])
            magnitude = rng.uniform(0.10, 0.25)
            # Affect 2-3 dimensions
            affected = rng.sample(list(base.keys()), k=rng.randint(2, 3))
            for dim in affected:
                # Anxiety moves opposite for "good" shocks
                if dim == "anxiety":
                    shock[dim] = -direction * magnitude
                else:
                    shock[dim] = direction * magnitude

        # --- Compose ---
        row = {"date": d.isoformat()}
        for dim in base:
            noise = rng.gauss(0, 0.06)
            val = base[dim] + drift[dim] + seasonal[dim] + weekly[dim] + shock[dim] + noise
            row[dim] = round(clamp(val), 4)

        # --- Correlation adjustments ---
        # Energy and focus are positively correlated
        avg_ef = (row["energy"] + row["focus"]) / 2
        row["energy"] = round(clamp(0.7 * row["energy"] + 0.3 * avg_ef), 4)
        row["focus"]  = round(clamp(0.7 * row["focus"]  + 0.3 * avg_ef), 4)

        # Anxiety and valence are inversely correlated
        row["valence"] = round(clamp(row["valence"] - 0.2 * (row["anxiety"] - base["anxiety"])), 4)

        entries.append(row)

    return entries

def main():
    rng = random.Random(SEED)
    entries = generate_entries(DAYS, START, rng)

    db_path = Path(DB_PATH)
    db_path.parent.mkdir(parents=True, exist_ok=True)

    # Remove existing DB to start fresh
    if db_path.exists():
        db_path.unlink()

    conn = sqlite3.connect(str(db_path))
    conn.execute("PRAGMA journal_mode=WAL")

    # Apply schema
    conn.executescript(SCHEMA.read_text())

    # Insert entries
    conn.executemany(
        "INSERT INTO mood_entries (date, energy, valence, anxiety, focus, sociability) "
        "VALUES (:date, :energy, :valence, :anxiety, :focus, :sociability)",
        entries,
    )
    conn.commit()

    # Quick sanity check
    count = conn.execute("SELECT COUNT(*) FROM mood_entries").fetchone()[0]
    first = conn.execute("SELECT * FROM mood_entries ORDER BY date LIMIT 1").fetchone()
    last  = conn.execute("SELECT * FROM mood_entries ORDER BY date DESC LIMIT 1").fetchone()

    print(f"Seeded {count} entries into {db_path}")
    print(f"  First: {first}")
    print(f"  Last:  {last}")

    conn.close()

if __name__ == "__main__":
    main()

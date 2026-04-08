#!/usr/bin/env python3
"""Load ish_data.csv into a SQLite database (data/ish.db)."""

import csv
import sqlite3
from datetime import date
from pathlib import Path

DATA_DIR = Path(__file__).parent
CSV_PATH = DATA_DIR / "ish_data.csv"
DB_PATH = DATA_DIR / "ish.db"

CREATE_TABLE = """
CREATE TABLE IF NOT EXISTS mood_entries (
    date        TEXT PRIMARY KEY,
    sleep       REAL NOT NULL,
    speed       REAL NOT NULL,
    anxiety     REAL NOT NULL,
    sensitivity REAL NOT NULL,
    outlook     REAL NOT NULL
);
"""

INSERT = """
INSERT OR REPLACE INTO mood_entries (date, sleep, speed, anxiety, sensitivity, outlook)
VALUES (?, ?, ?, ?, ?, ?);
"""


START_DATE = date(2022, 4, 1)


def main():
    conn = sqlite3.connect(DB_PATH)
    conn.execute(CREATE_TABLE)
    today = date.today()

    with open(CSV_PATH, newline="") as f:
        reader = csv.DictReader(f)
        rows = []
        skipped = 0
        for row in reader:
            if not row["date"].strip():
                skipped += 1
                continue
            entry_date = date.fromisoformat(row["date"])
            if entry_date < START_DATE or entry_date > today:
                skipped += 1
                continue
            rows.append(
                (
                    row["date"],
                    float(row["sleep"]),
                    float(row["speed"]),
                    float(row["anxiety"]),
                    float(row["sensitivity"]),
                    float(row["outlook"]),
                )
            )

    conn.executemany(INSERT, rows)
    conn.commit()
    print(f"Loaded {len(rows)} rows into {DB_PATH} (skipped {skipped} out-of-range)")
    conn.close()


if __name__ == "__main__":
    main()
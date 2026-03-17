-- ish mood database schema
-- One row per day, five dimensions scored [0, 1].

CREATE TABLE IF NOT EXISTS mood_entries (
    date         TEXT    PRIMARY KEY,   -- ISO 8601 date (YYYY-MM-DD)
    energy       REAL    NOT NULL CHECK (energy       >= 0 AND energy       <= 1),
    valence      REAL    NOT NULL CHECK (valence      >= 0 AND valence      <= 1),
    anxiety      REAL    NOT NULL CHECK (anxiety      >= 0 AND anxiety      <= 1),
    focus        REAL    NOT NULL CHECK (focus        >= 0 AND focus        <= 1),
    sociability  REAL    NOT NULL CHECK (sociability  >= 0 AND sociability  <= 1)
);

-- Fast lookups by date range.
CREATE INDEX IF NOT EXISTS idx_mood_entries_date ON mood_entries (date);

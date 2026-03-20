-- ============================================================
-- VT Hockey Data Collection App - Supabase Database Schema
-- ============================================================
-- Instructions:
--   1. Go to your Supabase project dashboard
--   2. Click "SQL Editor" in the left sidebar
--   3. Paste this entire file and click "Run"
-- ============================================================

-- -------------------------------------------------------
-- REFERENCE TABLES (populated before the season starts)
-- -------------------------------------------------------

CREATE TABLE IF NOT EXISTS players (
  id          SERIAL PRIMARY KEY,
  jersey_num  SMALLINT NOT NULL,
  full_name   TEXT NOT NULL,
  position    TEXT CHECK (position IN ('F', 'D', 'G')),
  active      BOOLEAN DEFAULT true,
  created_at  TIMESTAMPTZ DEFAULT now()
);

CREATE TABLE IF NOT EXISTS games (
  id          SERIAL PRIMARY KEY,
  game_date   DATE NOT NULL,
  opponent    TEXT NOT NULL,
  location    TEXT DEFAULT 'home' CHECK (location IN ('home', 'away', 'neutral')),
  season      TEXT NOT NULL DEFAULT '2025-26',
  notes       TEXT,
  created_at  TIMESTAMPTZ DEFAULT now()
);

-- -------------------------------------------------------
-- EVENT TABLES (filled in during games)
-- -------------------------------------------------------

-- Faceoffs: who took it, which zone, did we win?
CREATE TABLE IF NOT EXISTS faceoffs (
  id              SERIAL PRIMARY KEY,
  game_id         INTEGER REFERENCES games(id) ON DELETE CASCADE,
  period          TEXT NOT NULL CHECK (period IN ('1', '2', '3', 'OT', 'OT2', 'OT3', 'SO')),
  game_time       TEXT,                          -- stored as 'MM:SS', e.g. '14:23'
  player_id       INTEGER REFERENCES players(id),
  result          TEXT CHECK (result IN ('win', 'loss')),
  zone            TEXT CHECK (zone IN ('offensive', 'defensive', 'neutral')),
  collector_name  TEXT NOT NULL,
  created_at      TIMESTAMPTZ DEFAULT now()
);

-- Hits: who delivered it (or received it)
CREATE TABLE IF NOT EXISTS hits (
  id              SERIAL PRIMARY KEY,
  game_id         INTEGER REFERENCES games(id) ON DELETE CASCADE,
  period          TEXT NOT NULL CHECK (period IN ('1', '2', '3', 'OT', 'OT2', 'OT3', 'SO')),
  game_time       TEXT,
  player_id       INTEGER REFERENCES players(id),
  hit_type        TEXT DEFAULT 'delivered' CHECK (hit_type IN ('delivered', 'received')),
  collector_name  TEXT NOT NULL,
  created_at      TIMESTAMPTZ DEFAULT now()
);

-- Shot attempts (save/miss/block - does NOT include goals, those go in goals table)
CREATE TABLE IF NOT EXISTS shots (
  id              SERIAL PRIMARY KEY,
  game_id         INTEGER REFERENCES games(id) ON DELETE CASCADE,
  period          TEXT NOT NULL CHECK (period IN ('1', '2', '3', 'OT', 'OT2', 'OT3', 'SO')),
  game_time       TEXT,
  player_id       INTEGER REFERENCES players(id),
  shot_type       TEXT CHECK (shot_type IN ('wrist', 'slap', 'backhand', 'snap', 'tip', 'other')),
  result          TEXT CHECK (result IN ('save', 'missed', 'blocked', 'goal')),
  team            TEXT DEFAULT 'VT' CHECK (team IN ('VT', 'opponent')),
  -- Shot map coordinates (feet on ice, 0-200 x-axis, 0-85 y-axis)
  -- VT attacks toward x = 200 (right side of rink)
  x_coord         NUMERIC(6, 2),
  y_coord         NUMERIC(6, 2),
  collector_name  TEXT NOT NULL,
  created_at      TIMESTAMPTZ DEFAULT now()
);

-- Goals: scorer, up to 2 assists, situation
CREATE TABLE IF NOT EXISTS goals (
  id                   SERIAL PRIMARY KEY,
  game_id              INTEGER REFERENCES games(id) ON DELETE CASCADE,
  period               TEXT NOT NULL CHECK (period IN ('1', '2', '3', 'OT', 'OT2', 'OT3', 'SO')),
  game_time            TEXT,
  scorer_id            INTEGER REFERENCES players(id),
  assist1_id           INTEGER REFERENCES players(id),
  assist2_id           INTEGER REFERENCES players(id),
  situation            TEXT DEFAULT 'even' CHECK (situation IN ('even', 'power_play', 'short_handed', 'penalty_shot')),
  empty_net            BOOLEAN DEFAULT false,
  team                 TEXT DEFAULT 'VT' CHECK (team IN ('VT', 'opponent')),
  collector_name       TEXT NOT NULL,
  created_at           TIMESTAMPTZ DEFAULT now()
);

-- On-ice players at time of a goal (one row per player)
-- Link to goals.id if possible, or record independently
CREATE TABLE IF NOT EXISTS on_ice (
  id              SERIAL PRIMARY KEY,
  game_id         INTEGER REFERENCES games(id) ON DELETE CASCADE,
  goal_id         INTEGER REFERENCES goals(id) ON DELETE CASCADE,
  period          TEXT NOT NULL CHECK (period IN ('1', '2', '3', 'OT', 'OT2', 'OT3', 'SO')),
  game_time       TEXT,
  player_id       INTEGER REFERENCES players(id),
  team            TEXT DEFAULT 'VT' CHECK (team IN ('VT', 'opponent')),
  collector_name  TEXT NOT NULL,
  created_at      TIMESTAMPTZ DEFAULT now()
);

-- -------------------------------------------------------
-- ROW LEVEL SECURITY
-- This app uses the anon (public) key so collectors don't
-- need to log in. The policies below allow anyone with
-- the anon key to insert and read data.
-- -------------------------------------------------------

ALTER TABLE players        ENABLE ROW LEVEL SECURITY;
ALTER TABLE games          ENABLE ROW LEVEL SECURITY;
ALTER TABLE faceoffs       ENABLE ROW LEVEL SECURITY;
ALTER TABLE hits           ENABLE ROW LEVEL SECURITY;
ALTER TABLE shots          ENABLE ROW LEVEL SECURITY;
ALTER TABLE goals          ENABLE ROW LEVEL SECURITY;
ALTER TABLE on_ice         ENABLE ROW LEVEL SECURITY;

-- Allow anon to read everything (needed for dropdowns, summary tab)
CREATE POLICY "anon_select_players"  ON players  FOR SELECT TO anon USING (true);
CREATE POLICY "anon_select_games"    ON games    FOR SELECT TO anon USING (true);
CREATE POLICY "anon_select_faceoffs" ON faceoffs FOR SELECT TO anon USING (true);
CREATE POLICY "anon_select_hits"     ON hits     FOR SELECT TO anon USING (true);
CREATE POLICY "anon_select_shots"    ON shots    FOR SELECT TO anon USING (true);
CREATE POLICY "anon_select_goals"    ON goals    FOR SELECT TO anon USING (true);
CREATE POLICY "anon_select_on_ice"   ON on_ice   FOR SELECT TO anon USING (true);

-- Allow anon to insert data (collectors submit events)
CREATE POLICY "anon_insert_faceoffs" ON faceoffs FOR INSERT TO anon WITH CHECK (true);
CREATE POLICY "anon_insert_hits"     ON hits     FOR INSERT TO anon WITH CHECK (true);
CREATE POLICY "anon_insert_shots"    ON shots    FOR INSERT TO anon WITH CHECK (true);
CREATE POLICY "anon_insert_goals"    ON goals    FOR INSERT TO anon WITH CHECK (true);
CREATE POLICY "anon_insert_on_ice"   ON on_ice   FOR INSERT TO anon WITH CHECK (true);

-- Allow coaches/admins to insert games and players (authenticated users only)
CREATE POLICY "auth_insert_games"   ON games   FOR INSERT TO authenticated WITH CHECK (true);
CREATE POLICY "auth_insert_players" ON players FOR INSERT TO authenticated WITH CHECK (true);

-- Allow anon to delete their own records (undo last entry feature)
-- We limit this to records created in the last 5 minutes to prevent accidents
CREATE POLICY "anon_delete_recent_faceoffs" ON faceoffs FOR DELETE TO anon
  USING (created_at > now() - interval '5 minutes');
CREATE POLICY "anon_delete_recent_hits" ON hits FOR DELETE TO anon
  USING (created_at > now() - interval '5 minutes');
CREATE POLICY "anon_delete_recent_shots" ON shots FOR DELETE TO anon
  USING (created_at > now() - interval '5 minutes');
CREATE POLICY "anon_delete_recent_goals" ON goals FOR DELETE TO anon
  USING (created_at > now() - interval '5 minutes');
CREATE POLICY "anon_delete_recent_on_ice" ON on_ice FOR DELETE TO anon
  USING (created_at > now() - interval '5 minutes');

-- -------------------------------------------------------
-- SAMPLE PLAYERS - Replace with your actual VT roster!
-- You can add more with:
--   INSERT INTO players (jersey_num, full_name, position)
--   VALUES (99, 'Player Name', 'F');
-- -------------------------------------------------------

INSERT INTO players (jersey_num, full_name, position) VALUES
  (1,  'Starter Goalie',    'G'),
  (30, 'Backup Goalie',     'G'),
  (2,  'Defenseman A',      'D'),
  (5,  'Defenseman B',      'D'),
  (7,  'Defenseman C',      'D'),
  (8,  'Defenseman D',      'D'),
  (10, 'Forward A',         'F'),
  (11, 'Forward B',         'F'),
  (12, 'Forward C',         'F'),
  (13, 'Forward D',         'F'),
  (14, 'Forward E',         'F'),
  (15, 'Forward F',         'F'),
  (16, 'Forward G',         'F'),
  (17, 'Forward H',         'F'),
  (18, 'Forward I',         'F'),
  (19, 'Forward J',         'F'),
  (20, 'Forward K',         'F'),
  (21, 'Forward L',         'F'),
  (22, 'Forward M',         'F'),
  (24, 'Forward N',         'F');

-- IMPORTANT: After running this script, update the player names and jersey
-- numbers to match your actual VT roster! You can edit them in the
-- Supabase Table Editor (the spreadsheet-like view in the dashboard).

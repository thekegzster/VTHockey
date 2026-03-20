-- Run in Supabase SQL Editor to support the new On Ice + Goals-from-Shots flow.

-- 1. Link on_ice records to the shot they were recorded during
ALTER TABLE on_ice ADD COLUMN IF NOT EXISTS shot_id INTEGER REFERENCES shots(id);

-- 2. Link goal records back to the shot that triggered them
ALTER TABLE goals ADD COLUMN IF NOT EXISTS shot_id INTEGER REFERENCES shots(id);

-- 3. Make goal_id optional in on_ice (it was previously required for some policies)
ALTER TABLE on_ice ALTER COLUMN goal_id DROP NOT NULL;

-- 4. Allow anon inserts on on_ice (needed for auto-logging from shots tab)
-- Drop first in case it already exists, then recreate cleanly
DROP POLICY IF EXISTS "anon_insert_on_ice" ON on_ice;
CREATE POLICY "anon_insert_on_ice"
  ON on_ice FOR INSERT TO anon WITH CHECK (true);

-- Run in Supabase SQL Editor to add the turnovers (giveaways/takeaways) table.

CREATE TABLE IF NOT EXISTS turnovers (
  id             BIGSERIAL PRIMARY KEY,
  game_id        INTEGER NOT NULL REFERENCES games(id),
  period         TEXT,
  game_time      TEXT,
  player_id      INTEGER REFERENCES players(id),
  type           TEXT CHECK (type IN ('takeaway', 'giveaway')),
  zone           TEXT CHECK (zone IN ('offensive', 'neutral', 'defensive')),
  collector_name TEXT,
  created_at     TIMESTAMPTZ DEFAULT NOW()
);

ALTER TABLE turnovers ENABLE ROW LEVEL SECURITY;

DROP POLICY IF EXISTS "anon_select_turnovers" ON turnovers;
DROP POLICY IF EXISTS "anon_insert_turnovers" ON turnovers;
DROP POLICY IF EXISTS "anon_delete_turnovers" ON turnovers;

CREATE POLICY "anon_select_turnovers" ON turnovers FOR SELECT TO anon USING (true);
CREATE POLICY "anon_insert_turnovers" ON turnovers FOR INSERT TO anon WITH CHECK (true);
CREATE POLICY "anon_delete_turnovers" ON turnovers FOR DELETE TO anon USING (true);

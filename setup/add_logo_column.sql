-- Run this once in Supabase SQL Editor to add logo URL support.
-- After running, go to Table Editor → games and paste logo image
-- URLs into the opponent_logo_url column for each game.

ALTER TABLE games ADD COLUMN IF NOT EXISTS opponent_logo_url TEXT;

-- Example: update a specific game's logo
-- UPDATE games
--   SET opponent_logo_url = 'https://upload.wikimedia.org/wikipedia/commons/.../logo.png'
--   WHERE opponent = 'Penn State';

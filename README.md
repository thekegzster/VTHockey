# VT Hockey Data Collection App

A live game data collection app built with R Shiny and Supabase for the Virginia Tech hockey team.

---

## What it does

During a game, 5–7 collectors each open the app on their phone or laptop. Everyone connects to the same Supabase database in real time.

| Collector Role | Tab to Use |
|---|---|
| Faceoff tracker | Faceoffs tab |
| Hit counter | Hits tab |
| Shot tracker | Shots tab (click rink for location) |
| Goal recorder | Goals tab |
| On-ice tracker (who's on when goals happen) | Goals tab (bottom section) |

---

## Setup Guide (do this once before first use)

### Step 1 — Create a free Supabase project

1. Go to [supabase.com](https://supabase.com) and sign up for free
2. Click "New project", give it a name like `VTHockey`, set a database password
3. Wait ~2 minutes for it to provision

### Step 2 — Run the database schema

1. In your Supabase dashboard, click **SQL Editor** in the left sidebar
2. Click **New query**
3. Copy and paste the entire contents of `setup/schema.sql`
4. Click **Run** (green button)

You should see "Success. No rows returned."

### Step 3 — Add your credentials to the app

1. Find your credentials in Supabase: **Project Settings → API**
   - Copy the **Project URL** (looks like `https://abcxyz.supabase.co`)
   - Copy the **anon / public** key (a long string starting with `eyJ...`)
2. In the `VTHockey` folder, copy `.env.example` and rename the copy to `.env`
3. Open `.env` and paste in your URL and key:
   ```
   SUPABASE_URL=https://your-actual-project.supabase.co
   SUPABASE_ANON_KEY=eyJ...your-actual-key...
   ```

### Step 4 — Install R packages

Run this once in your R console:

```r
install.packages(c(
  "shiny",
  "bslib",
  "shinyjs",
  "httr2",
  "jsonlite",
  "ggplot2",
  "DT",
  "dplyr"
))
```

### Step 5 — Update the roster

The schema file includes placeholder player names. Replace them with your real roster:

**Option A** (easiest): Go to Supabase dashboard → **Table Editor** → `players` table → click the pencil icon to edit rows.

**Option B**: Run SQL in the SQL Editor:
```sql
-- Clear placeholder data first
DELETE FROM players;

-- Add real players
INSERT INTO players (jersey_num, full_name, position) VALUES
  (1,  'Real Goalie Name',   'G'),
  (10, 'Real Forward Name',  'F'),
  -- ... etc
```

---

## Running the App

```r
# In RStudio, open the VTHockey folder as a project, then:
shiny::runApp()

# Or from any R console:
shiny::runApp("path/to/VTHockey")
```

### Game day workflow

1. **Before the game**: Go to the Setup tab, create a new game entry (date + opponent)
2. **Each collector**: Open the app URL, go to Setup, enter their name, select the game, click "Start Collecting Data"
3. **During the game**: Use the appropriate tab to record events as they happen
4. **After the game**: Use the Summary tab to review all collected data

### Sharing the app (so others can access it on their phones)

**Option A — shinyapps.io** (recommended):
```r
# Install rsconnect, then:
rsconnect::deployApp("path/to/VTHockey")
```
This gives everyone a URL they can open on any device.

**Option B — local network**:
```r
# Run on your laptop, then share your laptop's IP address
shiny::runApp(host = "0.0.0.0", port = 3838)
# Others connect to http://YOUR_IP:3838
```

---

## App Structure

```
VTHockey/
├── global.R          # Loads packages, sources helpers, defines theme
├── ui.R              # All visual layout and tab definitions
├── server.R          # All reactive logic and Supabase submissions
├── R/
│   ├── supabase.R    # Functions that call the Supabase REST API
│   └── rink_plot.R   # Draws the hockey rink with ggplot2
├── www/
│   └── custom.css    # VT brand colors and mobile-friendly tweaks
├── setup/
│   └── schema.sql    # Run this once in Supabase to create tables
├── .env              # Your credentials (DO NOT share or commit)
├── .env.example      # Template showing what .env should look like
└── .gitignore        # Prevents .env from being committed to git
```

---

## Troubleshooting

**"Cannot reach Supabase" on the Setup tab**
- Check that your `.env` file exists and has the correct URL and key
- Make sure there are no extra spaces in the `.env` values
- Try the connection test: `source("R/supabase.R"); sb_check_connection()`

**Player dropdown is empty**
- Make sure you ran the full schema.sql including the sample INSERT statements
- Or add players manually via the Supabase Table Editor

**"Could not undo" error**
- The undo feature only works within 5 minutes of submission (by design)
- For older corrections, edit directly in the Supabase Table Editor

**Shot map clicks not registering**
- Make sure the click lands within the rink boundary (the light blue area)
- Clicks on the white border outside the rink are ignored

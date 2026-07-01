# ============================================================
# global.R  -  Runs once when the app starts
# ============================================================

# ---- Packages ----
library(shiny)
library(bslib)
library(shinyjs)
library(DT)
library(dplyr)
library(ggplot2)

# ---- Load Supabase API and rink drawing helpers ----
source("R/supabase.R")
source("R/rink_plot.R")
source("R/analytics.R")

# ---- Load .env file (your Supabase credentials) ----
# The .env file should look like:
#   SUPABASE_URL=https://yourproject.supabase.co
#   SUPABASE_ANON_KEY=your-anon-key-here
if (file.exists(".env")) {
  readRenviron(".env")
} else {
  warning(".env file not found. See README.md for setup instructions.")
}

# ---- VT Brand Colors ----
VT_MAROON <- "#861F41"
VT_ORANGE <- "#E5751F"

# ---- App-wide bslib theme ----
app_theme <- bs_theme(
  version   = 5,
  bg        = "#ffffff",
  fg        = "#1a1a1a",
  primary   = VT_MAROON,
  secondary = VT_ORANGE
)

# ---- Period choices (used in multiple tabs) ----
PERIOD_CHOICES <- c("1st Period" = "1", "2nd Period" = "2",
                    "3rd Period" = "3", "Overtime" = "OT",
                    "2OT" = "OT2", "3OT" = "OT3", "Shootout" = "SO")

# ---- Shot type choices ----
SHOT_TYPE_CHOICES <- c("Wrist shot"  = "wrist",
                       "Slap shot"   = "slap",
                       "Backhand"    = "backhand",
                       "Snap shot"   = "snap",
                       "Tip-in"      = "tip",
                       "Other"       = "other")

# ---- Notification helper: show green success toast ----
notify_ok <- function(msg) {
  showNotification(msg, type = "message", duration = 3)
}

# ---- Notification helper: show red error toast ----
notify_err <- function(msg) {
  showNotification(paste("Error:", msg), type = "error", duration = 6)
}

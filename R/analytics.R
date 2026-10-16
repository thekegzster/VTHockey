# ============================================================
# R/analytics.R
# xG model, zone classification, and advanced stat helpers
# ============================================================

# ---- Goal mouth location (VT attacks toward x = 200) ----
GOAL_X <- 189
GOAL_Y <- 42.5

# ============================================================
# PERIOD-BASED END SWAP
# VT attacks left in periods 1 & 3, right in period 2 — and
# alternates from there for any overtime/shootout periods.
# ============================================================

HOCKEY_PERIOD_ORDER <- c("1", "2", "3", "OT", "SO")

vt_attacks_right <- function(period) {
  if (is.null(period) || length(period) == 0) return(TRUE)
  period_chr <- as.character(period)
  idx <- match(period_chr, HOCKEY_PERIOD_ORDER)
  idx <- ifelse(is.na(idx), 2L, idx)
  idx %% 2 == 0
}

# Rotates (x, y) 180° when VT is NOT attacking right, bringing every
# shot into the canonical "VT attacks right" frame the xG/danger-zone
# model assumes (net at x=189).
to_canonical_xy <- function(x, y, period) {
  flip <- !vt_attacks_right(period)
  list(
    x = ifelse(flip, 200 - x, x),
    y = ifelse(flip, 85 - y, y)
  )
}

# Rotates (x, y) 180° for DISPLAY so personal shot maps (and any
# cross-period aggregate map) show shots as if VT always attacks left.
to_display_xy <- function(x, y, period) {
  flip <- vt_attacks_right(period)
  list(
    x = ifelse(flip, 200 - x, x),
    y = ifelse(flip, 85 - y, y)
  )
}

# ============================================================
# xG MODEL
# Logistic regression based on distance + angle + shot type.
# Coefficients are calibrated to produce ~9-10% average xG
# across all shot locations, consistent with NHL shooting %.
#
#  Distance effect: farther shots = lower xG
#  Angle effect:    sharper angle = lower xG
#  Shot type adj:   tip-ins & snap shots are more dangerous
# ============================================================

calc_xg <- function(x, y, shot_type = "wrist") {
  if (is.null(x) || is.null(y) || is.na(x) || is.na(y)) return(NA_real_)

  dist  <- sqrt((x - GOAL_X)^2 + (y - GOAL_Y)^2)
  angle <- abs(atan2(abs(y - GOAL_Y), abs(x - GOAL_X)) * 180 / pi)

  type_adj <- switch(as.character(shot_type),
    "tip"      =  0.30,
    "one-timer" = 0.30,
    "snap"     =  0.08,
    "wrist"    =  0.00,
    "slap"     = -0.10,
    "backhand" = -0.22,
    0.00
  )

  lp <- -1.0 - (0.04 * dist) - (0.02 * angle) + type_adj
  pmin(pmax(1 / (1 + exp(-lp)), 0), 1)
}

# Vectorised — adds an `xg` column to a shots data frame
add_xg <- function(df) {
  if (nrow(df) == 0) return(mutate(df, xg = numeric(0)))
  st_col     <- if ("shot_type" %in% names(df)) df$shot_type else rep("wrist", nrow(df))
  period_col <- if ("period" %in% names(df)) df$period else rep("1", nrow(df))
  canon      <- to_canonical_xy(df$x_coord, df$y_coord, period_col)
  df$xg <- mapply(calc_xg_empirical,
                  x         = canon$x,
                  y         = canon$y,
                  shot_type = st_col)
  df
}

# ============================================================
# EMPIRICAL xG MODEL (from real NHL play-by-play data, pooled
# across multiple seasons)
# Looks up a shot's expected-goal probability from a lookup
# table binned by distance-from-net, lateral position, and
# shot type. Falls back to the hand-tuned logistic model
# (calc_xg) for location/shot-type bins with too few real
# samples to trust, rather than a flat league-average number.
# ============================================================

# Resolve the lookup table's path relative to THIS file's own location,
# not the caller's working directory — so this works whether analytics.R
# is sourced from the Shiny app (root) or the Quarto report (Data_Analysis/).
.xg_lookup_path <- local({
  candidates <- c("R/xg_lookup_table.rds", "../R/xg_lookup_table.rds")
  found <- candidates[file.exists(candidates)]
  if (length(found) == 0) {
    stop("Could not locate xg_lookup_table.rds from working directory: ", getwd())
  }
  found[1]
})
XG_LOOKUP <- readRDS(.xg_lookup_path)

# Maps this app's shot_type strings to the numeric codes used when the
# lookup table was built. "other" has no clean NHL equivalent, so it
# falls back to "wrist" (the most common/neutral category).
.shot_type_to_code <- function(shot_type) {
  dplyr::case_when(
    shot_type == "wrist"    ~ 0,
    shot_type == "slap"     ~ 1,
    shot_type == "snap"     ~ 2,
    shot_type == "backhand" ~ 3,
    shot_type == "tip"      ~ 4,
    TRUE                    ~ 0   # "other" and anything unmapped -> wrist
  )
}

calc_xg_empirical <- function(x, y, shot_type = "wrist") {
  if (is.null(x) || is.null(y) || is.na(x) || is.na(y)) return(NA_real_)
  
  abs_x <- abs(x - 100)
  abs_y <- abs(y - 42.5)
  shot_code <- .shot_type_to_code(shot_type)
  
  x_group <- floor(abs_x / 10) * 100
  y_group <- floor(abs_y / 4) * 10
  
  match_row <- XG_LOOKUP[XG_LOOKUP$x_group == x_group &
                           XG_LOOKUP$y_group == y_group &
                           XG_LOOKUP$shot_group == shot_code, ]
  
  if (nrow(match_row) == 1 && match_row$n >= 15) {
    match_row$goal_percent[1]
  } else {
    calc_xg(x, y, shot_type)   # fall back to hand-tuned model for thin bins
  }
}

# ============================================================
# DANGER ZONE CLASSIFICATION
# high   : within 25 ft, angle ≤ 45° (the slot)
# medium : within 40 ft, or high-angle close shots
# low    : everything else
# ============================================================

classify_danger <- function(x, y) {
  dist  <- sqrt((x - GOAL_X)^2 + (y - GOAL_Y)^2)
  angle <- abs(atan2(abs(y - GOAL_Y), abs(x - GOAL_X)) * 180 / pi)
  in_crease_box <- (
    x >= (GOAL_X - 8) & x <= GOAL_X &
      y >= (GOAL_Y - 10) & y <= (GOAL_Y + 10)
  )
  inside_dots <- abs(y - GOAL_Y) <= 26
  in_front_of_goal <- x <= GOAL_X
  dplyr::case_when(
    !in_front_of_goal ~ "Low Danger",
    in_crease_box ~ "High Danger",
    dist <= 25 & angle <= 45 ~ "High Danger",
    dist <= 40 & inside_dots ~ "Medium Danger",
    TRUE                     ~ "Low Danger"
  )
}

# --- Builds map of danger zone areas ---
build_danger_zones <- function(res = 2) {
  grid <- expand.grid(
    x = seq(0, 200, by = res),
    y = seq(0, 85, by = res)
  )
  
  grid$zone <- mapply(classify_danger, grid$x, grid$y)
  grid
}

# ============================================================
# PLUS / MINUS  &  xG PLUS / MINUS
# Joins on_ice records to shots to compute for-vs-against stats.
#
# +/-    : +1 when a VT goal is scored while on ice,
#          -1 when an opponent goal is scored while on ice.
# xG+/-  : sum of xG for VT shots while on ice
#          minus sum of xG for opponent shots while on ice.
#          Opponent shot xG is calculated by mirroring x-coords
#          (200 - x) so the same model applies to their net (x≈11).
# ============================================================

compute_plus_minus <- function(on_ice, shots) {
  empty <- data.frame(player_id     = integer(0),
                      plus_minus    = integer(0),
                      xg_plus_minus = numeric(0))

  if (is.null(on_ice)  || !is.data.frame(on_ice)  || nrow(on_ice)  == 0) return(empty)
  if (is.null(shots)   || !is.data.frame(shots)   || nrow(shots)   == 0) return(empty)
  if (!"shot_id"  %in% names(on_ice)) return(empty)
  if (!"id"       %in% names(shots))  return(empty)
  if (!"team"     %in% names(shots))  return(empty)
  if (!"result"   %in% names(shots))  return(empty)

  # Calculate xG for every shot; mirror x for opponent shots so the
  # model targets their net (≈ x = 11) instead of VT's net (x = 189)
  st_col     <- if ("shot_type" %in% names(shots)) shots$shot_type else rep("wrist", nrow(shots))
  tm_col     <- shots$team
  period_col <- if ("period" %in% names(shots)) shots$period else rep("1", nrow(shots))
  canon      <- to_canonical_xy(shots$x_coord, shots$y_coord, period_col)
  
  shots$xg_pm <- mapply(function(x, y, st, tm) {
    if (is.na(x) || is.na(y)) return(0)
    if (tm == "VT") calc_xg_empirical(x,       y, st)
    else            calc_xg_empirical(200 - x, y, st)
  }, canon$x, canon$y, st_col, tm_col)

  # Join on_ice → shots on shot_id
  oi <- on_ice[!is.na(on_ice$shot_id), c("player_id", "shot_id")]
  s  <- shots[, c("id", "team", "result", "xg_pm")]
  merged <- merge(oi, s, by.x = "shot_id", by.y = "id", all.x = FALSE)
  if (nrow(merged) == 0) return(empty)

  merged$pm_val <- dplyr::case_when(
    merged$result == "goal" & merged$team == "VT"  ~  1L,
    merged$result == "goal" & merged$team != "VT"  ~ -1L,
    TRUE                                           ~  0L
  )
  merged$xg_val <- ifelse(merged$team == "VT",
                           tidyr::replace_na(merged$xg_pm, 0),
                          -tidyr::replace_na(merged$xg_pm, 0))

  merged |>
    dplyr::group_by(player_id) |>
    dplyr::summarise(
      plus_minus    = sum(pm_val,  na.rm = TRUE),
      xg_plus_minus = round(sum(xg_val, na.rm = TRUE), 2),
      .groups = "drop"
    )
}

# ============================================================
# PLUS / MINUS BY GAME
# Same logic as compute_plus_minus(), but keeps game_id so you
# can chart a player's +/- and xG+/- trend across the season.
# ============================================================

compute_plus_minus_by_game <- function(on_ice, shots) {
  empty <- data.frame(player_id     = integer(0),
                      game_id       = integer(0),
                      plus_minus    = integer(0),
                      xg_plus_minus = numeric(0))
  
  if (is.null(on_ice)  || !is.data.frame(on_ice)  || nrow(on_ice)  == 0) return(empty)
  if (is.null(shots)   || !is.data.frame(shots)   || nrow(shots)   == 0) return(empty)
  if (!"shot_id"  %in% names(on_ice)) return(empty)
  if (!all(c("id", "game_id", "team", "result") %in% names(shots))) return(empty)
  
  st_col     <- if ("shot_type" %in% names(shots)) shots$shot_type else rep("wrist", nrow(shots))
  period_col <- if ("period" %in% names(shots)) shots$period else rep("1", nrow(shots))
  canon      <- to_canonical_xy(shots$x_coord, shots$y_coord, period_col)
  
  shots$xg_pm <- mapply(function(x, y, st, tm) {
    if (is.na(x) || is.na(y)) return(0)
    if (tm == "VT") calc_xg_empirical(x, y, st) else calc_xg_empirical(200 - x, y, st)
  }, canon$x, canon$y, st_col, shots$team)
  
  oi <- on_ice[!is.na(on_ice$shot_id), c("player_id", "shot_id")]
  s  <- shots[, c("id", "game_id", "team", "result", "xg_pm")]
  merged <- merge(oi, s, by.x = "shot_id", by.y = "id", all.x = FALSE)
  if (nrow(merged) == 0) return(empty)
  
  merged$pm_val <- dplyr::case_when(
    merged$result == "goal" & merged$team == "VT"  ~  1L,
    merged$result == "goal" & merged$team != "VT"  ~ -1L,
    TRUE                                           ~  0L
  )
  merged$xg_val <- ifelse(merged$team == "VT",
                          tidyr::replace_na(merged$xg_pm, 0),
                          -tidyr::replace_na(merged$xg_pm, 0))
  
  merged |>
    dplyr::group_by(player_id, game_id) |>
    dplyr::summarise(
      plus_minus    = sum(pm_val,  na.rm = TRUE),
      xg_plus_minus = round(sum(xg_val, na.rm = TRUE), 2),
      .groups = "drop"
    )
}

# ============================================================
# ON-ICE GOALS FOR
# Counts how many VT goals each skater was on the ice for.
# Goalies are excluded. Optionally scoped to a single game_id.
# ============================================================

compute_on_ice_goals_for <- function(on_ice, shots, players, game_id = NULL) {
  empty <- data.frame(player_id = integer(0), on_ice_goals_for = integer(0))
  
  if (is.null(on_ice) || !is.data.frame(on_ice) || nrow(on_ice) == 0) return(empty)
  if (is.null(shots)  || !is.data.frame(shots)  || nrow(shots)  == 0) return(empty)
  if (is.null(players) || !is.data.frame(players) || nrow(players) == 0) return(empty)
  if (!"shot_id" %in% names(on_ice)) return(empty)
  if (!all(c("id", "team", "result") %in% names(shots))) return(empty)
  if (!all(c("id", "position") %in% names(players))) return(empty)
  
  vt_goals <- shots[shots$result == "goal" & shots$team == "VT" &
                      !is.na(shots$result) & !is.na(shots$team), ]
  if (!is.null(game_id) && "game_id" %in% names(shots)) {
    vt_goals <- vt_goals[vt_goals$game_id == game_id, ]
  }
  if (nrow(vt_goals) == 0) return(empty)
  
  skater_ids <- players$id[players$position != "G" & !is.na(players$position)]
  
  oi <- on_ice[on_ice$team == "VT" & !is.na(on_ice$shot_id) &
                 on_ice$player_id %in% skater_ids, c("player_id", "shot_id")]
  merged <- merge(oi, vt_goals[, "id", drop = FALSE], by.x = "shot_id", by.y = "id", all.x = FALSE)
  if (nrow(merged) == 0) return(empty)
  
  merged |>
    dplyr::group_by(player_id) |>
    dplyr::summarise(on_ice_goals_for = dplyr::n(), .groups = "drop")
}

# ============================================================
# COMPUTE FULL PLAYER STATS TABLE
# Returns one row per skater with all basic + advanced stats
# ============================================================

## Computing Goalie Stats
compute_goalie_stats <- function(shots, on_ice = data.frame()) {
  
  empty_result <- data.frame(
    goalie_id = integer(0), shots_against = integer(0), saves = integer(0),
    goals_against = integer(0), xg_against = numeric(0),
    high_danger_saves = integer(0), medium_danger_saves = integer(0), low_danger_saves = integer(0),
    high_danger_shots = integer(0), medium_danger_shots = integer(0), low_danger_shots = integer(0),
    save_pct = numeric(0), hd_save_pct = numeric(0), md_save_pct = numeric(0), ld_save_pct = numeric(0),
    gsax = numeric(0), games_played = integer(0), gaa = numeric(0), xgaa = numeric(0)
  )
  
  if (nrow(shots) == 0 || !"goalie_id" %in% names(shots)) {
    return(empty_result)
  }
  
  has_shot_type <- "shot_type" %in% names(shots)
  
  goalie_shots <- shots |>
    dplyr::filter(team != "VT") |>
    dplyr::filter(!is.na(goalie_id)) |>
    dplyr::mutate(
      shot_type_safe = if (has_shot_type) shot_type else "wrist",
      x_canon = to_canonical_xy(x_coord, y_coord, period)$x,
      y_canon = to_canonical_xy(x_coord, y_coord, period)$y,
      danger = mapply(function(x, y) classify_danger(200 - x, y), x_canon, y_canon),
      xga = mapply(function(x, y, st) {
        if (is.na(x) || is.na(y)) return(0)
        calc_xg_empirical(200 - x, y, st)
      }, x_canon, y_canon, shot_type_safe),
      saved = result == "save",
      goal_allowed = result == "goal"
    )
  
  stats <- goalie_shots |>
    dplyr::group_by(goalie_id) |>
    dplyr::summarise(
      shots_against = n(),
      saves = sum(saved),
      goals_against = sum(goal_allowed),
      xg_against = round(sum(xga, na.rm = TRUE), 2),
      high_danger_saves = sum(saved & danger == "High Danger"),
      medium_danger_saves = sum(saved & danger == "Medium Danger"),
      low_danger_saves = sum(saved & danger == "Low Danger"),
      high_danger_shots = sum(danger == "High Danger"),
      medium_danger_shots = sum(danger == "Medium Danger"),
      low_danger_shots = sum(danger == "Low Danger"),
      save_pct = ifelse(shots_against > 0, round(saves / shots_against * 100, 1), NA_real_),
      hd_save_pct = ifelse(high_danger_shots > 0, round(high_danger_saves / high_danger_shots * 100, 1), NA_real_),
      md_save_pct = ifelse(medium_danger_shots > 0, round(medium_danger_saves / medium_danger_shots * 100, 1), NA_real_),
      ld_save_pct = ifelse(low_danger_shots > 0, round(low_danger_saves / low_danger_shots * 100, 1), NA_real_),
      gsax = round(xg_against - goals_against, 2),
      .groups = "drop"
    )
  
  games_played_df <- if (nrow(on_ice) > 0 && all(c("player_id", "game_id") %in% names(on_ice))) {
    on_ice |>
      dplyr::filter(!is.na(player_id)) |>
      dplyr::distinct(player_id, game_id) |>
      dplyr::count(player_id, name = "games_played") |>
      dplyr::rename(goalie_id = player_id)
  } else {
    data.frame(goalie_id = integer(0), games_played = integer(0))
  }
  
  stats |>
    dplyr::left_join(games_played_df, by = "goalie_id") |>
    dplyr::mutate(
      games_played = tidyr::replace_na(games_played, 0L),
      gaa  = ifelse(games_played > 0, round(goals_against / games_played, 2), NA_real_),
      xgaa = ifelse(games_played > 0, round(xg_against / games_played, 2), NA_real_)
    )
}


compute_player_stats <- function(players, shots, goals, faceoffs, turnovers,
                                 on_ice = data.frame(), n_games = 1,
                                 penalties = data.frame()) {

  # guard: ensure required columns exist even on empty frames
  safe_nrow <- function(df) if (is.null(df) || nrow(df) == 0) 0L else nrow(df)

  # --- shots-based stats ---
  shots_vt <- if (nrow(shots) > 0 && "team" %in% names(shots))
    shots[shots$team == "VT" & !is.na(shots$team), ] else shots[0, ]

  if (nrow(shots_vt) > 0 && "x_coord" %in% names(shots_vt))
    shots_vt <- add_xg(shots_vt)

  shot_stats <- if (nrow(shots_vt) > 0) {
    shots_vt |>
      dplyr::mutate(
        x_canon = to_canonical_xy(x_coord, y_coord, period)$x,
        y_canon = to_canonical_xy(x_coord, y_coord, period)$y,
        danger  = ifelse(!is.na(x_canon), classify_danger(x_canon, y_canon), NA_character_)
      ) |>
      dplyr::group_by(player_id) |>
      dplyr::summarise(
        shots      = dplyr::n(),
        goals_shot = sum(result == "goal", na.rm = TRUE),
        on_goal    = sum(result %in% c("goal", "save"), na.rm = TRUE),
        xg_total   = round(sum(xg, na.rm = TRUE), 2),
        xg_per_shot = round(mean(xg, na.rm = TRUE), 3),
        high_danger    = sum(danger == "High Danger",   na.rm = TRUE),
        medium_danger  = sum(danger == "Medium Danger", na.rm = TRUE),
        low_danger     = sum(danger == "Low Danger",    na.rm = TRUE),
        hd_goals       = sum(danger == "High Danger"   & result == "goal", na.rm = TRUE),
        md_goals       = sum(danger == "Medium Danger" & result == "goal", na.rm = TRUE),
        ld_goals       = sum(danger == "Low Danger"    & result == "goal", na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    data.frame(player_id = integer(0), shots = integer(0),
               goals_shot = integer(0), on_goal = integer(0),
               xg_total = numeric(0), xg_per_shot = numeric(0),
               high_danger = integer(0), medium_danger = integer(0), low_danger = integer(0),
               hd_goals = integer(0), md_goals = integer(0), ld_goals = integer(0))
  }

  # --- scoring stats ---
  scorer_g <- if (nrow(goals) > 0 && "scorer_id" %in% names(goals)) {
    goals[goals$team == "VT" & !is.na(goals$team), ] |>
      dplyr::filter(!is.na(scorer_id)) |>
      dplyr::count(scorer_id, name = "goals") |>
      dplyr::rename(player_id = scorer_id)
  } else data.frame(player_id = integer(0), goals = integer(0))

  a1 <- if (nrow(goals) > 0 && "assist1_id" %in% names(goals)) {
    goals[goals$team == "VT" & !is.na(goals$team), ] |>
      dplyr::filter(!is.na(assist1_id)) |>
      dplyr::count(assist1_id, name = "a1") |>
      dplyr::rename(player_id = assist1_id)
  } else data.frame(player_id = integer(0), a1 = integer(0))

  a2 <- if (nrow(goals) > 0 && "assist2_id" %in% names(goals)) {
    goals[goals$team == "VT" & !is.na(goals$team), ] |>
      dplyr::filter(!is.na(assist2_id)) |>
      dplyr::count(assist2_id, name = "a2") |>
      dplyr::rename(player_id = assist2_id)
  } else data.frame(player_id = integer(0), a2 = integer(0))

  # --- situational scoring (power play / short handed) ---
  # A "point" on a PP/SH goal credits the scorer AND both assists, same
  # convention as regular points (goals + a1 + a2).
  vt_goals <- if (nrow(goals) > 0 && "team" %in% names(goals))
    goals[goals$team == "VT" & !is.na(goals$team), ] else goals[0, ]
  
  situational_stats <- if (nrow(vt_goals) > 0 && "situation" %in% names(vt_goals)) {
    pp_goals <- vt_goals[!is.na(vt_goals$situation) & vt_goals$situation == "power_play", ]
    sh_goals <- vt_goals[!is.na(vt_goals$situation) & vt_goals$situation == "short_handed", ]
    
    situational_ids <- function(df, id_col) {
      if (nrow(df) == 0 || !id_col %in% names(df)) return(integer(0))
      df[[id_col]][!is.na(df[[id_col]])]
    }
    
    pp_involved <- c(situational_ids(pp_goals, "scorer_id"),
                     situational_ids(pp_goals, "assist1_id"),
                     situational_ids(pp_goals, "assist2_id"))
    sh_involved <- c(situational_ids(sh_goals, "scorer_id"),
                     situational_ids(sh_goals, "assist1_id"),
                     situational_ids(sh_goals, "assist2_id"))
    
    pp_goal_ids <- situational_ids(pp_goals, "scorer_id")
    sh_goal_ids <- situational_ids(sh_goals, "scorer_id")
    
    all_player_ids <- unique(c(pp_involved, sh_involved))
    if (length(all_player_ids) == 0) {
      data.frame(player_id = integer(0), pp_goals = integer(0), pp_points = integer(0),
                 sh_goals = integer(0), sh_points = integer(0))
    } else {
      data.frame(
        player_id = all_player_ids,
        pp_goals  = sapply(all_player_ids, function(pid) sum(pp_goal_ids == pid)),
        pp_points = sapply(all_player_ids, function(pid) sum(pp_involved == pid)),
        sh_goals  = sapply(all_player_ids, function(pid) sum(sh_goal_ids == pid)),
        sh_points = sapply(all_player_ids, function(pid) sum(sh_involved == pid))
      )
    }
  } else {
    data.frame(player_id = integer(0), pp_goals = integer(0), pp_points = integer(0),
               sh_goals = integer(0), sh_points = integer(0))
  }
  
  # --- turnover stats ---
  turnover_stats <- if (nrow(turnovers) > 0 && "type" %in% names(turnovers)) {
    turnovers |>
      dplyr::group_by(player_id) |>
      dplyr::summarise(
        takeaways = sum(type == "takeaway", na.rm = TRUE),
        giveaways = sum(type == "giveaway", na.rm = TRUE),
        .groups   = "drop"
      )
  } else data.frame(player_id = integer(0), takeaways = integer(0),
                    giveaways = integer(0))
  
  # --- blocked shot stats ---
  # Counts shots opponents took that this VT player blocked, from
  # shots.blocked_by_player_id — captured on the Shots form when
  # team == opponent and result == blocked.
  block_stats <- if (nrow(shots) > 0 && "blocked_by_player_id" %in% names(shots)) {
    shots |>
      dplyr::filter(!is.na(blocked_by_player_id)) |>
      dplyr::count(blocked_by_player_id, name = "blocks") |>
      dplyr::rename(player_id = blocked_by_player_id)
  } else data.frame(player_id = integer(0), blocks = integer(0))

  
  # --- penalty minutes (individual) ---
  # Only penalties charged to a specific VT player. Bench minors have no
  # player_id, so they appear in team PIM but in nobody's individual total.
  pim_stats <- if (!is.null(penalties) && nrow(penalties) > 0 &&
                   all(c("team", "player_id", "minutes") %in% names(penalties))) {
    penalties |>
      dplyr::filter(!is.na(team), team == "VT", !is.na(player_id)) |>
      dplyr::group_by(player_id) |>
      dplyr::summarise(
        pim             = sum(minutes, na.rm = TRUE),
        penalties_taken = dplyr::n(),
        .groups = "drop"
      )
  } else data.frame(player_id = integer(0), pim = integer(0),
                    penalties_taken = integer(0))
  
  # --- plus/minus stats ---
  pm_stats <- compute_plus_minus(on_ice, shots)

  # --- faceoff stats ---
  fo_stats <- if (nrow(faceoffs) > 0) {
    faceoffs |>
      dplyr::group_by(player_id) |>
      dplyr::summarise(
        fo_total = dplyr::n(),
        fo_wins  = sum(result == "win", na.rm = TRUE),
        .groups  = "drop"
      )
  } else data.frame(player_id = integer(0), fo_total = integer(0),
                    fo_wins = integer(0))

  # --- games played per skater ---
  # Real per-player appearance count, from distinct game_id per player_id
  # in on_ice — same approach compute_goalie_stats() already uses. This
  # replaces the flat n_games fallback with each skater's actual games.
  games_played_df <- if (nrow(on_ice) > 0 && all(c("player_id", "game_id") %in% names(on_ice))) {
    on_ice |>
      dplyr::filter(!is.na(player_id)) |>
      dplyr::distinct(player_id, game_id) |>
      dplyr::count(player_id, name = "games_played")
  } else {
    data.frame(player_id = integer(0), games_played = integer(0))
  }
  
  # --- combine ---
  # Filter to skaters — guard against missing `active` column
  active_flag <- if ("active" %in% names(players)) players$active
  else rep(TRUE, nrow(players))
  skaters <- players[players$position != "G" & !is.na(players$position) &
                       active_flag, ]
  
  out <- skaters |>
    dplyr::select(player_id = id, jersey_num, full_name, position) |>
    dplyr::left_join(scorer_g,       by = "player_id") |>
    dplyr::left_join(a1,             by = "player_id") |>
    dplyr::left_join(a2,             by = "player_id") |>
    dplyr::left_join(shot_stats,     by = "player_id") |>
    dplyr::left_join(turnover_stats, by = "player_id") |>
    dplyr::left_join(block_stats,    by = "player_id") |>
    dplyr::left_join(fo_stats,       by = "player_id") |>
    dplyr::left_join(pm_stats,       by = "player_id") |>
    dplyr::left_join(games_played_df, by = "player_id") |>
    dplyr::left_join(situational_stats, by = "player_id") |>
    dplyr::left_join(pim_stats,         by = "player_id") |>
    dplyr::mutate(
      dplyr::across(c(goals, a1, a2, shots, goals_shot, on_goal,
                      high_danger, medium_danger, low_danger,
                      hd_goals, md_goals, ld_goals,
                      takeaways, giveaways, blocks, fo_total, 
                      fo_wins, pp_goals, pp_points, sh_goals, sh_points,
                      pim, penalties_taken),
                    ~ tidyr::replace_na(.x, 0L)),
      dplyr::across(c(xg_total, xg_per_shot),
                    ~ tidyr::replace_na(.x, 0.0)),
      plus_minus    = tidyr::replace_na(plus_minus,    0L),
      xg_plus_minus = tidyr::replace_na(xg_plus_minus, 0.0),
      games_played = if (n_games == 1) 1L else tidyr::replace_na(games_played, 0L),
      points      = goals + a1 + a2,
      shoot_pct   = ifelse(shots > 0,  round(goals / shots * 100, 1),  0.0),
      on_goal_pct = ifelse(shots > 0,  round(on_goal / shots * 100, 1), 0.0),
      hd_pct      = ifelse(shots > 0,  round(high_danger / shots * 100, 1), 0.0),
      hd_shoot_pct = ifelse(high_danger > 0, round(hd_goals / high_danger * 100, 1), NA_real_),
      md_shoot_pct = ifelse(medium_danger > 0, round(md_goals / medium_danger * 100, 1), NA_real_),
      ld_shoot_pct = ifelse(low_danger > 0, round(ld_goals / low_danger * 100, 1), NA_real_),
      fo_pct      = ifelse(fo_total > 0, round(fo_wins / fo_total * 100, 1), NA_real_),
      g_minus_xg  = round(goals - xg_total, 2),   # goals above expected
      pts_per_game = round(points / pmax(games_played, 1), 2),
      xg_per_game  = round(xg_total / pmax(games_played, 1), 2),
      shots_per_game     = round(shots     / pmax(games_played, 1), 2),
      takeaways_per_game = round(takeaways / pmax(games_played, 1), 2),
      giveaways_per_game = round(giveaways / pmax(games_played, 1), 2),
      ta_ga_ratio        = ifelse(giveaways > 0,
                                  round(takeaways / giveaways, 2),
                                  NA_real_),
      net_turnovers      = takeaways - giveaways,
      pim_per_game       = round(pim / pmax(games_played, 1), 2),
    ) |>
    dplyr::arrange(dplyr::desc(points), dplyr::desc(goals))
  
  out
}

# ============================================================
# PERCENTILE HELPERS
# ============================================================

# Returns 0-100 percentile of `val` within vector `vec`
# Higher is always better (caller should negate if lower = better)
calc_percentile <- function(val, vec) {
  vec <- vec[!is.na(vec)]
  if (length(vec) == 0 || is.na(val)) return(50L)
  as.integer(round(sum(vec <= val) / length(vec) * 100))
}

# Colour for a percentile: red -> orange -> light blue -> blue
pct_color <- function(pct) {
  if      (pct >= 85) "#1655a2"        # deep blue
  else if (pct >= 60) "#4a9fd4"        # light blue
  else if (pct >= 40) "#f5a623"        # orange
  else                "#d62020"        # red
}

# HTML for a single Baseball-Savant-style percentile bar row
pct_bar <- function(label, value_label, pct) {
  col <- pct_color(pct)
  div(
    class = "d-flex align-items-center mb-2",
    style = "font-size: 0.9rem;",
    # Percentile number
    div(style = paste0("width:36px; text-align:center; font-weight:700;",
                       " color:", col, "; font-size:1rem;"),
        pct),
    # Bar track
    div(class = "mx-2", style = "flex:1; background:#e9ecef; border-radius:4px; height:14px;",
        div(style = paste0("width:", pct, "%; background:", col,
                           "; border-radius:4px; height:14px; transition:width 0.4s;"))
    ),
    # Stat label + value
    div(style = "min-width:220px;",
        tags$b(label),
        tags$span(style = "color:#555; margin-left:6px; font-style:italic;",
                  value_label))
  )
}


# ============================================================
# PENALTIES (PIM)
# Times follow NCAA/ACHA Rules 16-23. Dropdown groupings are for
# convenience only.
# ============================================================

PENALTY_INFRACTIONS <- list(
  "Physical Fouls" = c(
    "Boarding"            = "boarding",
    "Charging"            = "charging",
    "Clipping"            = "clipping",
    "Contact to the Head" = "contact_head",
    "Elbowing"            = "elbowing",
    "Fighting"            = "fighting",
    "Hitting from Behind" = "hitting_behind",
    "Kneeing"             = "kneeing",
    "Roughing"            = "roughing"
  ),
  "Restraining Fouls" = c(
    "Holding"      = "holding",
    "Hooking"      = "hooking",
    "Interference" = "interference",
    "Obstruction"  = "obstruction",
    "Tripping"     = "tripping"
  ),
  "Stick Fouls" = c(
    "Butt-Ending"            = "butt_ending",
    "Cross-Checking"         = "cross_checking",
    "Grasping the Face Mask" = "face_mask",
    "High-Sticking"          = "high_sticking",
    "Holding the Stick"      = "holding_stick",
    "Slashing"               = "slashing",
    "Spearing"               = "spearing"
  ),
  "Other Fouls" = c(
    "Abuse of Officials"      = "abuse_officials",
    "Delay of Game"           = "delay_of_game",
    "Goalkeeper Infraction"   = "goalkeeper_infraction",
    "Illegal Equipment"       = "illegal_equipment",
    "Throwing Object on Ice"  = "throwing_object",
    "Too Many Players on Ice" = "too_many_players",
    "Unsportsmanlike Conduct" = "unsportsmanlike",
    "Other"                   = "other"
  )
)

PENALTY_CLASS_CHOICES <- c(
  "Minor (2 min)"                       = "minor",
  "Bench minor (2 min, charged to team)" = "bench_minor",
  "Double minor (4 min)"                = "double_minor",
  "Major (5 min)"                       = "major",
  "Minor + game misconduct (2 min)"     = "minor_gm",
  "Major + game misconduct (5 min)"     = "major_gm",
  "Misconduct (10 min)"                 = "misconduct",
  "Game misconduct (ejection only)"     = "game_misconduct",
  "Disqualification (5 min + ejection)" = "disqualification"
)

# Default minutes per type. The Penalties tab box stays editable.
PENALTY_CLASS_MINUTES <- c(
  minor = 2, bench_minor = 2, double_minor = 4, major = 5,
  minor_gm = 2, major_gm = 5, misconduct = 10,
  game_misconduct = 0, disqualification = 5
)

PENALTY_CLASS_SHORT <- c(
  minor = "Minor", bench_minor = "Bench minor", double_minor = "Double minor",
  major = "Major", minor_gm = "Minor + GM", major_gm = "Major + GM",
  misconduct = "Misconduct", game_misconduct = "Game misconduct",
  disqualification = "DQ"
)

penalty_infraction_label <- function(code) {
  lookup <- unlist(unname(PENALTY_INFRACTIONS))
  out <- names(lookup)[match(code, lookup)]
  ifelse(is.na(out), as.character(code), out)
}

penalty_class_short <- function(code) {
  out <- unname(PENALTY_CLASS_SHORT[as.character(code)])
  ifelse(is.na(out), as.character(code), out)
}

# Team-level PIM and penalty counts (includes bench minors)
sum_pim_by_team <- function(penalties) {
  zero <- list(vt = 0, opp = 0, vt_n = 0L, opp_n = 0L)
  if (is.null(penalties) || !is.data.frame(penalties) || nrow(penalties) == 0 ||
      !all(c("team", "minutes") %in% names(penalties))) return(zero)
  is_vt  <- !is.na(penalties$team) & penalties$team == "VT"
  is_opp <- !is.na(penalties$team) & penalties$team != "VT"
  list(
    vt    = sum(penalties$minutes[is_vt],  na.rm = TRUE),
    opp   = sum(penalties$minutes[is_opp], na.rm = TRUE),
    vt_n  = sum(is_vt),
    opp_n = sum(is_opp)
  )
}

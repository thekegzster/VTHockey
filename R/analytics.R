# ============================================================
# R/analytics.R
# xG model, zone classification, and advanced stat helpers
# ============================================================

# ---- Goal mouth location (VT attacks toward x = 200) ----
GOAL_X <- 189
GOAL_Y <- 42.5

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
  st_col <- if ("shot_type" %in% names(df)) df$shot_type else rep("wrist", nrow(df))
  df$xg <- mapply(calc_xg,
                  x         = df$x_coord,
                  y         = df$y_coord,
                  shot_type = st_col)
  df
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
  st_col <- if ("shot_type" %in% names(shots)) shots$shot_type else rep("wrist", nrow(shots))
  tm_col <- shots$team

  shots$xg_pm <- mapply(function(x, y, st, tm) {
    if (is.na(x) || is.na(y)) return(0)
    if (tm == "VT") calc_xg(x,       y, st)
    else            calc_xg(200 - x, y, st)   # mirror to opponent net
  }, shots$x_coord, shots$y_coord, st_col, tm_col)

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
# COMPUTE FULL PLAYER STATS TABLE
# Returns one row per skater with all basic + advanced stats
# ============================================================

## Computing Goalie Stats
compute_goalie_stats <- function(shots) {
  
  if (nrow(shots) == 0 || !"goalie_id" %in% names(shots)) {
    return(data.frame())
  }
  
  goalie_shots <- shots |>
    dplyr::filter(team != "VT") |>
    dplyr::filter(!is.na(goalie_id)) |>
    dplyr::mutate(
      danger = mapply(classify_danger, x_coord, y_coord),
      saved = result == "save",
      goal_allowed = result == "goal"
    )
  
  goalie_shots |>
    dplyr::group_by(goalie_id) |>
    dplyr::summarise(
      
      shots_against = n(),
      saves = sum(saved),
      goals_against = sum(goal_allowed),
      
      high_danger_saves = sum(saved & danger == "High Danger"),
      medium_danger_saves = sum(saved & danger == "Medium Danger"),
      low_danger_saves = sum(saved & danger == "Low Danger"),
      
      high_danger_shots = sum(danger == "High Danger"),
      medium_danger_shots = sum(danger == "Medium Danger"),
      low_danger_shots = sum(danger == "Low Danger"),
      
      save_pct = ifelse(shots_against > 0, 
                        round(saves / shots_against * 100, 1), NA_real_),
      
      hd_save_pct = ifelse(high_danger_shots > 0,
                           round(high_danger_saves / high_danger_shots * 100, 1),
                           NA_real_),
      
      md_save_pct = ifelse(medium_danger_shots > 0,
                           round(medium_danger_saves / medium_danger_shots * 100, 1),
                           NA_real_),
      
      ld_save_pct = ifelse(low_danger_shots > 0,
                           round(low_danger_saves / low_danger_shots * 100, 1),
                           NA_real_),
      
      .groups="drop"
    )
}


compute_player_stats <- function(players, shots, goals, faceoffs, turnovers,
                                 on_ice = data.frame(), n_games = 1) {

  # guard: ensure required columns exist even on empty frames
  safe_nrow <- function(df) if (is.null(df) || nrow(df) == 0) 0L else nrow(df)

  # --- shots-based stats ---
  shots_vt <- if (nrow(shots) > 0 && "team" %in% names(shots))
    shots[shots$team == "VT" & !is.na(shots$team), ] else shots[0, ]

  if (nrow(shots_vt) > 0 && "x_coord" %in% names(shots_vt))
    shots_vt <- add_xg(shots_vt)

  shot_stats <- if (nrow(shots_vt) > 0) {
    shots_vt |>
      dplyr::group_by(player_id) |>
      dplyr::summarise(
        shots      = dplyr::n(),
        goals_shot = sum(result == "goal", na.rm = TRUE),
        on_goal    = sum(result %in% c("goal", "save"), na.rm = TRUE),
        xg_total   = round(sum(xg, na.rm = TRUE), 2),
        xg_per_shot = round(mean(xg, na.rm = TRUE), 3),
        high_danger = sum(
          !is.na(x_coord) &
          classify_danger(x_coord, y_coord) == "High Danger",
          na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    data.frame(player_id = integer(0), shots = integer(0),
               goals_shot = integer(0), on_goal = integer(0),
               xg_total = numeric(0), xg_per_shot = numeric(0),
               high_danger = integer(0))
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
    dplyr::left_join(fo_stats,       by = "player_id") |>
    dplyr::left_join(pm_stats,       by = "player_id") |>
    dplyr::mutate(
      dplyr::across(c(goals, a1, a2, shots, goals_shot, on_goal,
                      high_danger, takeaways, giveaways, fo_total, fo_wins),
                    ~ tidyr::replace_na(.x, 0L)),
      dplyr::across(c(xg_total, xg_per_shot),
                    ~ tidyr::replace_na(.x, 0.0)),
      plus_minus    = tidyr::replace_na(plus_minus,    0L),
      xg_plus_minus = tidyr::replace_na(xg_plus_minus, 0.0),
      points      = goals + a1 + a2,
      shoot_pct   = ifelse(shots > 0,  round(goals / shots * 100, 1),  0.0),
      on_goal_pct = ifelse(shots > 0,  round(on_goal / shots * 100, 1), 0.0),
      hd_pct      = ifelse(shots > 0,  round(high_danger / shots * 100, 1), 0.0),
      fo_pct      = ifelse(fo_total > 0, round(fo_wins / fo_total * 100, 1), NA_real_),
      g_minus_xg  = round(goals - xg_total, 2),   # goals above expected
      pts_per_game = round(points / max(n_games, 1), 2),
      xg_per_game  = round(xg_total / max(n_games, 1), 2),
      shots_per_game     = round(shots     / max(n_games, 1), 2),
      takeaways_per_game = round(takeaways / max(n_games, 1), 2),
      giveaways_per_game = round(giveaways / max(n_games, 1), 2),
      ta_ga_ratio        = ifelse(giveaways > 0,
                                  round(takeaways / giveaways, 2),
                                  NA_real_),
      net_turnovers      = takeaways - giveaways
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

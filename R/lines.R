# ============================================================
# R/lines.R
# Line optimization: best forward lines / defense pairs, and
# each skater's best linemates. Ported from vt_to_nhl_pbp.R and
# vt_line_optimization.R so it runs on the app's in-memory data.
#
# Method (same as the original scripts):
#  - Every shot attempt (both teams) is scored with the xG model.
#  - Only shots with exactly 5 VT skaters on the ice count, so
#    pulled-goalie and short-handed shots are excluded.
#  - A unit is a forward trio (3F) or a defense pair (2D) that was
#    on the ice together. Rates are per 100 shot events (for +
#    against) because the app does not track time on ice.
# ============================================================

LINE_MIN_EVENTS <- 2L   # fewest shot events a unit needs to be considered
LINE_MIN_GAMES  <- 1L   # fewest games a unit needs to be considered

LINE_FORWARD_WEIGHTS <- c(0.35, 0.30, 0.20, 0.15)
LINE_DEFENSE_WEIGHTS <- c(0.45, 0.35, 0.20)
LINE_FORWARD_SLOTS   <- c("First Line", "Second Line", "Third Line", "Fourth Line")
LINE_DEFENSE_SLOTS   <- c("First Pair", "Second Pair", "Third Pair")

# ------------------------------------------------------------
# One row per shot per VT skater on the ice (5-skater shots only)
# ------------------------------------------------------------
build_line_events <- function(shots, on_ice, players) {
  empty <- data.frame(game_id = integer(0), event_id = integer(0),
                      xgf = numeric(0), xga = numeric(0),
                      skater_id = integer(0), player_name = character(0),
                      pos = character(0), stringsAsFactors = FALSE)
  
  ok <- function(df, cols) is.data.frame(df) && nrow(df) > 0 && all(cols %in% names(df))
  if (!ok(shots,   c("id", "game_id", "team", "x_coord", "y_coord")) ||
      !ok(on_ice,  c("shot_id", "player_id")) ||
      !ok(players, c("id", "full_name", "position"))) return(empty)
  
  # xG for every shot, in the same frame compute_plus_minus() uses:
  # normalize for period direction, then mirror opponent shots onto
  # the net at x = 189.
  shot_type <- if ("shot_type" %in% names(shots)) shots$shot_type else rep("wrist", nrow(shots))
  period    <- if ("period" %in% names(shots)) shots$period else rep("1", nrow(shots))
  canon     <- to_canonical_xy(shots$x_coord, shots$y_coord, period)
  known     <- !is.na(shots$team)
  is_vt     <- known & shots$team == "VT"
  
  xg <- mapply(function(x, y, st, vt) {
    if (is.na(x) || is.na(y)) return(NA_real_)
    if (vt) calc_xg_empirical(x, y, st) else calc_xg_empirical(200 - x, y, st)
  }, canon$x, canon$y, shot_type, is_vt)
  
  shot_xg <- data.frame(event_id = shots$id, game_id = shots$game_id,
                        xgf = ifelse(is_vt, xg, 0), xga = ifelse(is_vt, 0, xg))
  shot_xg <- shot_xg[known & !is.na(xg), ]
  
  # VT skaters (F/D only, goalies excluded) on the ice for each shot
  sk <- on_ice[!is.na(on_ice$shot_id) & !is.na(on_ice$player_id), c("shot_id", "player_id")]
  sk <- sk[!duplicated(sk), ]
  skater_info <- players[players$position %in% c("F", "D"), c("id", "full_name", "position")]
  sk <- merge(sk, skater_info, by.x = "player_id", by.y = "id")
  if (nrow(sk) == 0) return(empty)
  
  # Keep only shots with exactly 5 skaters (the original "5v5" filter)
  n_per_shot <- table(sk$shot_id)
  five <- as.integer(names(n_per_shot)[n_per_shot == 5L])
  sk <- sk[sk$shot_id %in% five, ]
  
  out <- merge(sk, shot_xg, by.x = "shot_id", by.y = "event_id")
  if (nrow(out) == 0) return(empty)
  
  data.frame(game_id = out$game_id, event_id = out$shot_id,
             xgf = out$xgf, xga = out$xga,
             skater_id = out$player_id, player_name = out$full_name,
             pos = out$position, stringsAsFactors = FALSE)
}

# ------------------------------------------------------------
# Game-level summary used for pairs and trios
# Input: one row per game with columns events, xgf, xga.
# ------------------------------------------------------------
line_summarize_games <- function(data) {
  events <- sum(data$events)
  gf     <- sum(data$xgf)
  ga     <- sum(data$xga)
  
  # Leave-one-game-out sensitivity ranges (not confidence intervals)
  remaining <- events - data$events
  loo <- matrix(NA_real_, nrow = 2L, ncol = 3L)
  if (nrow(data) > 1L) {
    rates <- 100 * cbind(gf - data$xgf,
                         ga - data$xga,
                         (gf - ga) - (data$xgf - data$xga)) / remaining
    loo <- apply(rates, 2, range)
  }
  
  tibble::tibble(
    events = events,
    games  = nrow(data),
    xgf_per_100  = if (events > 0) 100 * gf / events else NA_real_,
    xga_per_100  = if (events > 0) 100 * ga / events else NA_real_,
    net_per_100  = if (events > 0) 100 * (gf - ga) / events else NA_real_,
    xg_share_pct = if (gf + ga > 0) 100 * gf / (gf + ga) else NA_real_,
    largest_game_share = if (events > 0) max(data$events) / events else NA_real_,
    positive_games = sum(data$xgf > data$xga),
    negative_games = sum(data$xgf < data$xga),
    loo_xgf_min = loo[1, 1], loo_xgf_max = loo[2, 1],
    loo_xga_min = loo[1, 2], loo_xga_max = loo[2, 2],
    loo_net_min = loo[1, 3], loo_net_max = loo[2, 3]
  )
}

# ------------------------------------------------------------
# Pair report (same-position pairs) and forward-trio report
# ------------------------------------------------------------
compute_line_reports <- function(events) {
  empty <- list(pair_report = NULL, trio_report = NULL)
  if (!is.data.frame(events) || nrow(events) == 0) return(empty)
  
  pair_events <- events |>
    dplyr::select(game_id, event_id, pos,
                  player_a = skater_id, name_a = player_name, xgf, xga) |>
    dplyr::inner_join(
      events |> dplyr::select(game_id, event_id, pos,
                              player_b = skater_id, name_b = player_name),
      by = c("game_id", "event_id", "pos"),
      relationship = "many-to-many"
    ) |>
    dplyr::filter(player_a < player_b)
  
  pair_report <- pair_events |>
    dplyr::group_by(pos, player_a, name_a, player_b, name_b, game_id) |>
    dplyr::summarise(events = dplyr::n(), xgf = sum(xgf), xga = sum(xga),
                     .groups = "drop") |>
    dplyr::group_by(pos, player_a, name_a, player_b, name_b) |>
    dplyr::group_modify(~ line_summarize_games(.x)) |>
    dplyr::ungroup() |>
    dplyr::mutate(pair = paste(name_a, name_b, sep = " / "))
  
  # Forward trios: only shots with exactly 3 forwards (and so 2 defensemen)
  trio_shots <- events |>
    dplyr::filter(pos == "F") |>
    dplyr::group_by(game_id, event_id) |>
    dplyr::summarise(
      forwards = dplyr::n(),
      trio_id  = paste(sort(skater_id), collapse = "_"),
      trio     = paste(player_name[order(skater_id)], collapse = " / "),
      xgf      = dplyr::first(xgf),
      xga      = dplyr::first(xga),
      .groups  = "drop"
    ) |>
    dplyr::filter(forwards == 3L)
  
  trio_report <- if (nrow(trio_shots) == 0) NULL else
    trio_shots |>
    dplyr::group_by(trio_id, trio, game_id) |>
    dplyr::summarise(events = dplyr::n(), xgf = sum(xgf), xga = sum(xga),
                     .groups = "drop") |>
    dplyr::group_by(trio_id, trio) |>
    dplyr::group_modify(~ line_summarize_games(.x)) |>
    dplyr::ungroup()
  
  list(pair_report = pair_report, trio_report = trio_report)
}

# ------------------------------------------------------------
# Exact search for the best set of non-overlapping units.
# Highest-scoring unit gets the heaviest weight (same rule as the
# original script). Branch-and-bound instead of combn(), so it
# stays fast with a full season of trios.
# ------------------------------------------------------------
line_find_best_units <- function(units, scores, weights) {
  n_slots <- length(weights)
  n <- length(units)
  if (n < n_slots) return(NULL)
  
  ord    <- order(scores, decreasing = TRUE)
  units  <- units[ord]
  scores <- scores[ord]
  
  best_score <- -Inf
  best_idx   <- NULL
  
  search <- function(start, chosen, used, total) {
    k <- length(chosen)
    if (k == n_slots) {
      if (total > best_score) {
        best_score <<- total
        best_idx   <<- chosen
      }
      return(invisible(NULL))
    }
    if (start > n) return(invisible(NULL))
    remaining_w <- sum(weights[(k + 1):n_slots])
    for (i in start:n) {
      if (n - i + 1 < n_slots - k) break                         # not enough units left
      if (total + remaining_w * scores[i] <= best_score) break   # cannot beat current best
      if (any(units[[i]] %in% used)) next                        # player already used
      search(i + 1, c(chosen, i), c(used, units[[i]]),
             total + weights[k + 1] * scores[i])
    }
    invisible(NULL)
  }
  
  search(1, integer(0), character(0), 0)
  if (is.null(best_idx)) return(NULL)
  list(selected = ord[best_idx], score = best_score)
}

# If a full lineup is not possible (early season, single game), fall
# back to as many non-overlapping units as the data supports.
line_best_lineup <- function(units, scores, weights) {
  for (m in length(weights):1) {
    res <- line_find_best_units(units, scores, weights[seq_len(m)])
    if (!is.null(res)) return(res)
  }
  NULL
}

# ------------------------------------------------------------
# Three lineups: overall (net), offense (xGF), defense (xGA)
# Returns list(overall, offense, defense), each with $label,
# $forwards and $defense (data frames, or NULL if not enough data).
# ------------------------------------------------------------
line_build_lineups <- function(reports,
                               min_events = LINE_MIN_EVENTS,
                               min_games  = LINE_MIN_GAMES) {
  if (is.null(reports$pair_report)) return(NULL)
  
  fwd <- if (is.null(reports$trio_report)) NULL else
    reports$trio_report |>
    dplyr::filter(events >= min_events, games >= min_games) |>
    dplyr::rename(unit = trio)
  
  def <- reports$pair_report |>
    dplyr::filter(pos == "D", events >= min_events, games >= min_games) |>
    dplyr::rename(unit = pair)
  
  fwd_ids <- if (is.null(fwd) || nrow(fwd) == 0) list() else
    strsplit(as.character(fwd$trio_id), "_")
  def_ids <- lapply(seq_len(nrow(def)), function(i)
    c(as.character(def$player_a[i]), as.character(def$player_b[i])))
  
  pick <- function(df, ids, col, sign, weights, slots) {
    if (is.null(df) || nrow(df) == 0) return(NULL)
    scores <- sign * df[[col]]
    keep   <- is.finite(scores)
    df <- df[keep, , drop = FALSE]; ids <- ids[keep]; scores <- scores[keep]
    if (nrow(df) == 0) return(NULL)
    
    res <- line_best_lineup(ids, scores, weights)
    if (is.null(res)) return(NULL)
    
    chosen <- df[res$selected, , drop = FALSE]
    chosen$deployment_slot <- slots[seq_len(nrow(chosen))]
    chosen[, c("deployment_slot", "unit", "events", "games",
               "xgf_per_100", "xga_per_100", "net_per_100")]
  }
  
  objectives <- list(
    overall = list(label = "Best Overall",      col = "net_per_100", sign =  1),
    offense = list(label = "Offense-Maximized", col = "xgf_per_100", sign =  1),
    defense = list(label = "Defense-Maximized", col = "xga_per_100", sign = -1)
  )
  
  lapply(objectives, function(o) list(
    label    = o$label,
    forwards = pick(fwd, fwd_ids, o$col, o$sign, LINE_FORWARD_WEIGHTS, LINE_FORWARD_SLOTS),
    defense  = pick(def, def_ids, o$col, o$sign, LINE_DEFENSE_WEIGHTS, LINE_DEFENSE_SLOTS)
  ))
}

# ------------------------------------------------------------
# One skater's best linemates (same-position pairs), plus the best
# forward trio he has played on. Used by the Players tab.
# ------------------------------------------------------------
line_best_linemates <- function(reports, player_id, top_n = 5,
                                min_events = LINE_MIN_EVENTS) {
  out <- list(partners = NULL, best_line = NULL, pos = NA_character_)
  pr <- reports$pair_report
  if (is.null(pr) || nrow(pr) == 0) return(out)
  
  mine <- pr[pr$player_a == player_id | pr$player_b == player_id, ]
  if (nrow(mine) == 0) return(out)
  out$pos <- mine$pos[1]
  
  mine$linemate <- ifelse(mine$player_a == player_id, mine$name_b, mine$name_a)
  mine <- mine[mine$events >= min_events & is.finite(mine$net_per_100), ]
  mine <- mine[order(mine$net_per_100, decreasing = TRUE), ]
  out$partners <- utils::head(
    mine[, c("linemate", "events", "games", "xgf_per_100", "xga_per_100", "net_per_100")],
    top_n)
  
  tr <- reports$trio_report
  if (!is.null(tr) && nrow(tr) > 0 && identical(out$pos, "F")) {
    has_me <- vapply(strsplit(as.character(tr$trio_id), "_"),
                     function(ids) as.character(player_id) %in% ids, logical(1))
    mine_tr <- tr[has_me & tr$events >= min_events & is.finite(tr$net_per_100), ]
    if (nrow(mine_tr) > 0) out$best_line <- mine_tr[which.max(mine_tr$net_per_100), ]
  }
  out
}
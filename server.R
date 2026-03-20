# ============================================================
# server.R  -  All reactive logic and data submission
# ============================================================

server <- function(input, output, session) {

  # ----------------------------------------------------------
  # SESSION STATE
  # Reactive values shared across all tabs for this user's session
  # ----------------------------------------------------------
  rv <- reactiveValues(
    collector_name = NULL,   # name entered on Setup tab
    game_id        = NULL,   # id of the selected game
    game_label     = NULL,   # human-readable game description
    players        = NULL,   # named vector: player label -> id
    setup_done     = FALSE,  # TRUE once Start button is clicked

    # Shot location from rink click
    pending_shot_x = NULL,
    pending_shot_y = NULL,

    # Rolling logs of recent entries (list of character strings)
    faceoff_log = list(),
    hit_log     = list(),
    shot_log    = list(),
    goal_log    = list(),

    # Most recent record id for each table (for undo)
    last_faceoff_id = NULL,
    last_hit_id     = NULL,
    last_shot_id    = NULL,
    last_goal_id    = NULL
  )

  # ----------------------------------------------------------
  # CONNECTION STATUS CHECK  (shown on Setup tab)
  # ----------------------------------------------------------
  output$connection_status <- renderUI({
    connected <- sb_check_connection()
    if (connected) {
      div(class = "alert alert-success py-2 mb-0",
          icon("circle-check"), " Connected to Supabase database")
    } else {
      div(class = "alert alert-danger py-2 mb-0",
          icon("circle-xmark"),
          " Cannot reach Supabase. Check your .env file and internet connection.")
    }
  })

  # ----------------------------------------------------------
  # LOAD GAME LIST (Setup tab)
  # ----------------------------------------------------------
  games_list <- reactiveVal(c())

  load_games <- function() {
    games <- sb_load_games()
    games_list(games)
    updateSelectInput(session, "game_select", choices = games)
  }

  # Load on startup (tryCatch so a missing .env doesn't crash the app)
  observe({ tryCatch(load_games(), error = function(e) NULL) })

  # Reload when refresh button clicked
  observeEvent(input$btn_refresh_games, { load_games() })

  # ----------------------------------------------------------
  # CREATE A NEW GAME
  # ----------------------------------------------------------
  observeEvent(input$btn_create_game, {
    req(input$new_game_opponent)
    if (trimws(input$new_game_opponent) == "") {
      notify_err("Please enter the opponent name.")
      return()
    }

    result <- sb_insert("games", list(
      game_date = as.character(input$new_game_date),
      opponent  = trimws(input$new_game_opponent),
      location  = input$new_game_location,
      season    = trimws(input$new_game_season)
    ))

    if (result$success) {
      notify_ok(paste("Game created vs", input$new_game_opponent))
      load_games()
      # Auto-select the new game
      updateSelectInput(session, "game_select",
                        selected = as.character(result$id))
    } else {
      notify_err(result$error)
    }
  })

  # ----------------------------------------------------------
  # START SESSION
  # ----------------------------------------------------------
  observeEvent(input$btn_start_session, {
    if (trimws(input$collector_name) == "") {
      notify_err("Please enter your name.")
      return()
    }
    if (is.null(input$game_select) || input$game_select == "") {
      notify_err("Please select a game (or create one above).")
      return()
    }

    rv$collector_name <- trimws(input$collector_name)
    rv$game_id        <- as.integer(input$game_select)
    rv$game_label     <- names(games_list()[games_list() == input$game_select])
    rv$setup_done     <- TRUE

    # Load player roster from Supabase
    players <- sb_load_players()
    rv$players <- players

    # Populate all player dropdowns across tabs
    updateSelectizeInput(session, "fo_player",      choices = players, server = TRUE)
    updateSelectizeInput(session, "hit_player",     choices = players, server = TRUE)
    updateSelectizeInput(session, "shot_player",    choices = players, server = TRUE)
    updateSelectizeInput(session, "goal_scorer",    choices = players, server = TRUE)
    updateSelectizeInput(session, "goal_assist1",
                         choices = c("(none)" = "", players), server = TRUE)
    updateSelectizeInput(session, "goal_assist2",
                         choices = c("(none)" = "", players), server = TRUE)
    updateSelectizeInput(session, "on_ice_players", choices = players, server = TRUE)

    notify_ok(paste0("Session started!  Collector: ", rv$collector_name,
                     "  |  Game: ", rv$game_label))
  })

  # ----------------------------------------------------------
  # SESSION BANNER (shown at the top of every tab)
  # ----------------------------------------------------------
  output$session_banner <- renderUI({
    if (!rv$setup_done) return(NULL)
    div(
      class = "alert alert-primary mb-0 py-2 px-3 rounded-0",
      style = paste0("background:", VT_MAROON, "; color:white; border:none;"),
      icon("user"), " ", tags$strong(rv$collector_name),
      "  |  ",
      icon("hockey-puck"), " ", rv$game_label
    )
  })

  # ----------------------------------------------------------
  # SESSION REQUIRED NOTICES (shown when user visits a tab without setup)
  # ----------------------------------------------------------
  session_required_ui <- function(tab_name) {
    renderUI({
      if (rv$setup_done) return(NULL)
      div(
        class = "alert alert-warning",
        icon("triangle-exclamation"),
        tags$strong(" Setup required."),
        paste(" Please go to the Setup tab, enter your name, select a game,",
              "and click Start Collecting Data.")
      )
    })
  }

  output$session_required_faceoffs <- session_required_ui("faceoffs")
  output$session_required_hits     <- session_required_ui("hits")
  output$session_required_shots    <- session_required_ui("shots")
  output$session_required_goals    <- session_required_ui("goals")

  # ----------------------------------------------------------
  # GOAL TAB: show scorer/assist fields only for VT goals
  # ----------------------------------------------------------
  output$goal_vt_fields <- renderUI({
    req(input$goal_team)
    if (input$goal_team == "opponent") {
      return(helpText("Recording opponent goal — no scorer details needed."))
    }
    tagList(
      selectizeInput("goal_scorer", "Scorer",
                     choices = if (!is.null(rv$players)) rv$players else NULL,
                     width = "100%",
                     options = list(placeholder = "Start typing name or #...")),
      fluidRow(
        column(6,
          selectizeInput("goal_assist1", "Primary assist",
                         choices = c("(none)" = "",
                                     if (!is.null(rv$players)) rv$players else NULL),
                         width = "100%",
                         options = list(placeholder = "(none)"))
        ),
        column(6,
          selectizeInput("goal_assist2", "Secondary assist",
                         choices = c("(none)" = "",
                                     if (!is.null(rv$players)) rv$players else NULL),
                         width = "100%",
                         options = list(placeholder = "(none)"))
        )
      )
    )
  })

  # ----------------------------------------------------------
  # RINK CLICK → store pending shot location
  # ----------------------------------------------------------
  observeEvent(input$rink_click, {
    x <- input$rink_click$x
    y <- input$rink_click$y
    # Only accept clicks within the rink bounds
    if (!is.null(x) && !is.null(y) && x >= 0 && x <= 200 && y >= 0 && y <= 85) {
      rv$pending_shot_x <- round(x, 1)
      rv$pending_shot_y <- round(y, 1)
    }
  })

  observeEvent(input$btn_clear_shot_loc, {
    rv$pending_shot_x <- NULL
    rv$pending_shot_y <- NULL
  })

  output$shot_coords_display <- renderUI({
    if (is.null(rv$pending_shot_x)) {
      span(style = "color:#555;", icon("circle-info"),
           " No location selected — click the rink")
    } else {
      span(icon("location-dot"), " Shot location selected: ",
           tags$strong(paste0("x=", rv$pending_shot_x, "  y=", rv$pending_shot_y)))
    }
  })

  # ----------------------------------------------------------
  # RINK PLOT (Shots tab)
  # ----------------------------------------------------------
  output$rink_plot <- renderPlot({
    # Optionally overlay shots already recorded this game
    shots_data <- NULL
    if (isTRUE(input$show_game_shots) && !is.null(rv$game_id)) {
      shots_data <- tryCatch(
        sb_select("shots",
                  select  = "x_coord,y_coord,result",
                  filters = c("game_id" = paste0("eq.", rv$game_id))),
        error = function(e) NULL
      )
    }
    draw_rink(shots_df = shots_data,
              pending_x = rv$pending_shot_x,
              pending_y = rv$pending_shot_y)
  })

  # ----------------------------------------------------------
  # SUBMIT FACEOFF
  # ----------------------------------------------------------
  observeEvent(input$btn_submit_faceoff, {
    req(rv$setup_done)
    if (is.null(input$fo_player) || input$fo_player == "") {
      notify_err("Please select a player."); return()
    }

    disable("btn_submit_faceoff")
    on.exit(enable("btn_submit_faceoff"))

    result <- sb_insert("faceoffs", list(
      game_id        = rv$game_id,
      period         = input$fo_period,
      game_time      = trimws(input$fo_time),
      player_id      = as.integer(input$fo_player),
      result         = input$fo_result,
      zone           = input$fo_zone,
      collector_name = rv$collector_name
    ))

    if (result$success) {
      rv$last_faceoff_id <- result$id
      # Determine player label for the log
      player_label <- names(rv$players[rv$players == input$fo_player])
      entry <- paste0(input$fo_period, "P  ", input$fo_time, "  ",
                      player_label, "  — ", toupper(input$fo_result),
                      "  (", input$fo_zone, ")")
      rv$faceoff_log <- c(list(entry), rv$faceoff_log)[1:min(5, length(rv$faceoff_log) + 1)]
      notify_ok("Faceoff recorded!")
    } else {
      notify_err(result$error)
    }
  })

  observeEvent(input$btn_undo_faceoff, {
    req(!is.null(rv$last_faceoff_id))
    res <- sb_delete("faceoffs", rv$last_faceoff_id)
    if (res$success) {
      rv$last_faceoff_id <- NULL
      rv$faceoff_log <- rv$faceoff_log[-1]
      notify_ok("Last faceoff entry deleted.")
    } else {
      notify_err("Could not undo — record may be too old (5 min limit).")
    }
  })

  output$faceoff_log <- renderUI({
    if (length(rv$faceoff_log) == 0) return(p("No entries yet."))
    tags$ol(lapply(rv$faceoff_log, tags$li))
  })

  # ----------------------------------------------------------
  # SUBMIT HIT
  # ----------------------------------------------------------
  observeEvent(input$btn_submit_hit, {
    req(rv$setup_done)
    if (is.null(input$hit_player) || input$hit_player == "") {
      notify_err("Please select a player."); return()
    }

    disable("btn_submit_hit")
    on.exit(enable("btn_submit_hit"))

    result <- sb_insert("hits", list(
      game_id        = rv$game_id,
      period         = input$hit_period,
      game_time      = trimws(input$hit_time),
      player_id      = as.integer(input$hit_player),
      hit_type       = input$hit_type,
      collector_name = rv$collector_name
    ))

    if (result$success) {
      rv$last_hit_id <- result$id
      player_label <- names(rv$players[rv$players == input$hit_player])
      entry <- paste0(input$hit_period, "P  ", input$hit_time, "  ",
                      player_label, "  — ", input$hit_type)
      rv$hit_log <- c(list(entry), rv$hit_log)[1:min(5, length(rv$hit_log) + 1)]
      notify_ok("Hit recorded!")
    } else {
      notify_err(result$error)
    }
  })

  observeEvent(input$btn_undo_hit, {
    req(!is.null(rv$last_hit_id))
    res <- sb_delete("hits", rv$last_hit_id)
    if (res$success) {
      rv$last_hit_id <- NULL
      rv$hit_log <- rv$hit_log[-1]
      notify_ok("Last hit entry deleted.")
    } else {
      notify_err("Could not undo — record may be too old.")
    }
  })

  output$hit_log <- renderUI({
    if (length(rv$hit_log) == 0) return(p("No entries yet."))
    tags$ol(lapply(rv$hit_log, tags$li))
  })

  # ----------------------------------------------------------
  # SUBMIT SHOT
  # ----------------------------------------------------------
  observeEvent(input$btn_submit_shot, {
    req(rv$setup_done)
    if (is.null(input$shot_player) || input$shot_player == "") {
      notify_err("Please select a player."); return()
    }

    disable("btn_submit_shot")
    on.exit(enable("btn_submit_shot"))

    shot_data <- list(
      game_id        = rv$game_id,
      period         = input$shot_period,
      game_time      = trimws(input$shot_time),
      player_id      = as.integer(input$shot_player),
      shot_type      = input$shot_type,
      result         = input$shot_result,
      team           = input$shot_team,
      x_coord        = rv$pending_shot_x,
      y_coord        = rv$pending_shot_y,
      collector_name = rv$collector_name
    )
    # Remove NULL coordinates so Supabase stores NULL in DB
    shot_data <- Filter(Negate(is.null), shot_data)

    result <- sb_insert("shots", shot_data)

    if (result$success) {
      rv$last_shot_id <- result$id
      player_label <- names(rv$players[rv$players == input$shot_player])
      loc_str <- if (!is.null(rv$pending_shot_x))
        paste0(" @ (", rv$pending_shot_x, ",", rv$pending_shot_y, ")") else ""
      entry <- paste0(input$shot_period, "P  ", input$shot_time, "  ",
                      player_label, "  — ", input$shot_result, loc_str)
      rv$shot_log <- c(list(entry), rv$shot_log)[1:min(5, length(rv$shot_log) + 1)]
      # Clear the pending location after submit
      rv$pending_shot_x <- NULL
      rv$pending_shot_y <- NULL
      notify_ok("Shot recorded!")
    } else {
      notify_err(result$error)
    }
  })

  observeEvent(input$btn_undo_shot, {
    req(!is.null(rv$last_shot_id))
    res <- sb_delete("shots", rv$last_shot_id)
    if (res$success) {
      rv$last_shot_id <- NULL
      rv$shot_log <- rv$shot_log[-1]
      notify_ok("Last shot entry deleted.")
    } else {
      notify_err("Could not undo — record may be too old.")
    }
  })

  output$shot_log <- renderUI({
    if (length(rv$shot_log) == 0) return(p("No entries yet."))
    tags$ol(lapply(rv$shot_log, tags$li))
  })

  # ----------------------------------------------------------
  # SUBMIT GOAL + ON-ICE PLAYERS
  # ----------------------------------------------------------
  observeEvent(input$btn_submit_goal, {
    req(rv$setup_done)

    disable("btn_submit_goal")
    on.exit(enable("btn_submit_goal"))

    # Build goal record
    goal_data <- list(
      game_id        = rv$game_id,
      period         = input$goal_period,
      game_time      = trimws(input$goal_time),
      situation      = input$goal_situation,
      empty_net      = isTRUE(input$goal_empty_net),
      team           = input$goal_team,
      collector_name = rv$collector_name
    )

    # Add VT-specific scorer/assist fields
    if (input$goal_team == "VT") {
      if (!is.null(input$goal_scorer) && input$goal_scorer != "") {
        goal_data$scorer_id <- as.integer(input$goal_scorer)
      }
      if (!is.null(input$goal_assist1) && input$goal_assist1 != "") {
        goal_data$assist1_id <- as.integer(input$goal_assist1)
      }
      if (!is.null(input$goal_assist2) && input$goal_assist2 != "") {
        goal_data$assist2_id <- as.integer(input$goal_assist2)
      }
    }

    goal_result <- sb_insert("goals", goal_data)

    if (!goal_result$success) {
      notify_err(goal_result$error)
      return()
    }

    goal_id <- goal_result$id
    rv$last_goal_id <- goal_id

    # Insert one on_ice row per selected player
    on_ice_players <- input$on_ice_players
    if (length(on_ice_players) > 0) {
      for (pid in on_ice_players) {
        sb_insert("on_ice", list(
          game_id        = rv$game_id,
          goal_id        = goal_id,
          period         = input$goal_period,
          game_time      = trimws(input$goal_time),
          player_id      = as.integer(pid),
          team           = "VT",
          collector_name = rv$collector_name
        ))
      }
    }

    # Build log entry
    if (input$goal_team == "VT" &&
        !is.null(input$goal_scorer) && input$goal_scorer != "") {
      scorer_label <- names(rv$players[rv$players == input$goal_scorer])
      entry <- paste0(input$goal_period, "P  ", input$goal_time,
                      "  GOAL — ", scorer_label,
                      "  [", length(on_ice_players), " on ice]")
    } else {
      entry <- paste0(input$goal_period, "P  ", input$goal_time,
                      "  OPPONENT GOAL")
    }
    rv$goal_log <- c(list(entry), rv$goal_log)[1:min(5, length(rv$goal_log) + 1)]

    notify_ok(if (input$goal_team == "VT") "GOAL! Hokies score!" else "Opponent goal recorded.")
  })

  observeEvent(input$btn_undo_goal, {
    req(!is.null(rv$last_goal_id))
    res <- sb_delete("goals", rv$last_goal_id)
    if (res$success) {
      rv$last_goal_id <- NULL
      rv$goal_log <- rv$goal_log[-1]
      notify_ok("Last goal entry deleted.")
    } else {
      notify_err("Could not undo — record may be too old.")
    }
  })

  output$goal_log <- renderUI({
    if (length(rv$goal_log) == 0) return(p("No entries yet."))
    tags$ol(lapply(rv$goal_log, tags$li))
  })

  # ----------------------------------------------------------
  # SUMMARY TAB
  # ----------------------------------------------------------
  summary_data <- reactiveValues(
    faceoffs = data.frame(),
    hits     = data.frame(),
    shots    = data.frame(),
    goals    = data.frame(),
    on_ice   = data.frame()
  )

  observeEvent(input$btn_refresh_summary, {
    req(rv$game_id)
    gid <- paste0("eq.", rv$game_id)

    summary_data$faceoffs <- sb_select("faceoffs", filters = c("game_id" = gid))
    summary_data$hits     <- sb_select("hits",     filters = c("game_id" = gid))
    summary_data$shots    <- sb_select("shots",    filters = c("game_id" = gid))
    summary_data$goals    <- sb_select("goals",    filters = c("game_id" = gid))
    summary_data$on_ice   <- sb_select("on_ice",   filters = c("game_id" = gid))

    notify_ok("Summary data refreshed!")
  })

  output$summary_value_boxes <- renderUI({
    n_shots    <- nrow(summary_data$shots)
    n_goals    <- if (nrow(summary_data$goals) > 0 && "team" %in% names(summary_data$goals))
      sum(summary_data$goals$team == "VT", na.rm = TRUE) else 0
    n_hits     <- nrow(summary_data$hits)
    n_faceoffs <- nrow(summary_data$faceoffs)
    n_fo_wins  <- if (n_faceoffs > 0)
      sum(summary_data$faceoffs$result == "win", na.rm = TRUE) else 0

    stat_box <- function(label, value, color) {
      div(
        class = "card text-center mb-3",
        style = paste0("border-top: 4px solid ", color, ";"),
        div(class = "card-body py-3",
          h2(value, style = paste0("color:", color, "; font-weight:700; margin:0;")),
          p(label, class = "mb-0 text-muted fw-bold")
        )
      )
    }

    fluidRow(
      column(3, stat_box("Shots",        n_shots,                          VT_MAROON)),
      column(3, stat_box("VT Goals",     n_goals,                          "#c0392b")),
      column(3, stat_box("Hits",         n_hits,                           VT_ORANGE)),
      column(3, stat_box("Faceoff Wins", paste0(n_fo_wins, " / ", n_faceoffs), "#2471a3"))
    )
  })

  output$summary_shot_map <- renderPlot({
    df <- summary_data$shots
    shots_with_loc <- if (nrow(df) > 0 && "x_coord" %in% names(df))
      df[!is.na(df$x_coord), ] else NULL
    draw_rink(shots_df = shots_with_loc)
  })

  output$summary_goal_list <- renderUI({
    df <- summary_data$goals
    if (nrow(df) == 0) return(p("No goals recorded yet."))
    items <- apply(df, 1, function(row) {
      tags$li(paste0(row["team"], " goal — P", row["period"], " ", row["game_time"]))
    })
    tags$ul(items)
  })

  make_dt <- function(df) {
    if (nrow(df) == 0) return(datatable(data.frame(Message = "No data yet.")))
    datatable(df,
              options = list(pageLength = 15, scrollX = TRUE),
              rownames = FALSE,
              class    = "table-striped table-hover")
  }

  output$table_faceoffs <- renderDT({ make_dt(summary_data$faceoffs) })
  output$table_hits     <- renderDT({ make_dt(summary_data$hits) })
  output$table_shots    <- renderDT({ make_dt(summary_data$shots) })
  output$table_goals    <- renderDT({ make_dt(summary_data$goals) })
  output$table_on_ice   <- renderDT({ make_dt(summary_data$on_ice) })

  # ============================================================
  # PLAYER ANALYSIS TAB
  # ============================================================

  # Populate player dropdown once session starts
  observe({
    req(rv$setup_done, rv$players)
    skater_choices <- rv$players[
      !grepl("\\(G\\)", names(rv$players))   # exclude goalies
    ]
    updateSelectizeInput(session, "player_select",
                         choices = skater_choices, server = TRUE)
  })

  # Warn if session not started
  output$player_session_warn <- renderUI({
    if (rv$setup_done) return(NULL)
    div(class = "alert alert-warning",
        icon("triangle-exclamation"),
        " Go to the Setup tab first and start a session to load player data.")
  })

  # Load all data for analytics (reactive so it refreshes on demand)
  all_analytics <- reactive({
    req(rv$setup_done)
    list(
      players  = sb_select("players",  filters = c("active" = "eq.true")),
      shots    = sb_select("shots"),
      goals    = sb_select("goals"),
      faceoffs = sb_select("faceoffs"),
      hits     = sb_select("hits"),
      n_games  = max(nrow(sb_select("games")), 1)
    )
  })

  # Compute full team stats table (all players, all games)
  team_stats <- reactive({
    d <- all_analytics()
    compute_player_stats(
      players  = d$players,
      shots    = d$shots,
      goals    = d$goals,
      faceoffs = d$faceoffs,
      hits     = d$hits,
      n_games  = d$n_games
    )
  })

  # Selected player's row
  selected_player_stats <- reactive({
    req(input$player_select, nrow(team_stats()) > 0)
    team_stats() |>
      dplyr::filter(player_id == as.integer(input$player_select))
  })

  # ---- Player header banner ----
  output$player_header <- renderUI({
    req(nrow(selected_player_stats()) > 0)
    p <- selected_player_stats()
    div(
      class = "p-3 rounded",
      style = paste0("background:", VT_MAROON, "; color:white;"),
      tags$span(style = "font-size:2rem; font-weight:700; margin-right:12px;",
                paste0("#", p$jersey_num)),
      tags$span(style = "font-size:1.4rem; font-weight:600;",
                p$full_name),
      tags$span(
        style = paste0("margin-left:16px; background:", VT_ORANGE,
                       "; padding:3px 10px; border-radius:12px;",
                       " font-weight:600;"),
        p$position
      )
    )
  })

  # ---- Stat boxes ----
  output$player_stat_boxes <- renderUI({
    req(nrow(selected_player_stats()) > 0)
    p <- selected_player_stats()

    box <- function(val, lbl, color = VT_MAROON) {
      column(2,
        div(class = "card text-center",
            style = paste0("border-top:4px solid ", color, ";"),
            div(class = "card-body py-2",
                h3(val,  style = paste0("color:", color, "; font-weight:700; margin:0;")),
                p(lbl,   class = "mb-0 text-muted", style = "font-size:0.8rem; font-weight:600;")
            ))
      )
    }

    fluidRow(
      box(p$goals,      "Goals",         VT_MAROON),
      box(p$a1 + p$a2,  "Assists",       "#2471a3"),
      box(p$points,     "Points",        "#1a5276"),
      box(p$shots,      "Shots",         "#6c757d"),
      box(paste0(p$shoot_pct, "%"), "Shoot%", "#117a65"),
      box(round(p$xg_total, 1), "xG Total", "#7d3c98")
    )
  })

  # ---- Percentile bars ----
  output$player_percentiles <- renderUI({
    req(nrow(selected_player_stats()) > 0, nrow(team_stats()) > 1)
    p  <- selected_player_stats()
    ts <- team_stats()

    # helper: compute percentile for this player vs all teammates
    pct <- function(col) calc_percentile(p[[col]], ts[[col]])

    tagList(
      pct_bar("Points / Game",      paste0(p$pts_per_game, " pts"),
              pct("pts_per_game")),
      pct_bar("Goals Above Expected",
              paste0(ifelse(p$g_minus_xg >= 0, "+", ""), p$g_minus_xg),
              pct("g_minus_xg")),
      pct_bar("xG per Game",        paste0(p$xg_per_game, " xG"),
              pct("xg_per_game")),
      pct_bar("Shot Quality (xG/Shot)", paste0(round(p$xg_per_shot * 100, 1), "%"),
              pct("xg_per_shot")),
      pct_bar("High Danger Shot %", paste0(p$hd_pct, "%"),
              pct("hd_pct")),
      pct_bar("Shots / Game",       paste0(p$shots_per_game),
              pct("shots_per_game")),
      pct_bar("Hits / Game",        paste0(p$hits_per_game),
              pct("hits_per_game")),
      if (!is.na(p$fo_pct))
        pct_bar("Faceoff Win %", paste0(p$fo_pct, "%  (", p$fo_total, " taken)"),
                pct("fo_pct"))
    )
  })

  # ---- Advanced metrics card ----
  output$player_advanced <- renderUI({
    req(nrow(selected_player_stats()) > 0)
    p <- selected_player_stats()

    metric_row <- function(label, value, note = NULL) {
      div(class = "d-flex justify-content-between border-bottom py-2",
          div(tags$b(label),
              if (!is.null(note))
                tags$small(class = "text-muted d-block", note)),
          div(style = "font-size:1.1rem; font-weight:700;", value)
      )
    }

    tagList(
      metric_row("xG Total", round(p$xg_total, 2),
                 "Sum of shot probability across all shots taken"),
      metric_row("Goals − xG", paste0(ifelse(p$g_minus_xg >= 0, "+", ""), p$g_minus_xg),
                 "Positive = scoring above expectation"),
      metric_row("Shooting %", paste0(p$shoot_pct, "%"),
                 paste0(p$goals, " goals on ", p$shots, " shots")),
      metric_row("Shots on Goal %", paste0(p$on_goal_pct, "%"),
                 "% of shot attempts that were on target"),
      metric_row("High Danger Shots", paste0(p$high_danger, " (", p$hd_pct, "%)"),
                 "Shots from ≤25 ft at angle ≤45°"),
      metric_row("avg xG per Shot", paste0(round(p$xg_per_shot, 3)),
                 "Shot quality: higher = more dangerous locations"),
      if (p$fo_total > 0)
        metric_row("Faceoff W−L", paste0(p$fo_wins, "−", p$fo_total - p$fo_wins),
                   paste0(p$fo_pct, "% win rate"))
    )
  })

  # ---- Personal shot map ----
  output$player_shot_map <- renderPlot({
    req(input$player_select)
    d <- all_analytics()

    player_shots <- tryCatch({
      s <- d$shots
      if (nrow(s) == 0 || !"player_id" %in% names(s)) return(draw_rink())
      s <- s[s$player_id == as.integer(input$player_select) &
               !is.na(s$x_coord), ]
      if (nrow(s) == 0) return(draw_rink())
      s$result <- ifelse(is.na(s$result), "save", s$result)
      s
    }, error = function(e) NULL)

    if (is.null(player_shots) || nrow(player_shots) == 0) {
      draw_rink()
    } else {
      draw_rink(shots_df = player_shots)
    }
  })

  # ---- Game-by-game trend chart ----
  output$player_trend <- renderPlot({
    req(input$player_select)
    d     <- all_analytics()
    pid   <- as.integer(input$player_select)
    games_df <- sb_select("games", order = "game_date.asc")

    if (nrow(games_df) == 0) {
      plot.new(); text(0.5, 0.5, "No game data yet", cex = 1.2); return()
    }

    # Points per game
    g_pts <- if (nrow(d$goals) > 0 && "scorer_id" %in% names(d$goals)) {
      vg <- dplyr::filter(d$goals, team == "VT", !is.na(team))

      scorer <- vg |> dplyr::filter(!is.na(scorer_id),  scorer_id  == pid) |>
        dplyr::select(game_id) |> dplyr::mutate(pts = 1L)
      a1_pts <- vg |> dplyr::filter(!is.na(assist1_id), assist1_id == pid) |>
        dplyr::select(game_id) |> dplyr::mutate(pts = 1L)
      a2_pts <- vg |> dplyr::filter(!is.na(assist2_id), assist2_id == pid) |>
        dplyr::select(game_id) |> dplyr::mutate(pts = 1L)

      dplyr::bind_rows(scorer, a1_pts, a2_pts) |>
        dplyr::group_by(game_id) |>
        dplyr::summarise(points = sum(pts), .groups = "drop")
    } else data.frame(game_id = integer(0), points = integer(0))

    # xG per game
    g_xg <- if (nrow(d$shots) > 0 && "player_id" %in% names(d$shots)) {
      ps <- d$shots[d$shots$player_id == pid & !is.na(d$shots$player_id), ]
      if (nrow(ps) > 0) {
        ps <- add_xg(ps)
        ps |> dplyr::group_by(game_id) |>
          dplyr::summarise(xg = round(sum(xg, na.rm = TRUE), 2), .groups = "drop")
      } else data.frame(game_id = integer(0), xg = numeric(0))
    } else data.frame(game_id = integer(0), xg = numeric(0))

    trend <- games_df |>
      dplyr::select(game_id = id, game_date, opponent) |>
      dplyr::left_join(g_pts, by = "game_id") |>
      dplyr::left_join(g_xg,  by = "game_id") |>
      dplyr::mutate(
        points = tidyr::replace_na(points, 0),
        xg     = tidyr::replace_na(xg, 0),
        label  = paste0(substr(opponent, 1, 10))
      )

    if (max(c(trend$points, trend$xg)) == 0) {
      plot.new()
      text(0.5, 0.5, "No scoring/shot data for this player yet", cex = 1.1)
      return()
    }

    ggplot(trend, aes(x = label)) +
      geom_col(aes(y = points, fill = "Points"), width = 0.4,
               position = position_nudge(x = -0.22)) +
      geom_col(aes(y = xg, fill = "xG"), width = 0.4,
               position = position_nudge(x = 0.22)) +
      scale_fill_manual(values = c("Points" = VT_MAROON, "xG" = VT_ORANGE),
                        name = NULL) +
      labs(x = "Opponent", y = "Value") +
      theme_minimal(base_size = 12) +
      theme(
        legend.position  = "top",
        axis.text.x      = element_text(angle = 30, hjust = 1),
        panel.grid.major.x = element_blank()
      )
  })

  # ============================================================
  # REPORTS TAB
  # ============================================================

  # ============================================================
  # SCHEDULE TAB
  # ============================================================

  # -- State for report generation --
  sched_report_path    <- reactiveVal(NULL)
  sched_report_game_id <- reactiveVal(NULL)   # NULL = full season
  sched_report_label   <- reactiveVal("Full Season")
  sched_report_trigger <- reactiveVal(0L)

  # -- Schedule data: games + computed scores from goals --
  schedule_data <- reactiveVal(data.frame())

  load_schedule <- function() {
    tryCatch({
      gms <- sb_select("games", order = "game_date.asc")
      if (nrow(gms) == 0) { schedule_data(gms); return() }

      goals_df <- sb_select("goals", select = "game_id,team")
      if (nrow(goals_df) > 0) {
        gf <- dplyr::filter(goals_df, team == "VT") |>
          dplyr::count(game_id, name = "gf")
        ga <- dplyr::filter(goals_df, team == "opponent") |>
          dplyr::count(game_id, name = "ga")
        gms <- gms |>
          dplyr::left_join(gf, by = c("id" = "game_id")) |>
          dplyr::left_join(ga, by = c("id" = "game_id")) |>
          dplyr::mutate(gf = tidyr::replace_na(gf, 0L),
                        ga = tidyr::replace_na(ga, 0L))
      } else {
        gms$gf <- 0L; gms$ga <- 0L
      }
      schedule_data(gms)
    }, error = function(e) NULL)
  }

  observe({ tryCatch(load_schedule(), error = function(e) NULL) })
  observeEvent(input$btn_refresh_schedule, { load_schedule() })

  # -- Helper: build one game card as a div() --
  make_game_card <- function(game, is_past) {
    has_score <- is_past && "gf" %in% names(game) && !is.na(game$gf)

    if (has_score) {
      result       <- if (game$gf > game$ga) "W" else if (game$gf < game$ga) "L" else "T"
      border_col   <- switch(result, W = "#198754", L = "#dc3545", "#6c757d")
      result_badge <- div(
        style = paste0("display:inline-flex; align-items:center; gap:8px;",
                       " background:", border_col, "; color:white;",
                       " padding:5px 14px; border-radius:20px;",
                       " font-weight:700; font-size:0.95rem;"),
        tags$span(result), tags$span(paste0(game$gf, " \u2013 ", game$ga))
      )
    } else {
      border_col   <- VT_MAROON
      result_badge <- div(
        style = paste0("display:inline-block; background:#6c757d; color:white;",
                       " padding:4px 12px; border-radius:20px;",
                       " font-weight:600; font-size:0.82rem;"),
        "UPCOMING"
      )
    }

    date_str  <- format(as.Date(game$game_date), "%A, %B %d, %Y")
    loc_color <- if (!is.null(game$location) && game$location == "home")
      VT_MAROON else "#495057"
    loc_badge <- div(
      style = paste0("display:inline-block; background:", loc_color,
                     "; color:white; padding:2px 9px; border-radius:6px;",
                     " font-size:0.75rem; font-weight:700;"),
      toupper(if (!is.null(game$location)) game$location else "")
    )

    # VT logo — inline onerror fallback to "VT" circle
    vt_div <- div(class = "text-center",
      tags$img(src = "vt_logo.png", height = "72px",
        style = "object-fit:contain; max-width:90px;",
        onerror = paste0(
          "this.outerHTML='<div style=\"width:72px;height:72px;border-radius:50%;",
          "background:#861F41;color:white;display:flex;align-items:center;",
          "justify-content:center;font-weight:700;font-size:1.3rem;\">VT</div>'"
        )
      ),
      tags$p("Virginia Tech",
             style = "font-weight:700; margin:6px 0 0; font-size:0.88rem;")
    )

    # Opponent logo (URL) or styled initials circle
    initials <- paste(substring(strsplit(trimws(game$opponent), "\\s+")[[1]],
                                1, 1), collapse = "")
    initials <- substring(initials, 1, 3)
    has_logo <- "opponent_logo_url" %in% names(game) &&
                !is.na(game$opponent_logo_url) &&
                nchar(trimws(as.character(game$opponent_logo_url))) > 0
    opp_graphic <- if (has_logo) {
      tags$img(src = game$opponent_logo_url, height = "72px",
               style = "object-fit:contain; max-width:90px;")
    } else {
      div(style = paste0("width:72px; height:72px; border-radius:50%;",
                         " background:#4a4a4a; color:white;",
                         " display:flex; align-items:center;",
                         " justify-content:center; font-weight:700;",
                         " font-size:1.1rem; margin:auto;"), initials)
    }
    opp_div <- div(class = "text-center", opp_graphic,
      tags$p(game$opponent,
             style = "font-weight:700; margin:6px 0 0; font-size:0.88rem;")
    )

    center_div <- if (has_score) {
      div(class = "text-center px-4",
          div(style = "font-size:2.2rem; font-weight:800; line-height:1;",
              paste0(game$gf, "  \u2013  ", game$ga)),
          div(style = "font-size:0.72rem; color:#888; margin-top:4px;",
              "FINAL"))
    } else {
      div(class = "text-center px-4",
          div(style = "font-size:1.6rem; font-weight:300; color:#aaa;", "vs"))
    }

    report_row <- if (is_past) {
      div(class = "d-flex justify-content-end mt-3 pt-2",
          style = "border-top:1px solid #f0f0f0;",
          actionButton(paste0("game_rpt_", game$id), "Game Report",
                       icon = icon("file-export"),
                       class = "btn-outline-primary btn-sm"))
    } else NULL

    div(class = "card mb-3 shadow-sm",
        style = paste0("border-left:5px solid ", border_col, ";"),
        div(class = "card-body py-3 px-4",
            div(class = "d-flex justify-content-between align-items-center mb-3",
                div(class = "d-flex align-items-center gap-2",
                    tags$span(date_str, style = "font-weight:600; font-size:0.93rem;"),
                    loc_badge),
                result_badge),
            div(class = "d-flex align-items-center justify-content-center",
                style = "gap:2rem;",
                vt_div, center_div, opp_div),
            report_row
        )
    )
  }

  # -- Render all schedule cards --
  output$schedule_cards <- renderUI({
    sched <- schedule_data()
    if (nrow(sched) == 0) {
      return(div(class = "alert alert-info",
                 icon("circle-info"),
                 " No games found. Add games via the Setup tab or Supabase."))
    }

    today <- Sys.Date()
    upcoming <- sched[as.Date(sched$game_date) >  today, ]
    past     <- sched[as.Date(sched$game_date) <= today, ]

    # Build sections
    upcoming_section <- if (nrow(upcoming) > 0) {
      tagList(
        h5("Upcoming", class = "text-muted fw-bold mt-2 mb-3"),
        lapply(seq_len(nrow(upcoming)),
               function(i) make_game_card(upcoming[i, ], is_past = FALSE))
      )
    } else NULL

    past_section <- if (nrow(past) > 0) {
      tagList(
        h5("Results", class = "text-muted fw-bold mt-3 mb-3"),
        lapply(rev(seq_len(nrow(past))),   # most recent first
               function(i) make_game_card(past[i, ], is_past = TRUE))
      )
    } else NULL

    tagList(upcoming_section, past_section)
  })

  # -- Create an observer for each game report button (dynamic) --
  observe({
    sched <- schedule_data()
    req(nrow(sched) > 0)
    past_ids <- sched$id[as.Date(sched$game_date) <= Sys.Date()]

    lapply(past_ids, function(gid) {
      local({
        local_gid <- gid
        btn_id    <- paste0("game_rpt_", local_gid)
        observeEvent(input[[btn_id]], {
          row   <- schedule_data()[schedule_data()$id == local_gid, ]
          label <- if (nrow(row) > 0)
            paste0(row$game_date, " vs ", row$opponent) else "Game"
          sched_report_game_id(local_gid)
          sched_report_label(label)
          sched_report_trigger(isolate(sched_report_trigger()) + 1L)
        }, ignoreInit = TRUE)
      })
    })
  })

  # Full season report button
  observeEvent(input$btn_season_report, {
    sched_report_game_id(NULL)
    sched_report_label("Full Season")
    sched_report_trigger(isolate(sched_report_trigger()) + 1L)
  })

  # -- Run quarto when trigger fires --
  observeEvent(sched_report_trigger(), {
    req(sched_report_trigger() > 0)

    if (!requireNamespace("quarto", quietly = TRUE)) {
      notify_err("'quarto' package not installed. Run: install.packages('quarto')")
      return()
    }

    sched_report_path(NULL)
    output$sched_report_status <- renderUI({
      div(class = "alert alert-info mb-3",
          icon("spinner", class = "fa-spin"),
          tags$b(paste0(" Generating: ", sched_report_label(), " ...")),
          " This takes 20\u201340 seconds.")
    })

    withProgress(message = paste("Rendering:", sched_report_label()), value = 0.3, {
      tryCatch({
        params <- list(
          game_id    = sched_report_game_id(),
          game_label = sched_report_label()
        )
        qmd_path <- normalizePath(
          file.path(getwd(), "Data_Analysis", "vt_hockey_analysis.qmd"),
          mustWork = FALSE
        )
        out_name <- paste0("VT_Report_",
                           gsub("[^A-Za-z0-9]", "_", sched_report_label()),
                           ".html")
        setProgress(0.5, message = "Running analysis...")
        quarto::quarto_render(
          input          = qmd_path,
          output_format  = "html",
          output_file    = out_name,
          execute_params = params
        )
        out_path <- file.path(dirname(qmd_path), out_name)
        sched_report_path(out_path)
        setProgress(1.0)
        output$sched_report_status <- renderUI({
          div(class = "alert alert-success mb-3",
              icon("circle-check"),
              tags$b(paste0(" ", sched_report_label(), " report ready!")),
              " Click Download to save it.",
              br(), br(),
              downloadButton("btn_sched_download", "Download HTML",
                             icon = icon("download"), class = "btn-success"))
        })
      }, error = function(e) {
        output$sched_report_status <- renderUI({
          div(class = "alert alert-danger mb-3",
              icon("circle-xmark"), " Error: ", conditionMessage(e))
        })
      })
    })
  })

  output$btn_sched_download <- downloadHandler(
    filename = function() basename(sched_report_path()),
    content  = function(file) file.copy(sched_report_path(), file)
  )

}

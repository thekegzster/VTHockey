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
    to_log      = list(),
    shot_log    = list(),
    goal_log    = list(),

    # Most recent record id for each table (for undo)
    last_faceoff_id = NULL,
    last_to_id      = NULL,
    last_shot_id    = NULL,
    last_goal_id    = NULL,

    # On Ice tab: current line (jersey numbers as strings) + change log
    current_jerseys = character(0),
    line_log        = list()
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

    # --- Reset all per-game state so old data never bleeds into a new game ---
    rv$faceoff_log      <- list()
    rv$to_log           <- list()
    rv$shot_log         <- list()
    rv$goal_log         <- list()
    rv$last_faceoff_id  <- NULL
    rv$last_to_id       <- NULL
    rv$last_shot_id     <- NULL
    rv$last_goal_id     <- NULL
    rv$current_jerseys  <- character(0)
    rv$line_log         <- list()
    rv$pending_shot_x   <- NULL
    rv$pending_shot_y   <- NULL

    # Clear summary tables immediately so stale data isn't shown during fetch
    summary_data$faceoffs  <- data.frame()
    summary_data$turnovers <- data.frame()
    summary_data$shots     <- data.frame()
    summary_data$goals     <- data.frame()
    summary_data$on_ice    <- data.frame()

    # Load player roster from Supabase
    players <- sb_load_players()
    rv$players <- players

    # Populate all player dropdowns across tabs
    updateSelectizeInput(session, "fo_player",      choices = players, server = TRUE)
    updateSelectizeInput(session, "to_player",      choices = players, server = TRUE)
    updateSelectizeInput(session, "shot_player",    choices = players, server = TRUE)
    updateSelectizeInput(session, "goal_scorer",    choices = players, server = TRUE)
    updateSelectizeInput(session, "goal_assist1",
                         choices = c("(none)" = "", players), server = TRUE)
    updateSelectizeInput(session, "goal_assist2",
                         choices = c("(none)" = "", players), server = TRUE)
    updateSelectizeInput(session, "on_ice_players", choices = players, server = TRUE)

    # Refresh summary for the newly selected game
    tryCatch(do_refresh_summary(silent = TRUE), error = function(e) NULL)

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

  output$session_required_faceoffs  <- session_required_ui("faceoffs")
  output$session_required_turnovers <- session_required_ui("turnovers")
  output$session_required_shots    <- session_required_ui("shots")
  output$session_required_goals    <- session_required_ui("goals")

  # ----------------------------------------------------------
  # ON ICE TAB
  # ----------------------------------------------------------

  # Helper: look up player name from jersey number string
  jersey_to_name <- function(j) {
    if (is.null(j) || nchar(trimws(j)) == 0 || is.null(rv$players)) return("")
    m <- names(rv$players)[grepl(paste0("^#", trimws(j), "\\b"), names(rv$players))]
    if (length(m) == 1) m[1] else ""
  }

  # Helper: look up player_id from jersey number string
  jersey_to_id <- function(j) {
    if (is.null(j) || nchar(trimws(j)) == 0 || is.null(rv$players)) return(NULL)
    m <- rv$players[grepl(paste0("^#", trimws(j), "\\b"), names(rv$players))]
    if (length(m) == 1) as.integer(m[1]) else NULL
  }

  # Render player name label under each jersey box
  lapply(1:5, function(i) {
    local({
      li <- i
      output[[paste0("ice_lbl_", li)]] <- renderUI({
        val <- input[[paste0("ice_", li)]]
        nm  <- jersey_to_name(val)
        if (nchar(nm) == 0) {
          div(style = "height:18px; font-size:0.75rem; color:#ccc;", "\u2013")
        } else {
          last <- gsub("^#\\d+\\s+", "", nm)
          div(style = paste0("font-size:0.75rem; font-weight:700;",
                             " color:", VT_MAROON, "; margin-top:2px;"),
              last)
        }
      })
    })
  })

  # Set Line button — store jersey numbers in rv$current_jerseys
  observeEvent(input$btn_set_line, {
    req(rv$setup_done)
    jerseys <- vapply(1:5, function(i) {
      trimws(as.character(input[[paste0("ice_", i)]] %||% ""))
    }, character(1))
    jerseys <- jerseys[nchar(jerseys) > 0]

    if (length(jerseys) == 0) {
      notify_err("Enter at least one jersey number."); return()
    }

    rv$current_jerseys <- jerseys

    labels <- vapply(jerseys, function(j) {
      nm <- jersey_to_name(j)
      if (nchar(nm) > 0) nm else paste0("#", j, " (?)")
    }, character(1))

    entry <- paste0(format(Sys.time(), "%H:%M"), "  \u2192  ",
                    paste(labels, collapse = "  |  "))
    rv$line_log <- c(list(entry), rv$line_log)[1:min(15, length(rv$line_log) + 1)]
    notify_ok(paste0("Line set: ", paste0("#", jerseys, collapse = " \u2013 ")))
  })

  # Current lineup banner
  output$on_ice_banner <- renderUI({
    jerseys <- rv$current_jerseys
    if (length(jerseys) == 0) {
      return(div(class = "alert alert-warning mb-0",
                 icon("triangle-exclamation"),
                 " No line set yet \u2014 enter jersey numbers below and hit Set Line."))
    }
    circles <- lapply(jerseys, function(j) {
      nm  <- jersey_to_name(j)
      lbl <- if (nchar(nm) > 0) gsub("^#\\d+\\s+", "", nm) else "?"
      div(class = "text-center",
        div(style = paste0("width:58px; height:58px; border-radius:50%;",
                           " background:white; color:", VT_MAROON, ";",
                           " display:flex; align-items:center; justify-content:center;",
                           " font-weight:800; font-size:1.25rem;",
                           " border:2px solid rgba(255,255,255,0.4);"),
            paste0("#", j)),
        div(style = "font-size:0.72rem; font-weight:700; color:white; margin-top:3px;",
            lbl)
      )
    })
    div(
      style = paste0("background:", VT_MAROON, "; border-radius:10px;",
                     " padding:16px 20px; color:white;"),
      div(class = "d-flex justify-content-between align-items-center mb-2",
          tags$span(style = "font-weight:700; font-size:0.85rem; letter-spacing:1px;",
                    tagList(icon("users"), " CURRENT LINE ON ICE")),
          tags$span(style = "font-size:0.78rem; opacity:0.8;",
                    paste0(length(jerseys), " players"))
      ),
      div(class = "d-flex gap-3 justify-content-center", circles)
    )
  })

  # Line change log
  output$line_change_log <- renderUI({
    if (length(rv$line_log) == 0)
      return(p(class = "text-muted", "No line changes recorded yet."))
    tags$ul(class = "list-unstyled mb-0",
      lapply(rv$line_log, function(e) {
        tags$li(style = "font-size:0.83rem; padding:3px 0;",
                icon("hockey-puck"), " ", e)
      })
    )
  })

  # Assist fields shown in the Shots form when result == "goal" AND team == "VT"
  output$shot_goal_assist_fields <- renderUI({
    players_choices <- c("(none)" = "", if (!is.null(rv$players)) rv$players else NULL)
    tagList(
      selectizeInput("shot_goal_assist1", "Primary assist",
                     choices = players_choices, width = "100%",
                     options = list(placeholder = "(none)")),
      selectizeInput("shot_goal_assist2", "Secondary assist",
                     choices = players_choices, width = "100%",
                     options = list(placeholder = "(none)"))
    )
  })

  # session_required panel for on ice tab
  output$session_required_on_ice <- session_required_ui("on_ice")

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
      tryCatch(load_analytics(), error = function(e) NULL)
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
  # SUBMIT TURNOVER
  # ----------------------------------------------------------
  observeEvent(input$btn_submit_to, {
    req(rv$setup_done)
    if (is.null(input$to_player) || input$to_player == "") {
      notify_err("Please select a player."); return()
    }

    disable("btn_submit_to")
    on.exit(enable("btn_submit_to"))

    result <- sb_insert("turnovers", list(
      game_id        = rv$game_id,
      period         = input$to_period,
      game_time      = trimws(input$to_time),
      player_id      = as.integer(input$to_player),
      type           = input$to_type,
      zone           = input$to_zone,
      collector_name = rv$collector_name
    ))

    if (result$success) {
      rv$last_to_id <- result$id
      player_label  <- names(rv$players[rv$players == input$to_player])
      type_label    <- if (input$to_type == "takeaway") "TAKEAWAY" else "GIVEAWAY"
      entry <- paste0(input$to_period, "P  ", input$to_time, "  ",
                      player_label, "  \u2014 ", type_label,
                      "  (", input$to_zone, ")")
      rv$to_log <- c(list(entry), rv$to_log)[1:min(5, length(rv$to_log) + 1)]
      notify_ok(if (input$to_type == "takeaway") "Takeaway recorded!" else "Giveaway recorded.")
      tryCatch(load_analytics(), error = function(e) NULL)
    } else {
      notify_err(result$error)
    }
  })

  observeEvent(input$btn_undo_to, {
    req(!is.null(rv$last_to_id))
    res <- sb_delete("turnovers", rv$last_to_id)
    if (res$success) {
      rv$last_to_id <- NULL
      rv$to_log     <- rv$to_log[-1]
      notify_ok("Last turnover entry deleted.")
    } else {
      notify_err("Could not undo — record may be too old.")
    }
  })

  output$to_log <- renderUI({
    if (length(rv$to_log) == 0) return(p("No entries yet."))
    tags$ol(lapply(rv$to_log, tags$li))
  })

  # ----------------------------------------------------------
  # SUBMIT SHOT
  # ----------------------------------------------------------
  observeEvent(input$btn_submit_shot, {
    req(rv$setup_done)

    is_vt <- !is.null(input$shot_team) && input$shot_team == "VT"

    # Only require a player selection for VT shots
    if (is_vt && (is.null(input$shot_player) || input$shot_player == "")) {
      notify_err("Please select a VT player."); return()
    }

    disable("btn_submit_shot")
    on.exit(enable("btn_submit_shot"))

    shot_data <- list(
      game_id        = rv$game_id,
      period         = input$shot_period,
      game_time      = trimws(input$shot_time),
      player_id      = if (is_vt) as.integer(input$shot_player) else NULL,
      shot_type      = input$shot_type,
      result         = input$shot_result,
      team           = input$shot_team,
      x_coord        = rv$pending_shot_x,
      y_coord        = rv$pending_shot_y,
      collector_name = rv$collector_name
    )
    # Remove NULLs so Supabase stores NULL in DB
    shot_data <- Filter(Negate(is.null), shot_data)

    result <- sb_insert("shots", shot_data)

    if (result$success) {
      shot_id         <- result$id
      rv$last_shot_id <- shot_id

      # Auto-log current on-ice line for this shot
      log_on_ice_for_shot(shot_id)

      # If it's a goal — auto-create goal record
      is_goal <- !is.null(input$shot_result) && input$shot_result == "goal"
      if (is_goal) log_goal_from_shot(shot_id, is_vt)

      # Build log entry
      who <- if (is_vt)
        names(rv$players[rv$players == input$shot_player])
      else
        "Opponent"
      loc_str <- if (!is.null(rv$pending_shot_x))
        paste0(" @ (", rv$pending_shot_x, ",", rv$pending_shot_y, ")") else ""
      entry <- paste0(input$shot_period, "P  ", input$shot_time, "  ",
                      who, "  \u2014 ", input$shot_result, loc_str)
      rv$shot_log <- c(list(entry), rv$shot_log)[1:min(5, length(rv$shot_log) + 1)]
      rv$pending_shot_x <- NULL
      rv$pending_shot_y <- NULL

      if (is_goal) {
        notify_ok(if (is_vt) "\U0001f6a8 GOAL! Hokies score!" else "Opponent goal recorded.")
      } else {
        notify_ok("Shot recorded!")
      }
      tryCatch(load_analytics(), error = function(e) NULL)
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
  # AUTO-LOG GOAL from shot (called inside btn_submit_shot)
  # ----------------------------------------------------------
  log_goal_from_shot <- function(shot_id, is_vt) {
    goal_data <- list(
      game_id        = rv$game_id,
      shot_id        = shot_id,
      period         = input$shot_period,
      game_time      = trimws(input$shot_time),
      situation      = input$shot_goal_situation %||% "even",
      empty_net      = isTRUE(input$shot_goal_empty_net),
      team           = input$shot_team,
      collector_name = rv$collector_name
    )
    if (is_vt) {
      if (!is.null(input$shot_player) && input$shot_player != "")
        goal_data$scorer_id <- as.integer(input$shot_player)
      if (!is.null(input$shot_goal_assist1) && nchar(input$shot_goal_assist1) > 0)
        goal_data$assist1_id <- as.integer(input$shot_goal_assist1)
      if (!is.null(input$shot_goal_assist2) && nchar(input$shot_goal_assist2) > 0)
        goal_data$assist2_id <- as.integer(input$shot_goal_assist2)
    }
    goal_data <- Filter(Negate(is.null), goal_data)
    gr <- sb_insert("goals", goal_data)
    if (gr$success) rv$last_goal_id <- gr$id
  }

  # ----------------------------------------------------------
  # AUTO-LOG ON-ICE PLAYERS from current line (called per shot)
  # ----------------------------------------------------------
  log_on_ice_for_shot <- function(shot_id) {
    jerseys <- rv$current_jerseys
    if (length(jerseys) == 0) return()
    for (j in jerseys) {
      pid <- jersey_to_id(j)
      if (!is.null(pid)) {
        sb_insert("on_ice", Filter(Negate(is.null), list(
          game_id        = rv$game_id,
          shot_id        = shot_id,
          period         = input$shot_period,
          game_time      = trimws(input$shot_time),
          player_id      = pid,
          team           = "VT",
          collector_name = rv$collector_name
        )))
      }
    }
  }

  # ----------------------------------------------------------
  # SUMMARY TAB
  # ----------------------------------------------------------
  summary_data <- reactiveValues(
    faceoffs  = data.frame(),
    turnovers = data.frame(),
    shots     = data.frame(),
    goals     = data.frame(),
    on_ice    = data.frame()
  )

  # Shared fetch function used by auto-refresh, manual button, and on-setup trigger
  do_refresh_summary <- function(silent = FALSE) {
    req(rv$game_id)
    gid <- paste0("eq.", rv$game_id)
    summary_data$faceoffs  <- sb_select("faceoffs",  filters = c("game_id" = gid))
    summary_data$turnovers <- sb_select("turnovers", filters = c("game_id" = gid))
    summary_data$shots     <- sb_select("shots",     filters = c("game_id" = gid))
    summary_data$goals    <- sb_select("goals",    filters = c("game_id" = gid))
    summary_data$on_ice   <- sb_select("on_ice",   filters = c("game_id" = gid))
    if (!silent) notify_ok("Summary data refreshed!")
  }

  # Auto-refresh every 30 seconds when a game is active
  observe({
    invalidateLater(30000)
    if (!is.null(rv$game_id)) {
      tryCatch(do_refresh_summary(silent = TRUE), error = function(e) NULL)
    }
  })

  # Load once immediately when session setup completes
  observeEvent(rv$setup_done, {
    req(rv$setup_done)
    tryCatch(do_refresh_summary(silent = TRUE), error = function(e) NULL)
  })

  # Manual refresh button still works
  observeEvent(input$btn_refresh_summary, {
    tryCatch(do_refresh_summary(silent = FALSE), error = function(e)
      notify_err("Could not refresh summary."))
  })

  output$summary_value_boxes <- renderUI({
    n_shots    <- nrow(summary_data$shots)
    n_goals    <- if (nrow(summary_data$goals) > 0 && "team" %in% names(summary_data$goals))
      sum(summary_data$goals$team == "VT", na.rm = TRUE) else 0
    n_takeaways <- if (nrow(summary_data$turnovers) > 0 && "type" %in% names(summary_data$turnovers))
      sum(summary_data$turnovers$type == "takeaway", na.rm = TRUE) else 0
    n_giveaways <- if (nrow(summary_data$turnovers) > 0 && "type" %in% names(summary_data$turnovers))
      sum(summary_data$turnovers$type == "giveaway", na.rm = TRUE) else 0
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
      column(2, stat_box("Shots",        n_shots,                          VT_MAROON)),
      column(2, stat_box("VT Goals",     n_goals,                          "#c0392b")),
      column(2, stat_box("Takeaways",    n_takeaways,                      "#198754")),
      column(2, stat_box("Giveaways",    n_giveaways,                      VT_ORANGE)),
      column(2, stat_box("Faceoff Wins", paste0(n_fo_wins, " / ", n_faceoffs), "#2471a3"))
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
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0)
      return(p("No goals recorded yet."))
    needed <- c("team", "period", "game_time")
    if (!all(needed %in% names(df)))
      return(p("No goals recorded yet."))
    items <- lapply(seq_len(nrow(df)), function(i) {
      row <- df[i, ]
      scorer <- if ("scorer_name" %in% names(df) && !is.na(row$scorer_name))
        paste0(" (", row$scorer_name, ")") else ""
      tags$li(paste0(row$team, " goal — P", row$period, " ", row$game_time, scorer))
    })
    do.call(tags$ul, items)
  })

  make_dt <- function(df) {
    if (nrow(df) == 0) return(datatable(data.frame(Message = "No data yet.")))
    datatable(df,
              options = list(pageLength = 15, scrollX = TRUE),
              rownames = FALSE,
              class    = "table-striped table-hover")
  }

  output$table_faceoffs  <- renderDT({ make_dt(summary_data$faceoffs) })
  output$table_turnovers <- renderDT({ make_dt(summary_data$turnovers) })
  output$table_shots     <- renderDT({ make_dt(summary_data$shots) })
  output$table_goals    <- renderDT({ make_dt(summary_data$goals) })
  output$table_on_ice   <- renderDT({ make_dt(summary_data$on_ice) })

  # ============================================================
  # SEASON ANALYTICS CACHE
  # Fetched once on load, then refreshed after every submission.
  # All player/team outputs read from this cache — zero extra HTTP calls.
  # ============================================================

  analytics_data <- reactiveVal(list(
    players   = data.frame(),
    shots     = data.frame(),
    goals     = data.frame(),
    faceoffs  = data.frame(),
    turnovers = data.frame(),
    games     = data.frame(),
    n_games   = 1L
  ))

  load_analytics <- function() {
    tryCatch({
      players  <- sb_select("players",
                           select  = "id,jersey_num,full_name,position,active",
                           filters = c("active" = "eq.true"),
                           order   = "jersey_num.asc")
      shots     <- sb_select("shots")
      goals     <- sb_select("goals")
      faceoffs  <- sb_select("faceoffs")
      turnovers <- sb_select("turnovers")
      games     <- sb_select("games", order = "game_date.asc")
      analytics_data(list(
        players   = players,
        shots     = shots,
        goals     = goals,
        faceoffs  = faceoffs,
        turnovers = turnovers,
        games     = games,
        n_games   = max(nrow(games), 1L)
      ))
    }, error = function(e) NULL)
  }

  # Load immediately on app start (no session required)
  observe({ tryCatch(load_analytics(), error = function(e) NULL) })

  # ============================================================
  # PLAYER ANALYSIS TAB
  # ============================================================

  # Populate player dropdown from cache — no server=TRUE for fast response
  observe({
    d <- analytics_data()
    req(nrow(d$players) > 0)
    choices <- setNames(
      as.character(d$players$id),
      paste0("#", d$players$jersey_num, "  ", d$players$full_name,
             " (", d$players$position, ")")
    )
    choices <- choices[!grepl("\\(G\\)", names(choices))]
    updateSelectizeInput(session, "player_select", choices = choices)
  })

  # Soft status message (non-blocking)
  output$player_session_warn <- renderUI({
    if (nrow(analytics_data()$players) == 0)
      div(class = "alert alert-info",
          icon("circle-info"),
          " Loading season data\u2026 stats will appear in a moment.")
    else NULL
  })

  # Compute full team stats — pure computation from cache, no Supabase calls
  team_stats <- reactive({
    d <- analytics_data()
    req(nrow(d$players) > 0)
    compute_player_stats(
      players   = d$players,
      shots     = d$shots,
      goals     = d$goals,
      faceoffs  = d$faceoffs,
      turnovers = d$turnovers,
      n_games   = d$n_games
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
    req(nrow(selected_player_stats()) > 0, nrow(team_stats()) >= 1)
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
      pct_bar("Takeaways / Game",   paste0(p$takeaways_per_game),
              pct("takeaways_per_game")),
      pct_bar("Giveaways / Game \u2193", paste0(p$giveaways_per_game),
              100L - pct("giveaways_per_game")),
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
      metric_row("Takeaways", p$takeaways, "Puck recoveries from opponents"),
      metric_row("Giveaways", p$giveaways, "Puck losses to opponents"),
      metric_row("TA/GA Ratio",
                 if (!is.na(p$ta_ga_ratio)) p$ta_ga_ratio else paste0(p$takeaways, "/0"),
                 "Takeaways \u00f7 Giveaways — higher is better"),
      if (p$fo_total > 0)
        metric_row("Faceoff W\u2212L", paste0(p$fo_wins, "\u2212", p$fo_total - p$fo_wins),
                   paste0(p$fo_pct, "% win rate"))
    )
  })

  # ---- Personal shot map ----
  output$player_shot_map <- renderPlot({
    req(input$player_select)
    d <- analytics_data()

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
    d        <- analytics_data()
    pid      <- as.integer(input$player_select)
    games_df <- d$games

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

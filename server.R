# ============================================================
# server.R  -  All reactive logic and data submission
# ============================================================

options(shiny.fullstacktrace = TRUE)

server <- function(input, output, session) {
  
  # Detect mode from the URL query string (?mode=viewer), same logic ui()
  # already used to decide which tabs to build.
  app_mode <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query$mode) && query$mode == "viewer") "viewer" else "collector"
  })

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
    goalie_jersey   = NULL,
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
    rv$goalie_jersey    <- NULL
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
    updateSelectizeInput(session, "shot_blocked_by", choices = players, server = TRUE)

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
  
  # Render goalie name label
  output$ice_goalie_lbl <- renderUI({
    val <- input$ice_goalie
    nm  <- jersey_to_name(val)
    
    if (nchar(nm) == 0) {
      div(style = "height:18px; font-size:0.75rem; color:#ccc;", "–")
    } else {
      last <- gsub("^#\\d+\\s+", "", nm)
      div(style = paste0("font-size:0.75rem; font-weight:700;",
                         " color:", VT_MAROON, "; margin-top:2px;"),
          last)
    }
  })

  observeEvent(input$btn_set_line, {
    req(rv$setup_done)
    
    goalie <- trimws(as.character(input$ice_goalie %||% ""))
    
    jerseys <- vapply(1:5, function(i) {
      trimws(as.character(input[[paste0("ice_", i)]] %||% ""))
    }, character(1))
    jerseys <- jerseys[nchar(jerseys) > 0]
    
    if (length(jerseys) == 0 && nchar(goalie) == 0) {
      notify_err("Enter at least one player or goalie.")
      return()
    }
    
    rv$current_jerseys <- jerseys
    rv$goalie_jersey   <- goalie
    
    # Push the line to Supabase so every session watching this game sees it,
    # not just this browser tab.
    padded <- c(jerseys, rep("", 5 - length(jerseys)))[1:5]
    line_row <- list(
      game_id       = rv$game_id,
      goalie_jersey = if (nchar(goalie) > 0) goalie else NULL,
      jersey_1      = if (nchar(padded[1]) > 0) padded[1] else NULL,
      jersey_2      = if (nchar(padded[2]) > 0) padded[2] else NULL,
      jersey_3      = if (nchar(padded[3]) > 0) padded[3] else NULL,
      jersey_4      = if (nchar(padded[4]) > 0) padded[4] else NULL,
      jersey_5      = if (nchar(padded[5]) > 0) padded[5] else NULL,
      updated_by    = rv$collector_name
    )
    line_row <- Filter(Negate(is.null), line_row)
    sb_upsert("current_line", line_row, conflict_col = "game_id")
    
    labels <- vapply(jerseys, function(j) {
      nm <- jersey_to_name(j)
      if (nchar(nm) > 0) nm else paste0("#", j, " (?)")
    }, character(1))
    
    entry <- paste0(format(Sys.time(), "%H:%M"), "  \u2192  ",
                    paste(labels, collapse = "  |  "))
    rv$line_log <- c(list(entry), rv$line_log)[1:min(15, length(rv$line_log) + 1)]
    notify_ok(paste0("Line set: ", paste0("#", jerseys, collapse = " \u2013 ")))
  })
  
  # ----------------------------------------------------------
  # POLL FOR LINE CHANGES made by anyone else on this game
  # ----------------------------------------------------------
  observe({
    invalidateLater(2000)  # check every 4 seconds
    req(rv$game_id)
    
    row <- tryCatch(
      sb_select("current_line", filters = c("game_id" = paste0("eq.", rv$game_id))),
      error = function(e) NULL
    )
    if (is.null(row) || nrow(row) == 0) return()
    
    row <- row[1, ]
    new_goalie  <- if (!is.na(row$goalie_jersey)) as.character(row$goalie_jersey) else ""
    new_jerseys <- c(row$jersey_1, row$jersey_2, row$jersey_3, row$jersey_4, row$jersey_5)
    new_jerseys <- as.character(new_jerseys[!is.na(new_jerseys)])
    
    # Only update (and notify) if something actually changed vs what
    # this session currently has, to avoid spamming notifications.
    changed <- !identical(sort(new_jerseys), sort(rv$current_jerseys)) ||
      !identical(new_goalie, rv$goalie_jersey %||% "")
    
    if (changed) {
      rv$current_jerseys <- new_jerseys
      rv$goalie_jersey   <- new_goalie
    }
  })

  # Current lineup banner
  output$on_ice_banner <- renderUI({
    jerseys <- c(rv$goalie_jersey, rv$current_jerseys)
    jerseys <- jerseys[nchar(jerseys) > 0]
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
    req(input$shot_period)
    
    shots_data <- NULL
    if (isTRUE(input$show_game_shots) && !is.null(rv$game_id)) {
      shots_data <- tryCatch(
        sb_select("shots",
                  select  = "x_coord,y_coord,result,period,team",
                  filters = c("game_id" = paste0("eq.", rv$game_id))),
        error = function(e) NULL
      )
      if (!is.null(shots_data) && nrow(shots_data) > 0 && !is.null(input$shot_period)) {
        shots_data <- shots_data[shots_data$period == input$shot_period, ]
      }
    }
    draw_rink(shots_df = shots_data,
              pending_x = rv$pending_shot_x,
              pending_y = rv$pending_shot_y,
              attack_right = vt_attacks_right(input$shot_period))
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
      refresh_analytics_partial(c("faceoffs"))
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
      refresh_analytics_partial(c("turnovers"))
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

    is_blocked_opponent_shot <- !is_vt && !is.null(input$shot_result) &&
      input$shot_result == "blocked"
    
    shot_data <- list(
      game_id        = rv$game_id,
      period         = input$shot_period,
      game_time      = trimws(input$shot_time),
      player_id      = if (is_vt) as.integer(input$shot_player) else NULL,
      goalie_id      = if (!is.null(rv$goalie_jersey)) {
        jersey_to_id(rv$goalie_jersey)
      } else {
        NULL
      },
      shot_type            = input$shot_type,
      result               = input$shot_result,
      team                 = input$shot_team,
      x_coord              = rv$pending_shot_x,
      y_coord              = rv$pending_shot_y,
      blocked_by_player_id = if (is_blocked_opponent_shot &&
                                 !is.null(input$shot_blocked_by) &&
                                 input$shot_blocked_by != "") {
        as.integer(input$shot_blocked_by)
      } else {
        NULL
      },
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
      blocker_str <- if (is_blocked_opponent_shot && !is.null(input$shot_blocked_by) &&
                         input$shot_blocked_by != "") {
        blocker_name <- names(rv$players[rv$players == input$shot_blocked_by])
        paste0("  (blocked by ", blocker_name, ")")
      } else ""
      entry <- paste0(input$shot_period, "P  ", input$shot_time, "  ",
                      who, "  \u2014 ", input$shot_result, loc_str, blocker_str)
      rv$shot_log <- c(list(entry), rv$shot_log)[1:min(5, length(rv$shot_log) + 1)]
      rv$pending_shot_x <- NULL
      rv$pending_shot_y <- NULL

      if (is_goal) {
        notify_ok(if (is_vt) "\U0001f6a8 GOAL! Hokies score!" else "Opponent goal recorded.")
      } else {
        notify_ok("Shot recorded!")
      }
      refresh_analytics_partial(c("shots", "goals", "on_ice"))
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
    jerseys <- c(rv$goalie_jersey, rv$current_jerseys)
    jerseys <- jerseys[nchar(jerseys) > 0]
    if (length(jerseys) == 0) return()
    
    rows <- list()
    for (j in jerseys) {
      pid <- jersey_to_id(j)
      if (!is.null(pid)) {
        rows[[length(rows) + 1]] <- Filter(Negate(is.null), list(
          game_id        = rv$game_id,
          shot_id        = shot_id,
          period         = input$shot_period,
          game_time      = trimws(input$shot_time),
          player_id      = pid,
          team           = "VT",
          collector_name = rv$collector_name
        ))
      }
    }
    if (length(rows) > 0) sb_insert_many("on_ice", rows)
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

  # Guarantee a proper data frame regardless of what sb_select returns
  safe_df <- function(x) {
    if (is.null(x) || !is.data.frame(x)) data.frame() else x
  }
  
  # Filters a summary data frame down to one period, or passes it through
  # unfiltered when "All Periods" is selected. Used by every Summary tab output.
  filter_by_period <- function(df) {
    if (is.null(input$summary_period_filter) || input$summary_period_filter == "all") return(df)
    if (is.null(df) || nrow(df) == 0 || !"period" %in% names(df)) return(df[0, ])
    df[df$period == input$summary_period_filter, ]
  }
  
  # Single reactive every Summary output reads from, so the period filter
  # only needs to be applied once per table, not duplicated in every output.
  summary_filtered <- reactive({
    list(
      faceoffs  = filter_by_period(safe_df(summary_data$faceoffs)),
      turnovers = filter_by_period(safe_df(summary_data$turnovers)),
      shots     = filter_by_period(safe_df(summary_data$shots)),
      goals     = filter_by_period(safe_df(summary_data$goals)),
      on_ice    = filter_by_period(safe_df(summary_data$on_ice))
    )
  })

  # Shared fetch function used by auto-refresh, manual button, and on-setup trigger
  do_refresh_summary <- function(silent = FALSE) {
    req(rv$game_id)
    gid <- paste0("eq.", rv$game_id)
    summary_data$faceoffs  <- safe_df(sb_select("faceoffs",  filters = c("game_id" = gid)))
    summary_data$turnovers <- safe_df(sb_select("turnovers", filters = c("game_id" = gid)))
    summary_data$shots     <- safe_df(sb_select("shots",     filters = c("game_id" = gid)))
    summary_data$goals     <- safe_df(sb_select("goals",     filters = c("game_id" = gid)))
    summary_data$on_ice    <- safe_df(sb_select("on_ice",    filters = c("game_id" = gid)))
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
  
  # ----------------------------------------------------------
  # VIEWER MODE: lightweight game picker shown at top of Summary
  # tab (collector mode instead sets rv$game_id via Setup)
  # ----------------------------------------------------------
  output$viewer_game_picker <- renderUI({
    req(app_mode() == "viewer")
    div(style = "max-width:400px; margin-bottom:1rem;",
        selectInput("viewer_game_select", "Select Game", choices = NULL, width = "100%"))
  })
  
  observe({
    req(app_mode() == "viewer")
    games <- tryCatch(sb_load_games(), error = function(e) c())
    updateSelectInput(session, "viewer_game_select", choices = games)
  })
  
  observeEvent(input$viewer_game_select, {
    req(app_mode() == "viewer", input$viewer_game_select, input$viewer_game_select != "")
    
    games <- tryCatch(sb_load_games(), error = function(e) c())
    rv$game_id    <- as.integer(input$viewer_game_select)
    rv$game_label <- names(games[games == input$viewer_game_select])
    rv$setup_done <- TRUE
    
    summary_data$faceoffs  <- data.frame()
    summary_data$turnovers <- data.frame()
    summary_data$shots     <- data.frame()
    summary_data$goals     <- data.frame()
    summary_data$on_ice    <- data.frame()
    
    tryCatch(do_refresh_summary(silent = TRUE), error = function(e) NULL)
  })

  # ----------------------------------------------------------
  # SCOREBOARD (top of Summary tab)
  # ----------------------------------------------------------
  output$summary_scoreboard <- renderUI({
    req(rv$game_id)
    
    games_df <- analytics_data()$games
    game_row <- if (nrow(games_df) > 0) games_df[games_df$id == rv$game_id, ] else data.frame()
    
    goals_df  <- safe_df(summary_data$goals)
    vt_score  <- if (nrow(goals_df) > 0 && "team" %in% names(goals_df))
      sum(goals_df$team == "VT", na.rm = TRUE) else 0
    opp_score <- if (nrow(goals_df) > 0 && "team" %in% names(goals_df))
      sum(goals_df$team != "VT" & !is.na(goals_df$team), na.rm = TRUE) else 0
    
    opponent_name <- if (nrow(game_row) > 0) game_row$opponent else "Opponent"
    has_logo <- nrow(game_row) > 0 && "opponent_logo_url" %in% names(game_row) &&
      !is.na(game_row$opponent_logo_url) &&
      nchar(trimws(as.character(game_row$opponent_logo_url))) > 0
    initials <- substring(gsub("[^A-Za-z]", "", opponent_name), 1, 3)
    
    vt_logo <- tags$img(src = "vt_logo.png", height = "76px",
                        style = "object-fit:contain; filter:drop-shadow(0 2px 4px rgba(0,0,0,0.3));",
                        onerror = paste0(
                          "this.outerHTML='<div style=\"width:76px;height:76px;border-radius:50%;",
                          "background:white;color:", VT_MAROON, ";display:flex;align-items:center;",
                          "justify-content:center;font-weight:800;font-size:1.4rem;",
                          "box-shadow:0 2px 8px rgba(0,0,0,0.25);\">VT</div>'"))
    
    opp_logo <- if (has_logo) {
      tags$img(src = game_row$opponent_logo_url, height = "76px",
               style = "object-fit:contain; filter:drop-shadow(0 2px 4px rgba(0,0,0,0.3));",
               onerror = paste0(
                 "this.outerHTML='<div style=\"width:76px;height:76px;border-radius:50%;",
                 "background:#2c2c2c;color:white;display:flex;align-items:center;",
                 "justify-content:center;font-weight:800;font-size:1.4rem;",
                 "box-shadow:0 2px 8px rgba(0,0,0,0.25);\">", initials, "</div>'"))
    } else {
      div(style = paste0("width:76px; height:76px; border-radius:50%; background:#2c2c2c;",
                         " color:white; display:flex; align-items:center; justify-content:center;",
                         " font-weight:800; font-size:1.4rem;",
                         " box-shadow:0 2px 8px rgba(0,0,0,0.25);"), initials)
    }
    
    # Highlight whichever side is leading (or neither, if tied)
    vt_leading  <- vt_score > opp_score
    opp_leading <- opp_score > vt_score
    vt_score_col  <- if (vt_leading) "#ffffff" else "rgba(255,255,255,0.55)"
    opp_score_col <- if (opp_leading) "#ffffff" else "rgba(255,255,255,0.55)"
    
    div(
      class = "mb-0",
      style = paste0(
        "background:linear-gradient(135deg, ", VT_MAROON, " 0%, #5c1530 100%);",
        " border-radius:14px; box-shadow:0 8px 24px rgba(134,31,65,0.35);",
        " padding:28px 32px; position:relative; overflow:hidden;"
      ),
      # subtle orange accent stripe across the top
      div(style = paste0("position:absolute; top:0; left:0; right:0; height:5px;",
                         " background:", VT_ORANGE, ";")),
      
      div(class = "d-flex align-items-center justify-content-center", style = "gap:3rem;",
          div(class = "text-center", vt_logo,
              tags$p("VIRGINIA TECH", class = "mt-2 mb-0",
                     style = "color:white; font-weight:800; font-size:0.85rem; letter-spacing:1.5px;")
          ),
          
          div(class = "text-center", style = "min-width:180px;",
              div(class = "d-flex align-items-center justify-content-center",
                  style = "gap:14px;",
                  tags$span(vt_score, style = paste0(
                    "font-size:4rem; font-weight:900; line-height:1;",
                    " color:", vt_score_col, "; text-shadow:0 2px 6px rgba(0,0,0,0.3);")),
                  tags$span("\u2013", style = paste0(
                    "font-size:2.5rem; font-weight:300; color:", VT_ORANGE, ";")),
                  tags$span(opp_score, style = paste0(
                    "font-size:4rem; font-weight:900; line-height:1;",
                    " color:", opp_score_col, "; text-shadow:0 2px 6px rgba(0,0,0,0.3);"))
              )
          ),
          
          div(class = "text-center", opp_logo,
              tags$p(toupper(opponent_name), class = "mt-2 mb-0",
                     style = "color:white; font-weight:800; font-size:0.85rem; letter-spacing:1.5px;")
          )
      )
    )
  })
  
  # ----------------------------------------------------------
  # TEAM STATS (side card, replaces Goal Log)
  # ----------------------------------------------------------
  output$summary_team_stats <- renderUI({
    sf <- summary_filtered()
    shots_df     <- sf$shots
    turnovers_df <- sf$turnovers
    faceoffs_df  <- sf$faceoffs
    
    shots_vt  <- if (nrow(shots_df) > 0 && "team" %in% names(shots_df))
      sum(shots_df$team == "VT", na.rm = TRUE) else 0
    shots_opp <- if (nrow(shots_df) > 0 && "team" %in% names(shots_df))
      sum(shots_df$team != "VT" & !is.na(shots_df$team), na.rm = TRUE) else 0
    
    hd_vt <- 0; hd_opp <- 0
    if (nrow(shots_df) > 0 && all(c("x_coord", "y_coord", "team") %in% names(shots_df))) {
      has_loc  <- !is.na(shots_df$x_coord) & !is.na(shots_df$y_coord)
      vt_rows  <- shots_df[has_loc & shots_df$team == "VT", ]
      opp_rows <- shots_df[has_loc & shots_df$team != "VT" & !is.na(shots_df$team), ]
      
      period_col_vt <- if ("period" %in% names(vt_rows)) vt_rows$period else rep("1", nrow(vt_rows))
      period_col_opp <- if ("period" %in% names(opp_rows)) opp_rows$period else rep("1", nrow(opp_rows))
      
      if (nrow(vt_rows) > 0) {
        # Normalize into the canonical "VT attacks right" frame before
        # classifying — raw coords can sit at either end depending on period.
        vt_canon <- to_canonical_xy(vt_rows$x_coord, vt_rows$y_coord, period_col_vt)
        hd_vt <- sum(classify_danger(vt_canon$x, vt_canon$y) == "High Danger")
      }
      if (nrow(opp_rows) > 0) {
        # Same normalization, then mirror onto VT's net (opponent shoots at VT).
        opp_canon <- to_canonical_xy(opp_rows$x_coord, opp_rows$y_coord, period_col_opp)
        hd_opp <- sum(classify_danger(200 - opp_canon$x, opp_canon$y) == "High Danger")
      }
    }
    
    # Save % — VT's save % comes from opponent shots (VT's goalie faces
    # those); opponent's save % comes from VT shots (their goalie faces
    # those). Only shots that reached the net (save or goal) count as
    # shots on goal — missed/blocked shots don't test the goalie at all.
    vt_save_pct <- NA_real_; opp_save_pct <- NA_real_
    if (nrow(shots_df) > 0 && all(c("team", "result") %in% names(shots_df))) {
      vt_shot_rows  <- shots_df[!is.na(shots_df$team) & shots_df$team == "VT", ]
      opp_shot_rows <- shots_df[!is.na(shots_df$team) & shots_df$team != "VT", ]
      
      opp_on_goal <- sum(opp_shot_rows$result %in% c("save", "goal"), na.rm = TRUE)
      opp_saves   <- sum(opp_shot_rows$result == "save", na.rm = TRUE)
      vt_save_pct <- if (opp_on_goal > 0) round(opp_saves / opp_on_goal * 100, 1) else NA_real_
      
      vt_on_goal   <- sum(vt_shot_rows$result %in% c("save", "goal"), na.rm = TRUE)
      opp_saves_vt <- sum(vt_shot_rows$result == "save", na.rm = TRUE)
      opp_save_pct <- if (vt_on_goal > 0) round(opp_saves_vt / vt_on_goal * 100, 1) else NA_real_
    }
    
    takeaways_vt <- if (nrow(turnovers_df) > 0 && "type" %in% names(turnovers_df))
      sum(turnovers_df$type == "takeaway", na.rm = TRUE) else 0
    giveaways_vt <- if (nrow(turnovers_df) > 0 && "type" %in% names(turnovers_df))
      sum(turnovers_df$type == "giveaway", na.rm = TRUE) else 0
    
    fo_total   <- nrow(faceoffs_df)
    fo_wins_vt <- if (fo_total > 0 && "result" %in% names(faceoffs_df))
      sum(faceoffs_df$result == "win", na.rm = TRUE) else 0
    fo_pct_vt  <- if (fo_total > 0) round(fo_wins_vt / fo_total * 100, 1) else NA_real_
    fo_pct_opp <- if (!is.na(fo_pct_vt)) round(100 - fo_pct_vt, 1) else NA_real_
    
    # One comparison row: VT value | label | opponent value, with a two-color bar
    stat_row <- function(label, vt_val, opp_val, vt_pct = NULL, opp_pct = NULL) {
      if (is.null(vt_pct)) {
        v <- suppressWarnings(as.numeric(vt_val))
        o <- suppressWarnings(as.numeric(opp_val))
        total <- v + o
        vt_pct  <- if (!is.na(total) && total > 0) v / total * 100 else 50
        opp_pct <- 100 - vt_pct
      }
      div(class = "mb-3",
          div(class = "d-flex justify-content-between align-items-center mb-1",
              tags$span(style = paste0("font-weight:700; color:", VT_MAROON, ";"), vt_val),
              tags$span(style = "font-size:0.78rem; color:#888; text-transform:uppercase; letter-spacing:0.5px;", label),
              tags$span(style = "font-weight:700; color:#333;", opp_val)
          ),
          div(class = "d-flex", style = "height:6px; border-radius:4px; overflow:hidden; background:#e9ecef;",
              div(style = paste0("width:", vt_pct, "%; background:", VT_MAROON, ";")),
              div(style = paste0("width:", opp_pct, "%; background:#333;"))
          )
      )
    }
    
    tagList(
      stat_row("Shots", shots_vt, shots_opp),
      stat_row("High Danger Shots", hd_vt, hd_opp),
      stat_row("Takeaways", takeaways_vt, "\u2014", 50, 50),
      stat_row("Giveaways", giveaways_vt, "\u2014", 50, 50),
      if (!is.na(fo_pct_vt))
        stat_row("Faceoff %", paste0(fo_pct_vt, "%"), paste0(fo_pct_opp, "%"), fo_pct_vt, fo_pct_opp)
      else
        stat_row("Faceoff %", "\u2014", "\u2014", 50, 50),
      if (!is.na(vt_save_pct) || !is.na(opp_save_pct))
        stat_row("Save %",
                 if (is.na(vt_save_pct)) "\u2014" else paste0(vt_save_pct, "%"),
                 if (is.na(opp_save_pct)) "\u2014" else paste0(opp_save_pct, "%"),
                 if (is.na(vt_save_pct)) 50 else vt_save_pct,
                 if (is.na(opp_save_pct)) 50 else opp_save_pct)
      else
        stat_row("Save %", "\u2014", "\u2014", 50, 50),
      div(class = "alert alert-light border mt-3 mb-0", style = "font-size:0.76rem;",
          icon("circle-info"),
          " Takeaways/Giveaways are only tracked for VT players currently.")
    )
  })

  output$summary_shot_map <- renderPlot({
    df <- summary_filtered()$shots
    shots_with_loc <- if (nrow(df) > 0 && "x_coord" %in% names(df))
      df[!is.na(df$x_coord), ] else NULL
    
    is_single_period <- !is.null(input$summary_period_filter) &&
      input$summary_period_filter != "all"
    
    if (is_single_period) {
      # One real period selected — show shots exactly as recorded,
      # oriented to that period's actual attack direction.
      draw_rink(shots_df = shots_with_loc,
                attack_right = vt_attacks_right(input$summary_period_filter))
    } else {
      # "All Periods" — normalize every shot onto one consistent side
      # since periods 1/2/3 physically attack opposite ends.
      if (!is.null(shots_with_loc) && nrow(shots_with_loc) > 0) {
        period_col <- if ("period" %in% names(shots_with_loc))
          shots_with_loc$period else rep("1", nrow(shots_with_loc))
        disp <- to_display_xy(shots_with_loc$x_coord, shots_with_loc$y_coord, period_col)
        shots_with_loc$x_coord <- disp$x
        shots_with_loc$y_coord <- disp$y
      }
      draw_rink(shots_df = shots_with_loc, attack_right = FALSE)
    }
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
    df <- safe_df(df)
    if (nrow(df) == 0) return(datatable(data.frame(Message = "No data yet.")))
    datatable(df,
              options = list(pageLength = 15, scrollX = TRUE),
              rownames = FALSE,
              class    = "table-striped table-hover")
  }

  # Looks up "#jersey Full Name" for a player_id; used by the play-by-play log
  player_name_lookup <- function(pid, players_df) {
    if (is.null(pid) || is.na(pid)) return("\u2014")
    nm <- players_df$full_name[players_df$id == pid]
    jn <- players_df$jersey_num[players_df$id == pid]
    if (length(nm) == 1) paste0("#", jn, " ", nm) else paste0("Player ", pid)
  }
  
  # ----------------------------------------------------------
  # PLAY-BY-PLAY: every faceoff/turnover/shot/goal, in entry order
  # ----------------------------------------------------------
  output$table_play_by_play <- renderDT({
    players_df <- analytics_data()$players
    sf <- summary_filtered()
    fo <- sf$faceoffs
    to <- sf$turnovers
    sh <- sf$shots
    gl <- sf$goals
    
    rows <- list()
    
    if (nrow(fo) > 0) {
      rows[[length(rows) + 1]] <- data.frame(
        created_at  = fo$created_at,
        period      = fo$period,
        game_time   = fo$game_time,
        event       = "Faceoff",
        description = paste0(sapply(fo$player_id, player_name_lookup, players_df),
                             " \u2014 ", toupper(fo$result), " (", fo$zone, " zone)"),
        stringsAsFactors = FALSE
      )
    }
    
    if (nrow(to) > 0) {
      rows[[length(rows) + 1]] <- data.frame(
        created_at  = to$created_at,
        period      = to$period,
        game_time   = to$game_time,
        event       = "Turnover",
        description = paste0(sapply(to$player_id, player_name_lookup, players_df),
                             " \u2014 ", toupper(to$type), " (", to$zone, " zone)"),
        stringsAsFactors = FALSE
      )
    }
    
    if (nrow(sh) > 0) {
      who <- ifelse(sh$team == "VT",
                    sapply(sh$player_id, player_name_lookup, players_df),
                    "Opponent")
      block_note <- if ("blocked_by_player_id" %in% names(sh)) {
        sapply(sh$blocked_by_player_id, function(pid) {
          if (is.na(pid)) "" else paste0(" (blocked by ", player_name_lookup(pid, players_df), ")")
        })
      } else rep("", nrow(sh))
      rows[[length(rows) + 1]] <- data.frame(
        created_at  = sh$created_at,
        period      = sh$period,
        game_time   = sh$game_time,
        event       = "Shot",
        description = paste0(who, " \u2014 ", sh$shot_type, " shot, ", sh$result,
                             " (", sh$team, ")", block_note),
        stringsAsFactors = FALSE
      )
    }
    
    if (nrow(gl) > 0) {
      scorer  <- ifelse(gl$team == "VT",
                        sapply(gl$scorer_id, player_name_lookup, players_df),
                        "Opponent")
      assists <- mapply(function(a1, a2) {
        parts <- c(
          if (!is.na(a1)) player_name_lookup(a1, players_df) else NA,
          if (!is.na(a2)) player_name_lookup(a2, players_df) else NA
        )
        parts <- parts[!is.na(parts)]
        if (length(parts) == 0) "unassisted" else paste("assist:", paste(parts, collapse = ", "))
      }, gl$assist1_id, gl$assist2_id)
      rows[[length(rows) + 1]] <- data.frame(
        created_at  = gl$created_at,
        period      = gl$period,
        game_time   = gl$game_time,
        event       = "GOAL",
        description = paste0(scorer, " \u2014 ", toupper(gl$team), " goal (", assists, ")"),
        stringsAsFactors = FALSE
      )
    }
    
    if (length(rows) == 0) return(make_dt(data.frame()))
    
    pbp <- dplyr::bind_rows(rows) |>
      dplyr::mutate(created_at = as.POSIXct(created_at)) |>
      dplyr::arrange(created_at) |>
      dplyr::select(Period = period, Time = game_time, Event = event, Description = description)
    
    make_dt(pbp)
  })
  
  # ----------------------------------------------------------
  # PLAYER STATS: this game only, computed the same way as the
  # season Players tab but scoped to summary_data (single game)
  # ----------------------------------------------------------
  output$table_player_stats <- renderDT({
    d <- analytics_data()
    req(nrow(d$players) > 0)
    
    sf <- summary_filtered()
    stats <- compute_player_stats(
      players   = d$players,
      shots     = sf$shots,
      goals     = sf$goals,
      faceoffs  = sf$faceoffs,
      turnovers = sf$turnovers,
      on_ice    = sf$on_ice,
      n_games   = 1
    )
    
    if (nrow(stats) == 0) return(make_dt(data.frame()))
    
    stats |>
      dplyr::transmute(
        Player   = paste0("#", jersey_num, " ", full_name),
        Pos      = position,
        G        = goals,
        A        = a1 + a2,
        Pts      = points,
        Shots    = shots,
        `Shot%`  = shoot_pct,
        xG       = xg_total,
        `+/-`    = plus_minus,
        `xG+/-`  = xg_plus_minus,
        TA       = takeaways,
        GA       = giveaways,
        BLK      = blocks,
        `FO%`    = ifelse(is.na(fo_pct), "\u2014", paste0(fo_pct, "%"))
      ) |>
      dplyr::arrange(dplyr::desc(Pts)) |>
      make_dt()
  })
  
  # ----------------------------------------------------------
  # GOALIE STATS: this game only
  # ----------------------------------------------------------
  output$table_goalie_stats <- renderDT({
    d <- analytics_data()
    
    sf <- summary_filtered()
    goalie_stats <- tryCatch(
      compute_goalie_stats(
        shots  = sf$shots,
        on_ice = sf$on_ice
      ),
      error = function(e) NULL
    )
    
    if (is.null(goalie_stats) || nrow(goalie_stats) == 0) return(make_dt(data.frame()))
    
    # Guard against any expected column being absent (e.g. stale cache)
    required_cols <- c("goalie_id", "shots_against", "saves", "goals_against",
                       "save_pct", "hd_save_pct", "md_save_pct", "ld_save_pct", "gsax")
    missing_cols <- setdiff(required_cols, names(goalie_stats))
    if (length(missing_cols) > 0) {
      showNotification(
        paste("Goalie stats missing columns:", paste(missing_cols, collapse = ", ")),
        type = "error", duration = 8
      )
      return(make_dt(data.frame()))
    }
    
    goalie_stats |>
      dplyr::left_join(
        d$players |> dplyr::select(id, jersey_num, full_name),
        by = c("goalie_id" = "id")
      ) |>
      dplyr::transmute(
        Goalie    = paste0("#", jersey_num, " ", full_name),
        SA        = shots_against,
        SV        = saves,
        GA        = goals_against,
        `SV%`     = ifelse(is.na(save_pct), "\u2014", paste0(save_pct, "%")),
        `HD SV%`  = ifelse(is.na(hd_save_pct), "\u2014", paste0(hd_save_pct, "%")),
        `MD SV%`  = ifelse(is.na(md_save_pct), "\u2014", paste0(md_save_pct, "%")),
        `LD SV%`  = ifelse(is.na(ld_save_pct), "\u2014", paste0(ld_save_pct, "%")),
        GSAx      = ifelse(is.na(gsax), "\u2014", paste0(ifelse(gsax >= 0, "+", ""), gsax))
      ) |>
      make_dt()
  })

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
    on_ice    = data.frame(),
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
      on_ice    <- sb_select("on_ice", select = "player_id,shot_id,game_id")
      games     <- sb_select("games", order = "game_date.asc")
      analytics_data(list(
        players   = players,
        shots     = shots,
        goals     = goals,
        faceoffs  = faceoffs,
        turnovers = turnovers,
        on_ice    = on_ice,
        games     = games,
        n_games   = max(nrow(games), 1L)
      ))
    }, error = function(e) NULL)
  }
  
  # Refreshes only the tables a given submission type could have changed,
  # instead of re-fetching all 7 season tables every time. Much faster
  # than load_analytics() for the common case of "I just submitted one row."
  refresh_analytics_partial <- function(tables) {
    tryCatch({
      current <- analytics_data()
      if ("shots" %in% tables)     current$shots     <- sb_select("shots")
      if ("goals" %in% tables)     current$goals     <- sb_select("goals")
      if ("faceoffs" %in% tables)  current$faceoffs  <- sb_select("faceoffs")
      if ("turnovers" %in% tables) current$turnovers <- sb_select("turnovers")
      if ("on_ice" %in% tables)    current$on_ice    <- sb_select("on_ice", select = "player_id,shot_id,game_id")
      analytics_data(current)
    }, error = function(e) NULL)
  }

  # Load immediately on app start (no session required)
  observe({ tryCatch(load_analytics(), error = function(e) NULL) })

  # ============================================================
  # PLAYER ANALYSIS TAB
  # ============================================================

  # Populate the game filter dropdown ("Full Season" + one entry per game)
  observe({
    d <- analytics_data()
    req(nrow(d$games) > 0)
    game_choices <- c(
      "Full Season" = "season",
      setNames(as.character(d$games$id),
               paste0(d$games$game_date, " vs ", d$games$opponent))
    )
    updateSelectInput(session, "player_game_filter", choices = game_choices)
  })
  
  # Populate player dropdown from cache — no server=TRUE for fast response
  observe({
    d <- analytics_data()
    req(nrow(d$players) > 0)
    
    if (input$player_type == "Goalie") {
      
      players <- d$players |>
        dplyr::filter(position == "G")
      
    } else {
      
      players <- d$players |>
        dplyr::filter(position != "G")
      
    }
    
    choices <- setNames(
      as.character(players$id),
      paste0("#", players$jersey_num,
             "  ", players$full_name)
    )
    
    updateSelectizeInput(
      session,
      "player_select",
      choices = choices,
      server = TRUE
    )
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
  # Filters any event table down to a single game_id, or passes it through
  # unfiltered when "Full Season" is selected. Used by team_stats() below.
  filter_by_game <- function(df) {
    if (is.null(input$player_game_filter) || input$player_game_filter == "season") return(df)
    if (is.null(df) || nrow(df) == 0 || !"game_id" %in% names(df)) return(df[0, ])
    df[df$game_id == as.integer(input$player_game_filter), ]
  }
  
  team_stats <- reactive({
    
    d <- analytics_data()
    req(nrow(d$players) > 0)
    
    shots_f     <- filter_by_game(d$shots)
    goals_f     <- filter_by_game(d$goals)
    faceoffs_f  <- filter_by_game(d$faceoffs)
    turnovers_f <- filter_by_game(d$turnovers)
    on_ice_f    <- filter_by_game(d$on_ice)
    
    single_game <- !is.null(input$player_game_filter) && input$player_game_filter != "season"
    
    if (input$player_type == "Goalie") {
      
      goalie_stats <- compute_goalie_stats(
        shots  = shots_f,
        on_ice = d$on_ice   # unfiltered: games_played should reflect real season count
      )
      
      goalie_stats |>
        dplyr::left_join(
          d$players |>
            dplyr::select(
              id,
              jersey_num,
              full_name,
              position
            ),
          by = c("goalie_id" = "id")
        ) |>
        dplyr::mutate(
          player_id = goalie_id,
          games_played = if (single_game) 1L else games_played
        )
      
    } else {
      
      compute_player_stats(
        players   = d$players,
        shots     = shots_f,
        goals     = goals_f,
        faceoffs  = faceoffs_f,
        turnovers = turnovers_f,
        on_ice    = on_ice_f,
        n_games   = if (single_game) 1L else d$n_games
      )
    }
  })

  # Selected player's row
  selected_player_stats <- reactive({
    
    req(input$player_select)
    req(nrow(team_stats()) > 0)
    
    team_stats() |>
      dplyr::filter(
        player_id == as.integer(input$player_select)
      )
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

  output$player_stat_boxes_dynamic <- renderUI({
    
    req(input$player_select)
    
    if(input$player_type == "Goalie") {
      uiOutput("goalie_stat_boxes")
    } else {
      uiOutput("player_stat_boxes")
    }
    
  })
  
  # Goalie Stat boxes
  output$goalie_stat_boxes <- renderUI({
    
    req(nrow(selected_player_stats()) > 0)
    
    g <- selected_player_stats()
    
    box <- function(val, lbl, color = VT_MAROON) {
      column(3,
             div(class = "card text-center mb-3",
                 style = paste0(
                   "border:none; border-radius:12px;",
                   " background:linear-gradient(160deg, ", color, "12 0%, #ffffff 65%);",
                   " box-shadow:0 3px 10px rgba(0,0,0,0.08);",
                   " border-left:5px solid ", color, ";"
                 ),
                 div(class = "card-body py-3",
                     h2(val,
                        style = paste0(
                          "color:", color,
                          "; font-weight:800; margin:0; font-size:2rem;",
                          " letter-spacing:-0.5px;"
                        )),
                     p(toupper(lbl),
                       class = "mb-0",
                       style = paste0(
                         "font-size:0.72rem; font-weight:700; letter-spacing:0.8px;",
                         " color:#6c757d; margin-top:4px;"
                       ))
                 ))
      )
    }
    
    tagList(
      
      fluidRow(
        box(g$games_played, "Games", VT_MAROON),
        box(g$shots_against, "Shots Against", VT_ORANGE),
        box(if (length(g$gaa) == 0 || is.na(g$gaa)) "N/A" else g$gaa, "GAA", 
            VT_MAROON),
        box(if (length(g$xgaa) == 0 || is.na(g$xgaa)) "N/A" else g$xgaa, "xGAA", VT_ORANGE)
      ),
      
      fluidRow(
        box(paste0(g$save_pct, "%"), "Save %", VT_ORANGE),
        box(paste0(g$hd_save_pct, "%"), "High Danger Save %", VT_MAROON),
        box(paste0(g$md_save_pct, "%"), "Medium Danger Save %", VT_ORANGE),
        box(paste0(g$ld_save_pct, "%"), "Low Danger Save %", VT_MAROON)
      )
    )
  })
  
  # ---- Stat boxes ----
  output$player_stat_boxes <- renderUI({
    req(nrow(selected_player_stats()) > 0)
    p <- selected_player_stats()
    
    box <- function(val, lbl, color = VT_MAROON) {
      column(3,
             div(class = "card text-center mb-3",
                 style = paste0(
                   "border:none; border-radius:12px;",
                   " background:linear-gradient(160deg, ", color, "12 0%, #ffffff 65%);",
                   " box-shadow:0 3px 10px rgba(0,0,0,0.08);",
                   " border-left:5px solid ", color, ";"
                 ),
                 div(class = "card-body py-3",
                     h2(val,
                        style = paste0(
                          "color:", color,
                          "; font-weight:800; margin:0; font-size:2rem;",
                          " letter-spacing:-0.5px;"
                        )),
                     p(toupper(lbl),
                       class = "mb-0",
                       style = paste0(
                         "font-size:0.72rem; font-weight:700; letter-spacing:0.8px;",
                         " color:#6c757d; margin-top:4px;"
                       ))
                 ))
      )
    }
    
    pm_label    <- paste0(ifelse(p$plus_minus > 0, "+", ""), p$plus_minus)
    xgpm_label  <- paste0(ifelse(p$xg_plus_minus > 0, "+", ""), round(p$xg_plus_minus, 2))
    
    tagList(
      fluidRow(
        box(p$goals,      "Goals",         VT_MAROON),
        box(p$a1 + p$a2,  "Assists",       VT_ORANGE),
        box(p$points,     "Points",        VT_MAROON),
        box(round(p$xg_total, 1), "xG Total", VT_ORANGE),
      ),
      fluidRow(
        box(pm_label,   "+/\u2212", VT_ORANGE),
        box(xgpm_label, "xG+/\u2212", VT_MAROON),
        box(p$shots,      "Shots",         VT_ORANGE),
        box(paste0(p$shoot_pct, "%"), "Shoot%", VT_MAROON)
      )
    )
  })

  # Hiding percentile bar stuff
  output$player_percentile_section <- renderUI({
    
    if(input$player_type == "Goalie") {
      
      return(
        fluidRow(
          column(12,
                 card(
                   card_header("Personal Shot Map"),
                   card_body(
                     plotOutput("player_shot_map", height = "360px")
                   )
                 )
          )
        )
      )
      
    } else {
      
      return(
        fluidRow(
          column(5,
                 card(
                   card_header(
                     tagList(
                       "Percentile Rankings",
                       tags$small(class = "text-muted ms-2",
                                  "(vs VT teammates, season totals)")
                     )
                   ),
                   card_body(uiOutput("player_percentiles"))
                 )
          ),
          column(7,
                 card(
                   card_header("Personal Shot Map"),
                   card_body(
                     plotOutput("player_shot_map", height = "360px")
                   )
                 )
          )
        )
      )
      
    }
    
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
      pct_bar("+/\u2212",
              paste0(ifelse(p$plus_minus >= 0, "+", ""), p$plus_minus, " goals"),
              pct("plus_minus")),
      pct_bar("xG+/\u2212",
              paste0(ifelse(p$xg_plus_minus >= 0, "+", ""), round(p$xg_plus_minus, 2), " xG"),
              pct("xg_plus_minus")),
      if (!is.na(p$fo_pct))
        pct_bar("Faceoff Win %", paste0(p$fo_pct, "%  (", p$fo_total, " taken)"),
                pct("fo_pct"))
    )
  })

  # ---- Advanced metrics card ----
  output$advanced_metrics_dynamic <- renderUI({
    req(input$player_select)
    if (input$player_type == "Goalie") {
      uiOutput("goalie_advanced")
    } else {
      uiOutput("player_advanced")
    }
  })
  
  
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
      metric_row("Games Played", p$games_played),
      metric_row("xG Total", round(p$xg_total, 2)),
      metric_row("xG per Game", round(p$xg_total / p$games_played, 2)),
      metric_row("Goals Scored Above Expected", paste0(ifelse(p$g_minus_xg >= 0, "+", ""), p$g_minus_xg),),
      metric_row("Shooting %", paste0(p$shoot_pct, "%"),
                 paste0(p$goals, " goals on ", p$shots, " shots")),
      metric_row("Shots on Goal %", paste0(p$on_goal_pct, "%"),
                 "% of shot attempts that were on goal"),
      metric_row("High Danger Shots", p$high_danger,
                 if (is.na(p$hd_shoot_pct)) "0% shooting" else paste0(p$hd_shoot_pct, "% shooting")),
      metric_row("Medium Danger Shots", p$medium_danger,
                 if (is.na(p$md_shoot_pct)) "0% shooting" else paste0(p$md_shoot_pct, "% shooting")),
      metric_row("Low Danger Shots", p$low_danger,
                 if (is.na(p$ld_shoot_pct)) "0% shooting" else paste0(p$ld_shoot_pct, "% shooting")),
      metric_row("avg xG per Shot", paste0(round(p$xg_per_shot, 3))),
      metric_row("+/\u2212",
                 paste0(ifelse(p$plus_minus >= 0, "+", ""), p$plus_minus)),
      metric_row("xG+/\u2212",
                 paste0(ifelse(p$xg_plus_minus >= 0, "+", ""), round(p$xg_plus_minus, 2))),
      metric_row("Power Play Points", p$pp_points, paste0(p$pp_goals, " goals")),
      metric_row("Short Handed Points", p$sh_points, paste0(p$sh_goals, " goals")),
      metric_row("Takeaways", p$takeaways),
      metric_row("Giveaways", p$giveaways),
      metric_row("Takeaways", p$takeaways),
      metric_row("Giveaways", p$giveaways),
      metric_row("TA/GA Ratio",
                 if (!is.na(p$ta_ga_ratio)) p$ta_ga_ratio else paste0(p$takeaways, "/0")),
      metric_row("Blocked Shots", p$blocks),
      if (p$fo_total > 0)
        metric_row("Faceoff W\u2212L", paste0(p$fo_wins, "\u2212", p$fo_total - p$fo_wins),
                   paste0(p$fo_pct, "% win rate"))
    )
  })
  
  output$goalie_advanced <- renderUI({
    req(nrow(selected_player_stats()) > 0)
    g <- selected_player_stats()
    
    metric_row <- function(label, value, note = NULL) {
      div(class = "d-flex justify-content-between border-bottom py-2",
          div(tags$b(label),
              if (!is.null(note)) tags$small(class = "text-muted d-block", note)),
          div(style = "font-size:1.1rem; font-weight:700;", value)
      )
    }
    
    pct_str <- function(x) if (is.na(x) || is.nan(x)) "N/A" else paste0(x, "%")
    
    tagList(
      metric_row("Games Played", g$games_played),
      metric_row("Shots Against", g$shots_against),
      metric_row("Saves", g$saves),
      metric_row("Save %", pct_str(g$save_pct)),
      metric_row("Goals Against", g$goals_against),
      metric_row("Expected Goals Against", g$xgaa * g$games_played),
      metric_row("Goals Saved Above Expected",
        if (length(g$gsax) == 0 || is.na(g$gsax)) "N/A"
        else paste0(ifelse(g$gsax >= 0, "+", ""), g$gsax)
      ),
      metric_row("High Danger Shots", g$high_danger_shots,
                 paste0(g$high_danger_saves, " saved")),
      metric_row("High Danger Save %", pct_str(g$hd_save_pct)),
      metric_row("Medium Danger Shots", g$medium_danger_shots,
                 paste0(g$medium_danger_saves, " saved")),
      metric_row("Medium Danger Save %", pct_str(g$md_save_pct)),
      metric_row("Low Danger Shots", g$low_danger_shots,
                 paste0(g$low_danger_saves, " saved")),
      metric_row("Low Danger Save %", pct_str(g$ld_save_pct))
    )
  })

  # ---- Personal shot map ----
  output$player_shot_map <- renderPlot({
    req(input$player_select)
    d <- analytics_data()

    player_shots <- tryCatch({
      s <- filter_by_game(d$shots)
      if (nrow(s) == 0 || !"player_id" %in% names(s)) return(draw_rink())
      if(input$player_type == "Goalie") {
        
        s <- s[
          s$team != "VT" & !is.na(s$team) &
            s$goalie_id == as.integer(input$player_select) &
            !is.na(s$goalie_id) &
            !is.na(s$x_coord) &
            !is.na(s$result) &
            s$result %in% c("save", "goal"),
        ]
        
      } else {
        
        s <- s[
          s$player_id == as.integer(input$player_select) &
            !is.na(s$x_coord),
        ]
        
      }
      if (nrow(s) == 0) return(draw_rink())
      s$result <- ifelse(is.na(s$result), "save", s$result)
      # Normalize onto one consistent side regardless of which end
      # VT was actually attacking that period.
      period_col <- if ("period" %in% names(s)) s$period else rep("1", nrow(s))
      disp <- to_display_xy(s$x_coord, s$y_coord, period_col)
      s$x_coord <- disp$x
      s$y_coord <- disp$y
      s
    }, error = function(e) NULL)

    if (is.null(player_shots) || nrow(player_shots) == 0) {
      draw_rink(attack_right = FALSE)
    } else {
      draw_rink(shots_df = player_shots, attack_right = FALSE)
    }
  })

  # ---- Game-by-game trend chart ----
  output$trend_dynamic <- renderUI({
    req(input$player_select)
    if (input$player_type == "Goalie") {
      plotOutput("goalie_trend", height = "280px")
    } else {
      plotOutput("player_trend", height = "280px")
    }
  })
  
  output$player_trend <- renderPlot({
    req(input$player_select)
    d        <- analytics_data()
    pid      <- as.integer(input$player_select)
    games_df <- d$games
    
    if (nrow(games_df) == 0) {
      plot.new(); text(0.5, 0.5, "No game data yet", cex = 1.2); return()
    }
    
    pm_by_game <- compute_plus_minus_by_game(d$on_ice, d$shots)
    player_pm  <- pm_by_game[pm_by_game$player_id == pid, ]
    
    trend <- games_df |>
      dplyr::select(game_id = id, game_date, opponent) |>
      dplyr::left_join(player_pm, by = "game_id") |>
      dplyr::mutate(
        plus_minus    = tidyr::replace_na(plus_minus, 0L),
        xg_plus_minus = tidyr::replace_na(xg_plus_minus, 0),
        label = substr(opponent, 1, 10)
      ) |>
      dplyr::arrange(game_date)
    
    if (all(trend$plus_minus == 0) && all(trend$xg_plus_minus == 0)) {
      plot.new()
      text(0.5, 0.5, "No on-ice data for this player yet", cex = 1.1)
      return()
    }
    
    ggplot(trend, aes(x = label)) +
      geom_hline(yintercept = 0, color = "#ccc", linewidth = 0.4) +
      geom_col(aes(y = plus_minus, fill = "+/-"), width = 0.4,
               position = position_nudge(x = -0.22)) +
      geom_col(aes(y = xg_plus_minus, fill = "xG+/-"), width = 0.4,
               position = position_nudge(x = 0.22)) +
      scale_fill_manual(values = c("+/-" = VT_MAROON, "xG+/-" = VT_ORANGE), name = NULL) +
      labs(x = "Opponent", y = "Value") +
      theme_minimal(base_size = 12) +
      theme(
        legend.position    = "top",
        axis.text.x        = element_text(angle = 30, hjust = 1),
        panel.grid.major.x = element_blank()
      )
  })
  
  
  # ---- Goalie game-by-game trend: Save % and GSAx per game ----
  output$goalie_trend <- renderPlot({
    req(input$player_select)
    d        <- analytics_data()
    gid      <- as.integer(input$player_select)
    games_df <- d$games
    
    if (nrow(games_df) == 0) {
      plot.new(); text(0.5, 0.5, "No game data yet", cex = 1.2); return()
    }
    
    shots <- d$shots
    has_faced <- nrow(shots) > 0 && "goalie_id" %in% names(shots)
    
    faced <- if (has_faced) {
      shots[!is.na(shots$goalie_id) & shots$goalie_id == gid &
              shots$team != "VT" & !is.na(shots$team), ]
    } else {
      shots[0, ]
    }
    
    if (nrow(faced) == 0) {
      plot.new(); text(0.5, 0.5, "No shots faced by this goalie yet", cex = 1.1); return()
    }
    
    st_col <- if ("shot_type" %in% names(faced)) faced$shot_type else rep("wrist", nrow(faced))
    faced$xga_shot <- mapply(function(x, y, st) {
      if (is.na(x) || is.na(y)) return(0)
      calc_xg(200 - x, y, st)
    }, faced$x_coord, faced$y_coord, st_col)
    
    per_game <- faced |>
      dplyr::group_by(game_id) |>
      dplyr::summarise(
        goals_against = sum(result == "goal", na.rm = TRUE),
        xga           = round(sum(xga_shot, na.rm = TRUE), 2),
        .groups = "drop"
      )
    
    trend <- games_df |>
      dplyr::select(game_id = id, game_date, opponent) |>
      dplyr::inner_join(per_game, by = "game_id") |>
      dplyr::mutate(label = substr(opponent, 1, 10)) |>
      dplyr::arrange(game_date)
    
    if (nrow(trend) == 0) {
      plot.new(); text(0.5, 0.5, "No shots faced by this goalie yet", cex = 1.1); return()
    }
    
    ggplot(trend, aes(x = label)) +
      geom_col(aes(y = goals_against, fill = "GA"), width = 0.4,
               position = position_nudge(x = -0.22)) +
      geom_col(aes(y = xga, fill = "xGA"), width = 0.4,
               position = position_nudge(x = 0.22)) +
      scale_fill_manual(values = c("GA" = VT_MAROON, "xGA" = VT_ORANGE), name = NULL) +
      labs(x = "Opponent", y = "Goals Against") +
      theme_minimal(base_size = 12) +
      theme(
        legend.position    = "top",
        axis.text.x        = element_text(angle = 30, hjust = 1),
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
    initials_html <- paste0(
      "this.outerHTML='<div style=\"width:72px;height:72px;border-radius:50%;",
      "background:#4a4a4a;color:white;display:flex;align-items:center;",
      "justify-content:center;font-weight:700;font-size:1.1rem;margin:auto;\">",
      initials, "</div>'"
    )
    opp_graphic <- if (has_logo) {
      tags$img(src     = game$opponent_logo_url,
               height  = "72px",
               style   = "object-fit:contain; max-width:90px;",
               onerror = initials_html)
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
    # A game counts as "played" if its date has passed OR it already has
    # recorded goals — so testing/entering stats for a future-dated game
    # still shows the live score instead of being stuck on "UPCOMING".
    has_any_goals <- sched$id %in% (sb_select("goals", select = "game_id")$game_id %||% integer(0))
    is_played <- as.Date(sched$game_date) <= today | has_any_goals
    
    upcoming <- sched[!is_played, ]
    past     <- sched[is_played, ]

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
  # -- Create an observer for each game report button (dynamic) --
  observe({
    sched <- schedule_data()
    req(nrow(sched) > 0)
    
    # Must match the same "played" definition used in output$schedule_cards,
    # or a game showing a real score there won't have had its report
    # button observer created here — making the click do nothing.
    has_any_goals <- sched$id %in% (sb_select("goals", select = "game_id")$game_id %||% integer(0))
    past_ids <- sched$id[as.Date(sched$game_date) <= Sys.Date() | has_any_goals]
    
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

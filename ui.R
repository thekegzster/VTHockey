# ============================================================
# ui.R  -  The visual layout of the app
# ============================================================

ui <- page_navbar(
  title = tags$span(
    tags$img(src = "vt_logo.png", height = "28px",
             style = "margin-right: 8px; vertical-align: middle;",
             onerror = "this.style.display='none'"),
    "VT Hockey Data Collector"
  ),
  theme    = app_theme,
  fillable = FALSE,

  # header appears above every panel; useShinyjs() and tags$head() live here
  header = tagList(
    useShinyjs(),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
    uiOutput("session_banner")
  ),

  # ===========================================================
  # TAB 1: SETUP
  # ===========================================================
  nav_panel(
    title = tagList(icon("gear"), " Setup"),
    value = "tab_setup",

    div(class = "container-fluid py-4", style = "max-width: 700px;",

      h2("Session Setup", style = paste0("color:", VT_MAROON, "; font-weight:700;")),
      p("Fill this out before each game session. Your name will be attached to
         every record you submit so we can track who entered what."),

      # ---- Collector name ----
      card(
        card_header("Who are you?"),
        card_body(
          textInput("collector_name", "Your name",
                    placeholder = "e.g. Alex Smith",
                    width = "100%"),
          helpText("This is recorded with every entry you submit.")
        )
      ),

      br(),

      # ---- Select existing game ----
      card(
        card_header("Select the current game"),
        card_body(
          selectInput("game_select", "Choose game", choices = NULL, width = "100%"),
          actionButton("btn_refresh_games", "Refresh game list",
                       icon = icon("rotate"), class = "btn-outline-secondary btn-sm"),
          hr(),
          div(
            id = "setup_status",
            uiOutput("connection_status")
          )
        )
      ),

      br(),

      # ---- Create a new game ----
      card(
        card_header("...or create a new game"),
        card_body(
          fluidRow(
            column(6, dateInput("new_game_date", "Game date", value = Sys.Date(), width = "100%")),
            column(6, textInput("new_game_opponent", "Opponent", placeholder = "e.g. Penn State", width = "100%"))
          ),
          fluidRow(
            column(6,
              selectInput("new_game_location", "Location",
                          choices = c("Home" = "home", "Away" = "away", "Neutral" = "neutral"),
                          width = "100%")
            ),
            column(6,
              textInput("new_game_season", "Season", value = "2025-26", width = "100%")
            )
          ),
          actionButton("btn_create_game", "Create game",
                       icon = icon("plus"), class = "btn-secondary")
        )
      ),

      br(),

      # ---- Start session button ----
      div(
        class = "d-grid",
        actionButton(
          "btn_start_session", "Start Collecting Data",
          icon  = icon("play"),
          class = "btn-primary btn-lg",
          style = "font-size: 1.2rem; padding: 14px;"
        )
      )
    )
  ),

  # ===========================================================
  # TAB 2: FACEOFFS
  # ===========================================================
  nav_panel(
    title = tagList(icon("circle-dot"), " Faceoffs"),
    value = "tab_faceoffs",

    div(class = "container-fluid py-4", style = "max-width: 600px;",

      uiOutput("session_required_faceoffs"),

      card(
        card_header(h4("Record a Faceoff", class = "mb-0")),
        card_body(
          fluidRow(
            column(6,
              selectInput("fo_period", "Period", choices = PERIOD_CHOICES, width = "100%")
            ),
            column(6,
              textInput("fo_time", "Time remaining (MM:SS)",
                        placeholder = "14:23", width = "100%")
            )
          ),
          selectizeInput("fo_player", "VT Player taking faceoff",
                         choices = NULL, width = "100%",
                         options = list(placeholder = "Start typing name or #...")),
          fluidRow(
            column(6,
              selectInput("fo_result", "Result",
                          choices = c("Win" = "win", "Loss" = "loss"),
                          width = "100%")
            ),
            column(6,
              selectInput("fo_zone", "Zone",
                          choices = c("Offensive Zone" = "offensive",
                                      "Defensive Zone" = "defensive",
                                      "Neutral Zone"   = "neutral"),
                          width = "100%")
            )
          ),
          div(
            class = "d-flex gap-2 mt-3",
            div(
              class = "flex-grow-1",
              actionButton("btn_submit_faceoff", "Submit Faceoff",
                           icon = icon("check"), class = "btn-primary w-100",
                           style = "font-size: 1.1rem; padding: 12px;")
            ),
            actionButton("btn_undo_faceoff", "Undo Last",
                         icon = icon("rotate-left"), class = "btn-outline-danger")
          )
        )
      ),

      br(),
      card(
        card_header("Recent Faceoff Entries"),
        card_body(uiOutput("faceoff_log"))
      )
    )
  ),

  # ===========================================================
  # TAB 3: HITS
  # ===========================================================
  nav_panel(
    title = tagList(icon("bolt"), " Hits"),
    value = "tab_hits",

    div(class = "container-fluid py-4", style = "max-width: 600px;",

      uiOutput("session_required_hits"),

      card(
        card_header(h4("Record a Hit", class = "mb-0")),
        card_body(
          fluidRow(
            column(6,
              selectInput("hit_period", "Period", choices = PERIOD_CHOICES, width = "100%")
            ),
            column(6,
              textInput("hit_time", "Time remaining (MM:SS)",
                        placeholder = "14:23", width = "100%")
            )
          ),
          selectizeInput("hit_player", "VT Player",
                         choices = NULL, width = "100%",
                         options = list(placeholder = "Start typing name or #...")),
          selectInput("hit_type", "Hit type",
                      choices = c("VT player delivered a hit" = "delivered",
                                  "VT player received a hit"  = "received"),
                      width = "100%"),
          div(
            class = "d-flex gap-2 mt-3",
            div(
              class = "flex-grow-1",
              actionButton("btn_submit_hit", "Submit Hit",
                           icon = icon("check"), class = "btn-primary w-100",
                           style = "font-size: 1.1rem; padding: 12px;")
            ),
            actionButton("btn_undo_hit", "Undo Last",
                         icon = icon("rotate-left"), class = "btn-outline-danger")
          )
        )
      ),

      br(),
      card(
        card_header("Recent Hit Entries"),
        card_body(uiOutput("hit_log"))
      )
    )
  ),

  # ===========================================================
  # TAB 4: SHOTS + SHOT MAP
  # ===========================================================
  nav_panel(
    title = tagList(icon("crosshairs"), " Shots"),
    value = "tab_shots",

    div(class = "container-fluid py-4",

      uiOutput("session_required_shots"),

      fluidRow(
        # Left column: shot details form
        column(4,
          card(
            card_header(h4("Record a Shot", class = "mb-0")),
            card_body(
              fluidRow(
                column(6,
                  selectInput("shot_period", "Period", choices = PERIOD_CHOICES, width = "100%")
                ),
                column(6,
                  textInput("shot_time", "Time (MM:SS)", placeholder = "14:23", width = "100%")
                )
              ),
              selectizeInput("shot_player", "VT Player",
                             choices = NULL, width = "100%",
                             options = list(placeholder = "Start typing name or #...")),
              selectInput("shot_type", "Shot type",
                          choices = SHOT_TYPE_CHOICES, width = "100%"),
              selectInput("shot_result", "Result",
                          choices = c("On goal (save)"  = "save",
                                      "Goal"            = "goal",
                                      "Missed net"      = "missed",
                                      "Blocked"         = "blocked"),
                          width = "100%"),
              selectInput("shot_team", "Team shooting",
                          choices = c("VT" = "VT", "Opponent" = "opponent"),
                          width = "100%"),
              div(
                id    = "shot_location_display",
                class = "alert alert-info py-2 mb-2",
                style = "font-size: 0.9rem;",
                uiOutput("shot_coords_display")
              ),
              div(
                class = "d-flex gap-2",
                div(
                  class = "flex-grow-1",
                  actionButton("btn_submit_shot", "Submit Shot",
                               icon = icon("check"), class = "btn-primary w-100",
                               style = "font-size: 1.1rem; padding: 12px;")
                ),
                actionButton("btn_undo_shot", "Undo Last",
                             icon = icon("rotate-left"), class = "btn-outline-danger")
              ),
              actionButton("btn_clear_shot_loc", "Clear location click",
                           icon = icon("xmark"), class = "btn-outline-secondary btn-sm mt-2")
            )
          ),
          br(),
          card(
            card_header("Recent Shot Entries"),
            card_body(uiOutput("shot_log"))
          )
        ),

        # Right column: interactive rink map
        column(8,
          card(
            card_header(
              div(
                class = "d-flex justify-content-between align-items-center",
                span(h5("Click on the rink to mark shot location", class = "mb-0")),
                checkboxInput("show_game_shots", "Show all game shots", value = TRUE)
              )
            ),
            card_body(
              plotOutput(
                "rink_plot",
                click  = "rink_click",
                height = "420px"
              ),
              helpText("Click anywhere on the rink above to record where the shot was taken.",
                       "The gold crosshair marks your most recent click.",
                       "Shot location is optional — you can submit without clicking.")
            )
          )
        )
      )
    )
  ),

  # ===========================================================
  # TAB 5: GOALS & ON-ICE
  # ===========================================================
  nav_panel(
    title = tagList(icon("star"), " Goals"),
    value = "tab_goals",

    div(class = "container-fluid py-4", style = "max-width: 750px;",

      uiOutput("session_required_goals"),

      card(
        card_header(h4("Record a Goal", class = "mb-0")),
        card_body(

          fluidRow(
            column(4,
              selectInput("goal_period", "Period", choices = PERIOD_CHOICES, width = "100%")
            ),
            column(4,
              textInput("goal_time", "Time remaining (MM:SS)",
                        placeholder = "14:23", width = "100%")
            ),
            column(4,
              selectInput("goal_team", "Which team scored?",
                          choices = c("VT" = "VT", "Opponent" = "opponent"),
                          width = "100%")
            )
          ),

          hr(),
          h6("Scoring Details", class = "fw-bold text-muted"),

          uiOutput("goal_vt_fields"),  # shown/hidden based on goal_team

          selectInput("goal_situation", "Situation",
                      choices = c("Even strength"   = "even",
                                  "Power play"      = "power_play",
                                  "Short handed"    = "short_handed",
                                  "Penalty shot"    = "penalty_shot"),
                      width = "100%"),

          checkboxInput("goal_empty_net", "Empty net goal", value = FALSE),

          hr(),
          h6("VT Players On Ice", class = "fw-bold text-muted"),
          helpText("Select ALL VT players who were on the ice when the goal was scored (up to 6)."),

          selectizeInput(
            "on_ice_players",
            label    = NULL,
            choices  = NULL,
            multiple = TRUE,
            width    = "100%",
            options  = list(
              placeholder  = "Click to select players...",
              maxItems     = 6,
              closeAfterSelect = FALSE
            )
          ),

          div(
            class = "d-flex gap-2 mt-3",
            div(
              class = "flex-grow-1",
              actionButton("btn_submit_goal", "Submit Goal",
                           icon = icon("check"), class = "btn-primary w-100",
                           style = "font-size: 1.1rem; padding: 12px;")
            ),
            actionButton("btn_undo_goal", "Undo Last",
                         icon = icon("rotate-left"), class = "btn-outline-danger")
          )
        )
      ),

      br(),
      card(
        card_header("Recent Goal Entries"),
        card_body(uiOutput("goal_log"))
      )
    )
  ),

  # ===========================================================
  # TAB 6: SUMMARY / DASHBOARD
  # ===========================================================
  nav_panel(
    title = tagList(icon("chart-bar"), " Summary"),
    value = "tab_summary",

    div(class = "container-fluid py-4",

      div(
        class = "d-flex justify-content-between align-items-center mb-4",
        h2("Game Summary", style = paste0("color:", VT_MAROON, "; font-weight:700; margin:0;")),
        actionButton("btn_refresh_summary", "Refresh Data",
                     icon = icon("rotate"), class = "btn-primary")
      ),

      # Stat counters
      uiOutput("summary_value_boxes"),

      br(),

      # Shot map
      fluidRow(
        column(8,
          card(
            card_header("Shot Map (all shots this game)"),
            card_body(
              plotOutput("summary_shot_map", height = "380px")
            )
          )
        ),
        column(4,
          card(
            card_header("Goal Log"),
            card_body(uiOutput("summary_goal_list"))
          )
        )
      ),

      br(),

      # Detail tables
      tabsetPanel(
        tabPanel("Faceoffs",
          br(),
          DTOutput("table_faceoffs")
        ),
        tabPanel("Hits",
          br(),
          DTOutput("table_hits")
        ),
        tabPanel("Shots",
          br(),
          DTOutput("table_shots")
        ),
        tabPanel("Goals",
          br(),
          DTOutput("table_goals")
        ),
        tabPanel("On Ice",
          br(),
          DTOutput("table_on_ice")
        )
      )
    )
  ),

  # ===========================================================
  # TAB 7: PLAYER ANALYSIS  (Baseball Savant style)
  # ===========================================================
  nav_panel(
    title = tagList(icon("person"), " Players"),
    value = "tab_players",

    div(class = "container-fluid py-4",

      # ---- Player selector ----
      fluidRow(
        column(4,
          selectizeInput("player_select", "Select player",
                         choices  = NULL, width = "100%",
                         options  = list(placeholder = "Choose a player..."))
        ),
        column(8,
          uiOutput("player_header")
        )
      ),

      br(),

      uiOutput("player_session_warn"),

      # ---- Stat boxes row ----
      uiOutput("player_stat_boxes"),

      br(),

      # ---- Percentile chart + shot map ----
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
      ),

      br(),

      # ---- Advanced analytics cards ----
      fluidRow(
        column(6,
          card(
            card_header("Advanced Metrics"),
            card_body(uiOutput("player_advanced"))
          )
        ),
        column(6,
          card(
            card_header("Game-by-Game Trend"),
            card_body(
              plotOutput("player_trend", height = "280px")
            )
          )
        )
      ),

      br(),

      # ---- xG explanation ----
      div(
        class = "alert alert-light border mt-2",
        style = "font-size:0.85rem;",
        tags$b("About xG (Expected Goals): "),
        "xG estimates the probability that a shot becomes a goal based on
         where it was taken (distance from net, angle) and shot type.
         A shot from directly in front at 15 ft might have xG = 0.28,
         while a shot from centre ice has xG ≈ 0.03.
         G − xG measures goal-scoring above or below expectation — a
         positive number means the player is finishing better than average."
      )
    )
  ),

  # ===========================================================
  # TAB 8: SCHEDULE
  # ===========================================================
  nav_panel(
    title = tagList(icon("calendar-days"), " Schedule"),
    value = "tab_schedule",

    div(class = "container-fluid py-4",

      # ---- Top bar ----
      div(class = "d-flex justify-content-between align-items-center mb-4",
        h2("Season Schedule",
           style = paste0("color:", VT_MAROON, "; font-weight:700; margin:0;")),
        div(class = "d-flex gap-2",
          actionButton("btn_refresh_schedule", "Refresh",
                       icon = icon("rotate"),
                       class = "btn-outline-secondary btn-sm"),
          actionButton("btn_season_report",
                       "Full Season Report",
                       icon  = icon("file-export"),
                       class = "btn-primary")
        )
      ),

      # ---- Report status + download (shown after generation) ----
      uiOutput("sched_report_status"),
      br(),

      # ---- Schedule cards (built dynamically in server) ----
      uiOutput("schedule_cards"),

      br(),
      div(
        class = "alert alert-light border",
        style = "font-size:0.82rem;",
        icon("circle-info"), " ",
        tags$b("Logos: "),
        "To add opponent logos, paste an image URL into the ",
        tags$code("opponent_logo_url"),
        " column of the ",
        tags$code("games"),
        " table in your Supabase Table Editor. Most school logos are
         available from ESPN, team websites, or Wikimedia Commons."
      )
    )
  )
)

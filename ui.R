# ============================================================
# ui.R  -  The visual layout of the app
# ============================================================

# ============================================================
# Collector-only tabs (Setup, Faceoffs, Turnovers, Shots, On Ice)
# ============================================================
collector_tabs <- list(
  
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
        
        card(
          card_header("Who are you?"),
          card_body(
            textInput("collector_name", "Your name",
                      placeholder = "e.g. Matthew Schaefer",
                      width = "100%"),
            helpText("This is recorded with every entry you submit.")
          )
        ),
        
        br(),
        
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
              column(
                6,
                radioButtons(
                  "fo_result",
                  "Result",
                  choices = c(
                    "Win" = "win",
                    "Loss" = "loss"
                  ),
                  inline = TRUE
                )
              ),
              column(
                6,
                selectInput(
                  "fo_zone",
                  "Zone",
                  choices = c(
                    "Offensive Zone" = "offensive",
                    "Defensive Zone" = "defensive",
                    "Neutral Zone" = "neutral"
                  ),
                  width = "100%"
                )
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
  # TAB 3: GIVEAWAYS / TAKEAWAYS
  # ===========================================================
  nav_panel(
    title = tagList(icon("right-left"), " Turnovers"),
    value = "tab_turnovers",
    
    div(class = "container-fluid py-4", style = "max-width: 600px;",
        
        uiOutput("session_required_turnovers"),
        
        card(
          card_header(h4("Record a Turnover", class = "mb-0")),
          card_body(
            fluidRow(
              column(6,
                     selectInput("to_period", "Period", choices = PERIOD_CHOICES, width = "100%")
              ),
              column(6,
                     textInput("to_time", "Time remaining (MM:SS)",
                               placeholder = "14:23", width = "100%")
              )
            ),
            selectizeInput("to_player", "VT Player",
                           choices = NULL, width = "100%",
                           options = list(placeholder = "Start typing name or #...")),
            fluidRow(
              column(
                6,
                radioButtons(
                  "to_type",
                  "Type",
                  choices = c(
                    "Takeaway" = "takeaway",
                    "Giveaway" = "giveaway"
                  ),
                  inline = FALSE
                )
              ),
              column(
                6,
                radioButtons(
                  "to_zone",
                  "Zone",
                  choices = c(
                    "Offensive" = "offensive",
                    "Neutral" = "neutral",
                    "Defensive" = "defensive"
                  ),
                  inline = FALSE
                )
              )
            ),
            div(
              class = "d-flex gap-2 mt-3",
              div(
                class = "flex-grow-1",
                actionButton("btn_submit_to", "Submit Turnover",
                             icon = icon("check"), class = "btn-primary w-100",
                             style = "font-size: 1.1rem; padding: 12px;")
              ),
              actionButton("btn_undo_to", "Undo Last",
                           icon = icon("rotate-left"), class = "btn-outline-danger")
            )
          )
        ),
        
        br(),
        card(
          card_header("Recent Turnover Entries"),
          card_body(uiOutput("to_log"))
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
                     radioButtons(
                       "shot_team",
                       "Team shooting",
                       choices = c(
                         "VT" = "VT",
                         "Opponent" = "opponent"
                       ),
                       inline = TRUE
                     ),
                     conditionalPanel(
                       condition = "input.shot_team == 'VT'",
                       selectizeInput("shot_player", "VT Player",
                                      choices = NULL, width = "100%",
                                      options = list(placeholder = "Start typing name or #..."))
                     ),
                     radioButtons(
                       "shot_type",
                       "Shot type",
                       choices = SHOT_TYPE_CHOICES,
                       inline = FALSE
                     ),
                     
                     radioButtons(
                       "shot_result",
                       "Result",
                       choices = c(
                         "On goal (save)" = "save",
                         "Goal" = "goal",
                         "Missed net" = "missed",
                         "Blocked" = "blocked"
                       ),
                       inline = FALSE
                     ),
                     
                     conditionalPanel(
                       condition = "input.shot_team == 'opponent' && input.shot_result == 'blocked'",
                       div(
                         style = paste0("border-left: 4px solid ", VT_ORANGE,
                                        "; padding-left: 10px; margin: 6px 0;"),
                         h6(tagList(icon("shield"), " Blocked By"),
                            class = "fw-bold mb-2",
                            style = paste0("color:", VT_ORANGE, ";")),
                         selectizeInput("shot_blocked_by", "VT player who blocked it",
                                        choices = NULL, width = "100%",
                                        options = list(placeholder = "Start typing name or #..."))
                       )
                     ),
                     
                     conditionalPanel(
                       condition = "input.shot_result == 'goal'",
                       div(
                         style = paste0("border-left: 4px solid #861F41;",
                                        " padding-left: 10px; margin: 6px 0;"),
                         h6(tagList(icon("star"), " Goal Details"),
                            class = "fw-bold mb-2",
                            style = "color:#861F41;"),
                         selectInput("shot_goal_situation", "Situation",
                                     choices = c("Even strength" = "even",
                                                 "Power play"    = "power_play",
                                                 "Short handed"  = "short_handed",
                                                 "Penalty shot"  = "penalty_shot"),
                                     width = "100%"),
                         checkboxInput("shot_goal_empty_net", "Empty net goal",
                                       value = FALSE),
                         conditionalPanel(
                           condition = "input.shot_team == 'VT'",
                           uiOutput("shot_goal_assist_fields")
                         )
                       )
                     ),
                     
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
  # TAB 5: ON ICE TRACKER
  # ===========================================================
  nav_panel(
    title = tagList(icon("users"), " On Ice"),
    value = "tab_on_ice",
    
    div(class = "container-fluid py-4", style = "max-width: 680px;",
        
        uiOutput("session_required_on_ice"),
        
        uiOutput("on_ice_banner"),
        
        br(),
        
        card(
          card_header(
            div(class = "d-flex justify-content-between align-items-center",
                h4("Update Line", class = "mb-0"),
                tags$small("Type jersey numbers then hit Set Line",
                           class = "text-muted")
            )
          ),
          card_body(
            
            div(
              class = "text-center mb-4",
              tags$h6(
                "Goalie (leave blank if pulled)",
                class = "fw-bold mb-2",
                style = paste0("color:", VT_MAROON, ";")
              ),
              div(
                class = "d-flex justify-content-center",
                tags$input(
                  id          = "ice_goalie",
                  type        = "text",
                  inputmode   = "numeric",
                  maxlength   = "2",
                  placeholder = "#",
                  class       = "form-control jersey-box goalie-box",
                  autocomplete = "off"
                )
              ),
              uiOutput("ice_goalie_lbl")
            ),
            
            tags$h6(
              "Skaters (leave unused boxes blank)",
              class = "fw-bold text-center mb-2",
              style = paste0("color:", VT_MAROON, ";")
            ),
            
            div(
              class = "d-flex flex-wrap gap-3 justify-content-center mb-1",
              lapply(1:6, function(i) {
                div(
                  class = "text-center",
                  tags$input(
                    id          = paste0("ice_", i),
                    type        = "text",
                    inputmode   = "numeric",
                    maxlength   = "2",
                    placeholder = "#",
                    class       = "form-control jersey-box",
                    autocomplete = "off"
                  ),
                  uiOutput(paste0("ice_lbl_", i))
                )
              })
            ),
            
            br(),
            
            actionButton(
              "btn_set_line", "Set Line",
              icon  = icon("check"),
              class = "btn-success w-100",
              style = "font-size:1.25rem; padding:14px; font-weight:700;"
            ),
            
            br(), br(),
            
            div(
              class = "alert alert-light border",
              style = "font-size:0.82rem;",
              icon("circle-info"), " ",
              "The active line is ", tags$b("automatically recorded"),
              " with every shot logged in the Shots tab.",
              " Update this whenever there is a line change."
            )
          )
        ),
        
        br(),
        
        card(
          card_header("Line Change Log"),
          card_body(uiOutput("line_change_log"))
        )
    )
  )
)

# ============================================================
# Shared tabs (visible in both modes: Summary, Players, Schedule)
# ============================================================
shared_tabs <- list(
  
  # ===========================================================
  # TAB 6: SUMMARY / DASHBOARD
  # ===========================================================
  nav_panel(
    title = tagList(icon("chart-bar"), " Summary"),
    value = "tab_summary",
    
    div(class = "container-fluid py-4",
        
        uiOutput("viewer_game_picker"),
        
        div(
          class = "d-flex justify-content-between align-items-center mb-4",
          h2("Game Summary", style = paste0("color:", VT_MAROON, "; font-weight:700; margin:0;")),
          div(class = "d-flex align-items-center gap-2",
              div(style = "min-width:180px;",
                  selectInput("summary_period_filter", NULL,
                              choices = c("All Periods" = "all", PERIOD_CHOICES),
                              width = "100%")
              ),
              actionButton("btn_refresh_summary", "Refresh Data",
                           icon = icon("rotate"), class = "btn-primary")
          )
        ),
        
        uiOutput("summary_scoreboard"),
        
        br(),
        
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
                   card_header("Team Stats"),
                   card_body(uiOutput("summary_team_stats"))
                 )
          )
        ),
        
        br(),
        
        tabsetPanel(
          tabPanel("Play-by-Play",
                   br(),
                   DTOutput("table_play_by_play")
          ),
          tabPanel("Player Stats",
                   br(),
                   DTOutput("table_player_stats")
          ),
          tabPanel("Goalie Stats",
                   br(),
                   DTOutput("table_goalie_stats")
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
        
        fluidRow(
          column(3,
                 selectInput("player_game_filter", "Game",
                             choices = c("Full Season" = "season"),
                             width = "100%")
          ),
          
          column(3,
                 selectInput("player_type", "Player Type",
                             choices = c("Skater", "Goalie"),
                             selected = "Skater")
          ),
          
          column(3,
                 selectizeInput("player_select", "Select player",
                                choices  = NULL,
                                width = "100%",
                                options = list(placeholder = "Choose a player..."))
          ),
          column(8,
                 uiOutput("player_header")
          )
        ),
        
        br(),
        
        uiOutput("player_session_warn"),
        
        uiOutput("player_stat_boxes_dynamic"),
        
        br(),
        
        uiOutput("player_percentile_section"),
        
        fluidRow(
          column(6,
                 card(
                   card_header("Advanced Metrics"),
                   card_body(uiOutput("advanced_metrics_dynamic"))
                 )
          ),
          column(6,
                 card(
                   card_header("Game-by-Game Trend"),
                   card_body(uiOutput("trend_dynamic"))
                 )
          )
        ),
        
        br(),
    )
  ),
  
  # ===========================================================
  # TAB 8: SCHEDULE
  # ===========================================================
  nav_panel(
    title = tagList(icon("calendar-days"), " Schedule"),
    value = "tab_schedule",
    
    div(class = "container-fluid py-4",
        
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
        
        uiOutput("sched_report_status"),
        br(),
        
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

# ============================================================
# ui as a function of the request — reads ?mode= from the URL
# before the page renders, and builds a different navbar for
# collector vs viewer links.
# ============================================================
ui <- function(request) {
  query <- shiny::parseQueryString(request$QUERY_STRING)
  app_mode_ui <- if (!is.null(query$mode) && query$mode == "viewer") "viewer" else "collector"
  
  all_tabs <- if (app_mode_ui == "viewer") shared_tabs else c(collector_tabs, shared_tabs)
  
  navbar_args <- c(
    list(
      title = tags$span(
        tags$img(src = "vt_logo.png", height = "28px",
                 style = "margin-right: 8px; vertical-align: middle;",
                 onerror = "this.style.display='none'"),
        "VT Hockey Data Collector"
      ),
      theme    = app_theme,
      fillable = FALSE,
      header = tagList(
        useShinyjs(),
        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
        uiOutput("session_banner")
      )
    ),
    all_tabs
  )
  
  do.call(page_navbar, navbar_args)
}
# ============================================================
# R/supabase.R
# All functions that talk to the Supabase REST API
# ============================================================
# How Supabase's REST API works:
#   - It's a standard HTTP API that wraps your PostgreSQL tables
#   - Every table becomes an endpoint: /rest/v1/{table_name}
#   - You send JSON for inserts, and use query parameters for filters
#   - The anon key goes in both the "apikey" and "Authorization" headers
# ============================================================

library(httr2)
library(jsonlite)

# ---- Internal helper: build a base request with auth headers ----

.sb_base_req <- function(path) {
  url  <- Sys.getenv("SUPABASE_URL")
  key  <- Sys.getenv("SUPABASE_ANON_KEY")

  if (url == "" || key == "") {
    stop("Supabase credentials not found. Check your .env file.")
  }

  request(paste0(url, "/rest/v1/", path)) |>
    req_headers(
      "apikey"        = key,
      "Authorization" = paste("Bearer", key),
      "Content-Type"  = "application/json"
    )
}

# ---- Check if credentials are configured ----

sb_check_connection <- function() {
  tryCatch({
    resp <- .sb_base_req("games?select=id&limit=1") |>
      req_perform()
    resp_status(resp) == 200
  }, error = function(e) FALSE)
}

# ---- Insert a single record into a table ----
# Returns list with $success (TRUE/FALSE) and $id (inserted row id) or $error

sb_insert <- function(table, data) {
  tryCatch({
    resp <- .sb_base_req(table) |>
      req_headers("Prefer" = "return=representation") |>  # ask for inserted row back
      req_body_json(data) |>
      req_method("POST") |>
      req_perform()

    result <- resp_body_json(resp)
    list(success = TRUE, id = result[[1]]$id, data = result[[1]])
  }, error = function(e) {
    message("Supabase insert error: ", conditionMessage(e))
    list(success = FALSE, error = conditionMessage(e))
  })
}

# ---- Select rows from a table ----
# 'filters' is a named character vector, e.g. c("game_id" = "eq.5", "active" = "eq.true")
# Returns a data frame (empty data frame on error)

sb_select <- function(table, select = "*", filters = NULL, order = NULL, limit = NULL) {
  tryCatch({
    params <- paste0("select=", select)

    if (!is.null(filters)) {
      for (col in names(filters)) {
        params <- paste0(params, "&", col, "=", filters[[col]])
      }
    }
    if (!is.null(order))  params <- paste0(params, "&order=", order)
    if (!is.null(limit))  params <- paste0(params, "&limit=", limit)

    resp <- .sb_base_req(paste0(table, "?", params)) |>
      req_perform()
    
    result <- resp_body_json(resp, simplifyVector = TRUE)
    
    # jsonlite's simplifyVector returns a plain empty list() for a JSON
    # empty array `[]` (zero matching rows), instead of a zero-row data
    # frame like it does for non-empty results. Every caller in this app
    # assumes sb_select() always returns a data frame, so normalize here
    # once rather than defensively checking in every single caller.
    if (!is.data.frame(result)) {
      return(data.frame())
    }
    
    result
  }, error = function(e) {
    message("Supabase select error: ", conditionMessage(e))
    data.frame()
  })
}

# ---- Delete a record by id ----
# Used for the "undo last entry" feature

sb_delete <- function(table, id) {
  tryCatch({
    resp <- .sb_base_req(paste0(table, "?id=eq.", id)) |>
      req_method("DELETE") |>
      req_perform()
    list(success = resp_status(resp) %in% c(200, 204))
  }, error = function(e) {
    list(success = FALSE, error = conditionMessage(e))
  })
}

# ---- Convenience: load players as a named vector for selectInput ----
# Returns c("# 10 John Smith (F)" = 1, ...) sorted by jersey number

sb_load_players <- function() {
  df <- sb_select(
    "players",
    select  = "id,jersey_num,full_name,position",
    filters = c("active" = "eq.true"),
    order   = "jersey_num.asc"
  )
  if (nrow(df) == 0) return(setNames(character(0), character(0)))
  labels <- paste0("#", df$jersey_num, "  ", df$full_name, " (", df$position, ")")
  setNames(as.character(df$id), labels)
}

# ---- Convenience: load recent games as a named vector for selectInput ----

sb_load_games <- function() {
  df <- sb_select(
    "games",
    select = "id,game_date,opponent,location",
    order  = "game_date.desc",
    limit  = 20
  )
  if (nrow(df) == 0) return(setNames(character(0), character(0)))
  labels <- paste0(df$game_date, "  vs  ", df$opponent,
                   "  (", df$location, ")")
  setNames(as.character(df$id), labels)
}

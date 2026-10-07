#' Create a lazy DBI query source
#'
#' The source implements [table_source()] and fetches bounded pages for a
#' query result. The built-in pager supports RSQLite only. Other DBI drivers
#' can provide an `adapter` list with `column_names(connection, query,
#' params)`, `row_count(connection, query, params)`, and `get_rows(connection,
#' query, params, start, count, columns)` callbacks. This keeps dialect-specific
#' pagination in the backend adapter rather than assuming all DBI drivers use
#' the same SQL.
#'
#' The built-in SQLite adapter wraps the query as a subquery, runs one cached
#' `COUNT(*)`, and adds bound `LIMIT`/`OFFSET` values to page requests. A single
#' trailing semicolon is removed before wrapping. The query must produce one
#' result set; arbitrary SQL statements are not parsed or rewritten. The row
#' count can be expensive, but it is computed once at source creation and on
#' explicit refresh, never on repaint.
#'
#' The returned source is read-only. Sorting, filtering, and search are not
#' pushed into the user query.
#'
#' @param connection A [db_connection()] wrapper or open DBI connection.
#'   It remains owned by its caller; closing this source never disconnects it.
#' @param query A single query that returns a result set.
#' @param params Optional DBI parameter list passed to the query.
#' @param adapter Optional backend adapter callbacks. `NULL` selects the
#'   built-in RSQLite pager when `connection` is an RSQLite connection.
#' @return An experimental [table_source()] accepted directly by
#'   [data_table()].
#' @export
db_query_source <- function(connection, query, params = NULL, adapter = NULL) {
  if (!requireNamespace("DBI", quietly = TRUE)) {
    stop("Database support requires the DBI package.", call. = FALSE)
  }
  check_scalar_character(query, "query")
  query <- trimws(query)
  query <- sub(";\\s*$", "", query, perl = TRUE)
  if (!nzchar(query)) stop("`query` must contain a result-set query.", call. = FALSE)
  if (!is.null(params) && !is.list(params)) {
    stop("`params` must be NULL or a list of DBI query parameters.", call. = FALSE)
  }

  wrapped <- inherits(connection, "TermrDBConnection")
  if (wrapped) {
    if (!connection$is_valid()) stop("The database connection is not valid.", call. = FALSE)
    con <- connection$con
    connection_valid <- function() connection$is_valid()
  } else {
    con <- connection
    valid <- tryCatch(isTRUE(DBI::dbIsValid(con)), error = function(e) FALSE)
    if (!valid) stop("`connection` must be an open DBI connection.", call. = FALSE)
    connection_valid <- function() tryCatch(isTRUE(DBI::dbIsValid(con)), error = function(e) FALSE)
  }

  if (is.null(adapter)) {
    if (!inherits(con, "SQLiteConnection") || !requireNamespace("RSQLite", quietly = TRUE)) {
      stop(paste0("No safe lazy query pager is available for this DBI driver. ",
                  "Use the built-in SQLite adapter or supply `adapter` callbacks."), call. = FALSE)
    }
    adapter <- sqlite_query_adapter()
  }
  required <- c("column_names", "row_count", "get_rows")
  if (!is.list(adapter) || !all(required %in% names(adapter)) ||
      !all(vapply(adapter[required], is.function, logical(1)))) {
    stop("`adapter` must provide `column_names()`, `row_count()`, and `get_rows()` functions.",
         call. = FALSE)
  }

  state <- new.env(parent = emptyenv())
  state$closed <- FALSE
  state$error <- NULL
  check_source <- function() {
    if (state$closed) stop("This database query source is closed.", call. = FALSE)
    if (!connection_valid()) stop("The database connection is no longer valid.", call. = FALSE)
    invisible(TRUE)
  }
  safe_call <- function(fun, ...) {
    check_source()
    fun(con, query, params, ...)
  }
  columns <- safe_call(adapter$column_names)
  if (!is.character(columns) || !length(columns) || anyNA(columns) ||
      any(!nzchar(columns)) || anyDuplicated(columns)) {
    stop("The query pager returned invalid or duplicate column names.", call. = FALSE)
  }
  count <- safe_call(adapter$row_count)
  if (!is.numeric(count) || length(count) != 1L || is.na(count) || !is.finite(count) ||
      count < 0 || count != floor(count) || count > .Machine$integer.max) {
    stop("The query pager returned an invalid row count.", call. = FALSE)
  }
  state$count <- as.numeric(count)
  state$columns <- columns

  source <- table_source(
    row_count = function() state$count,
    column_names = columns,
    get_rows = function(start, count, columns = NULL) {
      check_source()
      selected <- if (is.null(columns)) state$columns else columns
      if (!all(selected %in% state$columns)) {
        stop("The query source was asked for an unknown column.", call. = FALSE)
      }
      rows <- adapter$get_rows(con, query, params, start, count, selected)
      state$error <- NULL
      rows
    },
    refresh = function() {
      check_source()
      state$count <- as.numeric(safe_call(adapter$row_count))
    },
    close = function() {
      state$closed <- TRUE
      invisible(NULL)
    },
    on_error = function(error, start, count, columns) {
      state$error <- conditionMessage(error)
      as.data.frame(stats::setNames(rep(list(rep(NA_character_, count)), length(columns)), columns),
                    check.names = FALSE, stringsAsFactors = FALSE)
    }
  )
  source$query <- query
  source$last_error <- function() state$error
  source
}

sqlite_query_adapter <- function() {
  get_query <- function(connection, sql, params) {
    if (is.null(params) || !length(params)) DBI::dbGetQuery(connection, sql) else
      DBI::dbGetQuery(connection, sql, params = params)
  }
  quote_columns <- function(connection, columns) {
    vapply(columns, function(name) as.character(DBI::dbQuoteIdentifier(connection, name)), "")
  }
  list(
    column_names = function(connection, query, params) {
      sql <- paste0("SELECT * FROM (", query, ") AS termr_query LIMIT 0")
      names(get_query(connection, sql, params))
    },
    row_count = function(connection, query, params) {
      sql <- paste0("SELECT COUNT(*) AS n FROM (", query, ") AS termr_query")
      result <- get_query(connection, sql, params)
      as.numeric(result$n[[1L]])
    },
    get_rows = function(connection, query, params, start, count, columns) {
      selected <- quote_columns(connection, columns)
      sql <- paste0("SELECT ", paste(selected, collapse = ", "), " FROM (",
                    query, ") AS termr_query LIMIT ? OFFSET ?")
      page_params <- c(params %||% list(), list(as.integer(count), as.integer(start - 1L)))
      get_query(connection, sql, page_params)
    }
  )
}

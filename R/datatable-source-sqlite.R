#' Create a lazy source for a SQLite table
#'
#' This optional adapter uses bounded `LIMIT`/`OFFSET` queries and
#' parameterized filter values. It supports `equals`, `contains`, `range`, and
#' `missing` [table_filter()] filters and sorting by quoted column names.
#' Arbitrary queries and other SQL dialects are intentionally outside this
#' adapter; custom [table_source()] callbacks can implement driver-specific
#' pagination.
#'
#' @param connection An open DBI connection created by RSQLite.
#' @param table A table name or `DBI::Id`.
#' @param own_connection If `TRUE`, `source$close()` disconnects this
#'   connection. The default leaves connection ownership with the caller.
#' @return An experimental table source accepted by [data_table()].
#' @export
db_table_source <- function(connection, table, own_connection = FALSE) {
  check_flag(own_connection)
  if (!requireNamespace("DBI", quietly = TRUE) || !requireNamespace("RSQLite", quietly = TRUE)) {
    stop("`db_table_source()` requires the optional DBI and RSQLite packages.", call. = FALSE)
  }
  if (!inherits(connection, "SQLiteConnection") || !DBI::dbIsValid(connection)) {
    stop("`connection` must be an open RSQLite connection.", call. = FALSE)
  }
  table_sql <- as.character(DBI::dbQuoteIdentifier(connection, table))
  field_names <- DBI::dbListFields(connection, table)
  if (!length(field_names) || anyNA(field_names) || any(!nzchar(field_names)) || anyDuplicated(field_names)) {
    stop("The SQLite table must have unique, non-empty column names.", call. = FALSE)
  }
  quoted <- stats::setNames(vapply(field_names, function(x) as.character(DBI::dbQuoteIdentifier(connection, x)), ""), field_names)
  state <- new.env(parent = emptyenv())
  state$filters <- list()
  state$sort <- NULL
  make_where <- function(filters) {
    if (!length(filters)) return(list(sql = "", params = list()))
    clauses <- character()
    params <- list()
    for (name in names(filters)) {
      if (!(name %in% field_names)) stop(sprintf("Unknown SQLite filter column `%s`.", name), call. = FALSE)
      f <- filters[[name]]
      if (!inherits(f, "termr_table_filter")) {
        stop(sprintf("SQLite sources cannot translate function filter for `%s`; use `table_filter()`.", name), call. = FALSE)
      }
      qname <- quoted[[name]]
      if (f$type == "missing") {
        clauses <- c(clauses, paste0(qname, " IS NULL"))
      } else if (f$type == "equals") {
        if (length(f$value) == 1L && is.na(f$value)) {
          clauses <- c(clauses, paste0(qname, " IS NULL"))
        } else {
          clauses <- c(clauses, paste0(qname, " = ?"))
          params[[length(params) + 1L]] <- f$value
        }
      } else if (f$type == "contains") {
        lhs <- if (isTRUE(f$case_sensitive)) qname else paste0("lower(", qname, ")")
        rhs <- if (isTRUE(f$case_sensitive)) "?" else "lower(?)"
        clauses <- c(clauses, paste0("instr(", lhs, ", ", rhs, ") > 0"))
        params[[length(params) + 1L]] <- f$value
      } else if (f$type == "range") {
        lop <- if (f$inclusive) ">=" else ">"
        hip <- if (f$inclusive) "<=" else "<"
        clauses <- c(clauses, paste0("(", qname, " ", lop, " ? AND ", qname, " ", hip, " ? )"))
        params[[length(params) + 1L]] <- f$min
        params[[length(params) + 1L]] <- f$max
      } else {
        stop(sprintf("SQLite sources do not support `%s` filters.", f$type), call. = FALSE)
      }
    }
    list(sql = paste("WHERE", paste(clauses, collapse = " AND ")), params = params)
  }
  count_rows <- function(filters) {
    where <- make_where(filters)
    result <- DBI::dbGetQuery(connection, paste("SELECT COUNT(*) AS n FROM", table_sql, where$sql), params = where$params)
    as.numeric(result$n[[1L]])
  }
  state$count <- count_rows(state$filters)
  build_order <- function(sort) {
    if (is.null(sort)) return("")
    if (!all(sort$columns %in% field_names)) stop("SQLite sort refers to an unknown column.", call. = FALSE)
    paste("ORDER BY", paste(vapply(seq_along(sort$columns), function(i) {
      paste(quoted[[sort$columns[[i]]]], if (sort$decreasing[[i]]) "DESC" else "ASC")
    }, ""), collapse = ", "))
  }
  table_source(
    row_count = function() state$count,
    column_names = field_names,
    get_rows = function(start, count, columns = NULL) {
      selected <- columns %||% names(quoted)
      if (!all(selected %in% names(quoted))) stop("Requested an unknown SQLite column.", call. = FALSE)
      where <- make_where(state$filters)
      sql <- paste("SELECT", paste(unname(quoted[selected]), collapse = ","), "FROM", table_sql,
                   where$sql, build_order(state$sort), "LIMIT ? OFFSET ?")
      DBI::dbGetQuery(connection, sql, params = c(where$params, list(as.integer(count), as.integer(start - 1L))))
    },
    sort = function(spec) {
      if (!is.null(spec) && !all(spec$columns %in% field_names)) stop("SQLite sort refers to an unknown column.", call. = FALSE)
      state$sort <- spec
      TRUE
    },
    filter = function(filters) {
      # Validate and parameterize before changing state.
      make_where(filters %||% list())
      state$filters <- filters %||% list()
      state$count <- count_rows(state$filters)
      TRUE
    },
    refresh = function() state$count <- count_rows(state$filters),
    close = if (own_connection) function() DBI::dbDisconnect(connection) else NULL
  )
}

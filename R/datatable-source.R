#' Create an experimental lazy table source
#'
#' A table source lets [data_table()] request only the rows it needs. All
#' callbacks are synchronous. `get_rows()` must return a data frame with the
#' requested columns in the requested order. Row counts are known.
#'
#' Optional `sort` and `filter` callbacks change the source's current view.
#' They receive the sort keys or the named filter list used by
#' `DataTable$sort()` and `DataTable$filter_columns()`. Returning `FALSE`
#' rejects the operation; returning `NULL` or `TRUE` accepts it. A source
#' should expose only operations it can implement without materializing its
#' entire data set.
#'
#' `search` receives a parsed query, column names, `start`, `direction`, and
#' `include_current`; it returns `NULL` or `list(position, column)`. `row_key`
#' maps view positions to stable row identities. `set_value(row, column,
#' value)` receives a current view position. `refresh` and `close` are
#' optional lifecycle callbacks. `on_error(error, start, count, columns)` may
#' return a correctly shaped placeholder data frame when a page read fails;
#' the table then displays the error instead of aborting a paint. Closing a
#' source is explicit and never closes resources the source does not own.
#'
#' @param row_count Function returning the current number of rows, or a
#'   non-negative scalar count.
#' @param column_names Function returning unique, non-empty names, or a
#'   character vector.
#' @param get_rows Function `function(start, count, columns = NULL)`.
#' @param sort,filter,search Optional delegated operations.
#' @param row_key Optional function mapping view positions to row keys.
#' @param set_value Optional function `(row, column, value)` for editing.
#' @param refresh,close Optional functions called by `$refresh()` and
#'   `$close()`.
#' @param on_error Optional function returning a page-shaped placeholder when
#'   `get_rows()` fails. It receives the original condition and requested page.
#' @return An experimental `termr_table_source` object.
#' @export
table_source <- function(row_count, column_names, get_rows, sort = NULL,
                         filter = NULL, search = NULL, row_key = NULL,
                         set_value = NULL, refresh = NULL, close = NULL,
                         on_error = NULL) {
  as_count <- function(x, what) {
    if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
        x < 0 || x != floor(x) || x > .Machine$integer.max) {
      stop(sprintf("`%s` must be a non-negative whole number no larger than %s.",
                   what, .Machine$integer.max), call. = FALSE)
    }
    as.integer(x)
  }
  if (!is.function(row_count) && !is.null(row_count)) {
    fixed_rows <- as_count(row_count, "row_count")
    row_count <- function() fixed_rows
  }
  if (!is.function(column_names) && !is.null(column_names)) {
    fixed_names <- column_names
    column_names <- function() fixed_names
  }
  if (is.null(row_count) || is.null(column_names) || !is.function(get_rows)) {
    stop("`row_count`, `column_names`, and `get_rows` are required.", call. = FALSE)
  }
  callbacks <- list(sort = sort, filter = filter, search = search,
                    row_key = row_key, set_value = set_value,
                    refresh = refresh, close = close, on_error = on_error)
  bad <- names(callbacks)[!vapply(callbacks, function(x) is.null(x) || is.function(x), TRUE)]
  if (length(bad)) stop(sprintf("`%s` must be a function or NULL.", bad[[1L]]), call. = FALSE)
  source <- c(list(row_count = row_count, column_names = column_names,
                   get_rows = get_rows), callbacks)
  source$capabilities <- function() list(
    sortable = !is.null(sort), filterable = !is.null(filter),
    searchable = !is.null(search), editable = !is.null(set_value),
    row_count_known = TRUE
  )
  structure(source,
            class = "termr_table_source")
}

is_table_source <- function(x) inherits(x, "termr_table_source")


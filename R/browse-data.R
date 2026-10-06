#' Browse a data frame in the terminal
#'
#' `browse_data()` opens an interactive viewer for a data frame: summary
#' metrics, a virtualised table (fast for millions of rows), information
#' about the selected column, search across all columns, sorting, and row
#' details. `data_browser()` builds the same app without running it, e.g.
#' to customise or test it.
#'
#' Keys: arrows / Page Up / Page Down move the cell cursor; `/` focuses the
#' search box (Enter applies it, Escape in the box clears it); `s` sorts by
#' the current column (again: descending); `r` resets sorting and search;
#' Enter shows the row; `q` quits.
#'
#' @param data A data frame or matrix.
#' @param title Title shown at the top (defaults to the expression).
#' @param ... Passed to [data_table()] (e.g. `formatters`, `columns`).
#' @return `browse_data()` returns `NULL` invisibly; `data_browser()`
#'   returns an [App].
#' @export
#' @examples
#' viewer <- data_browser(mtcars)
#' pilot <- test_app(viewer, 100, 30)
#' pilot$press("s")
#' pilot$stop()
#' if (interactive()) browse_data(mtcars)
browse_data <- function(data, title = deparse(substitute(data)), ...) {
  force(title)
  run(data_browser(data, title = title, ...))
  invisible(NULL)
}

#' @rdname browse_data
#' @export
data_browser <- function(data, title = deparse(substitute(data)), ...) {
  force(title)
  data <- as_table_data(data)
  title <- paste(title, collapse = " ")
  n_missing <- sum(vapply(data, function(col) sum(is.na(col)), numeric(1)))
  table <- data_table(data, id = "table", cursor = "cell", zebra = TRUE, ...)
  sort_state <- new.env()
  sort_state$column <- NULL
  sort_state$decreasing <- FALSE

  status <- label("", id = "status", style = style(foreground = "$muted"))
  info <- key_value(list(), id = "column-info")
  search <- input(placeholder = "Search all columns (Enter)", id = "search")

  show_column <- function(app) {
    j <- table$cursor_column
    if (j < 1L) return(invisible())
    info$data <- column_summary(data[[j]], names(data)[[j]])
    app$query_one("#info-panel")$set(border_title = names(data)[[j]])
  }
  show_status <- function(app) {
    sorting <- if (is.null(sort_state$column)) "" else
      sprintf("  sorted by %s%s", sort_state$column, if (sort_state$decreasing) " (desc)" else "")
    status$update(sprintf("%s of %s rows%s", format(table$row_count, big.mark = ","),
                          format(nrow(data), big.mark = ","), sorting))
  }

  ui <- vertical(
    label(title, style = style(bold = TRUE, foreground = "$accent")),
    horizontal(
      metric("Rows", nrow(data)),
      metric("Columns", ncol(data)),
      metric("Missing", n_missing),
      style = style(height = "auto")
    ),
    search,
    horizontal(
      table,
      panel(info, title = "Column", id = "info-panel", style = style(width = 34, height = "1fr")),
      style = style(height = "1fr")
    ),
    status,
    label("/ search  s sort  r reset  Enter row  Ctrl+P commands  q quit", style = style(foreground = "$muted")),
    style = style(padding = c(0, 1))
  )

  app(
    ui,
    title = title,
    bind("q", "quit", "Quit"),
    bind("/", function(app) app$query_one("#search")$focus(), "Search"),
    bind("s", function(app) {
      j <- max(1L, table$cursor_column)
      name <- names(data)[[j]]
      sort_state$decreasing <- identical(sort_state$column, name) && !sort_state$decreasing
      sort_state$column <- name
      table$sort(name, decreasing = sort_state$decreasing)
      show_status(app)
    }, "Sort by column"),
    bind("r", function(app) {
      sort_state$column <- NULL
      search$clear()
      table$clear_sort()
      table$filter(NULL)
      show_status(app)
    }, "Reset"),
    on("mount", "#status", function(event, app) {
      show_status(app)
      show_column(app)
      table$focus()
    }),
    on("datatable.cell_selected", "#table", function(event, app) show_column(app)),
    on("datatable.row_activated", "#table", function(event, app) {
      row <- event$data$row
      values <- lapply(data[row, , drop = FALSE], function(v) format_metric(v))
      app$push_screen(modal(key_value(values), title = paste("Row", row), width = 60))
    }),
    on("input.submitted", "#search", function(event, app) {
      table$filter(search_rows(data, event$data$value))
      show_status(app)
      table$focus()
    }),
    on("key", "#search", function(event, app) {
      if (event$key == "escape") {
        search$clear()
        table$filter(NULL)
        show_status(app)
        table$focus()
        event$stop()
      }
    })
  )
}

# Rows of `data` where any column contains `query` (case-insensitive).
search_rows <- function(data, query) {
  query <- trimws(query)
  if (!nzchar(query)) return(NULL)
  q <- tolower(query)
  hit <- rep(FALSE, nrow(data))
  for (col in data) {
    text <- if (is.factor(col)) as.character(col) else format(col, trim = TRUE)
    hit <- hit | grepl(q, tolower(text), fixed = TRUE)
  }
  which(hit)
}

column_summary <- function(x, name) {
  out <- list(
    Type = paste(class(x), collapse = "/"),
    Missing = sum(is.na(x)),
    Unique = length(unique(x))
  )
  if (is.numeric(x) && any(!is.na(x))) {
    q <- stats_quantiles(x)
    out <- c(out, list(Min = q[[1]], Median = q[[2]], Mean = mean(x, na.rm = TRUE), Max = q[[3]]))
  } else if (length(x)) {
    counts <- sort(table(x, useNA = "no"), decreasing = TRUE)
    top <- utils::head(counts, 3)
    for (k in seq_along(top)) out[[paste0("Top ", k)]] <- sprintf("%s (%d)", names(top)[[k]], top[[k]])
  }
  out
}

stats_quantiles <- function(x) {
  x <- sort(x[!is.na(x)])
  c(x[[1]], x[[ceiling(length(x) / 2)]], x[[length(x)]])
}

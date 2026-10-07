# Structural and timing profile for DataTable's eager and lazy paths.
# Run from the package root: Rscript tools/bench/datatable-source.R

suppressMessages(pkgload::load_all(path = normalizePath("."), quiet = TRUE))

timer <- function(expr) unname(system.time(force(expr))["elapsed"] * 1000)

profile_frame <- function(n) {
  data <- data.frame(id = seq_len(n), group = rep(letters, length.out = n),
                     value = as.double(seq_len(n)) / 7, flag = seq_len(n) %% 2L == 0L)
  invisible(render_widget(data_table(data, cursor = "none"), 120, 40))
  gc()
  elapsed <- median(replicate(3L, timer({
    t <- data_table(data, cursor = "none")
    render_widget(t, 120, 40)
  })))
  data.frame(scenario = paste0("data.frame-", format(n, scientific = FALSE), "-initial"),
    rows = n, viewport = 39L, fetch_calls = NA_integer_, rows_fetched = NA_integer_,
    cache_hits = NA_integer_, cache_misses = NA_integer_, rows_rendered = NA_integer_,
    cells_rendered = NA_integer_, median_ms = round(elapsed, 2), stringsAsFactors = FALSE)
}

profile_wide <- function(n = 100000L, columns = 100L) {
  data <- as.data.frame(replicate(columns, seq_len(n), simplify = FALSE))
  invisible(render_widget(data_table(data, cursor = "none"), 120, 40))
  gc()
  elapsed <- median(replicate(3L, timer({
    t <- data_table(data, cursor = "none")
    render_widget(t, 120, 40)
  })))
  data.frame(scenario = paste0("data.frame-wide-", n, "x", columns), rows = n,
    viewport = 39L, fetch_calls = NA_integer_, rows_fetched = NA_integer_,
    cache_hits = NA_integer_, cache_misses = NA_integer_, rows_rendered = NA_integer_,
    cells_rendered = NA_integer_, median_ms = round(elapsed, 2), stringsAsFactors = FALSE)
}

synthetic_source <- function(n) {
  state <- new.env(parent = emptyenv())
  state$reverse <- FALSE
  state$even <- FALSE
  row_count <- function() if (state$even) n %/% 2L else n
  map_rows <- function(pos) {
    count <- row_count()
    if (state$reverse) pos <- count - pos + 1
    if (state$even) pos * 2 else pos
  }
  table_source(
    row_count = row_count,
    column_names = c("id", "group", "value"),
    get_rows = function(start, count, columns = NULL) {
      size <- row_count()
      take <- if (start > size) 0L else min(count, size - start + 1L)
      ids <- if (take) as.double(map_rows(seq.int(start, length.out = take))) else numeric()
      out <- data.frame(id = ids, group = ifelse(ids %% 2 == 0, "even", "odd"), value = ids / 7)
      if (!is.null(columns)) out <- out[columns]
      out
    },
    sort = function(spec) {
      state$reverse <- !is.null(spec) && isTRUE(spec$decreasing[[1L]])
      TRUE
    },
    search = function(query, columns, start, direction, include_current) {
      if (!("id" %in% columns) || !grepl("^[0-9]+$", query$pattern)) return(NULL)
      target <- as.numeric(query$pattern)
      if (state$even && target %% 2 != 0) return(NULL)
      pos <- if (state$even) target / 2 else target
      if (state$reverse) pos <- row_count() - pos + 1
      if (pos < 1 || pos > row_count()) return(NULL)
      if (direction > 0 && pos < start && !include_current) return(NULL)
      if (direction < 0 && pos > start && !include_current) return(NULL)
      list(position = as.integer(pos), column = "id")
    },
    filter = function(filters) {
      if (is.null(filters)) state$even <- FALSE else {
        f <- filters[["group"]]
        state$even <- inherits(f, "termr_table_filter") && identical(f$type, "equals") && identical(f$value, "even")
      }
      TRUE
    }
  )
}

profile_lazy <- function(n, action = "initial") {
  gc()
  runs <- replicate(3L, {
    t <- NULL
    elapsed <- timer({
    t <- data_table(synthetic_source(n), cursor = "none")
    render_widget(t, 120, 40)
    if (action == "scroll+1") t$scroll_to_row(2L)
    if (action == "page-down") t$scroll_to_row(40L)
    if (action == "middle") t$scroll_to_row(n %/% 2L)
    if (action == "end") t$scroll_to_row(n)
    if (action == "sort") t$sort("id", decreasing = TRUE)
    if (action == "filter") t$filter_columns(list(group = table_filter("equals", "even")))
    if (action == "search") t$find(as.character(n %/% 2L), column = "id")
    if (action == "resize") t$set_column_width("id", 12L)
    if (action == "hide-column") t$set_column_visible("group", FALSE)
    if (action != "initial") render_widget(t, 120, 40)
    })
    list(table = t, elapsed = elapsed)
  }, simplify = FALSE)
  t <- runs[[length(runs)]]$table
  elapsed <- median(vapply(runs, `[[`, 0, "elapsed"))
  stats <- t$source_stats()
  data.frame(scenario = paste0("lazy-", format(n, scientific = FALSE), "-", action),
    rows = t$row_count, viewport = 39L, fetch_calls = stats$fetch_calls,
    rows_fetched = stats$rows_requested, cache_hits = stats$cache_hits,
    cache_misses = stats$cache_misses, rows_rendered = stats$rows_rendered,
    cells_rendered = stats$cells_rendered,
    median_ms = round(elapsed, 2), stringsAsFactors = FALSE)
}

results <- do.call(rbind, c(
  lapply(c(1e5, 1e6), profile_frame),
  list(profile_wide()),
  lapply(c("initial", "scroll+1", "page-down", "middle", "end", "sort", "filter", "search", "resize", "hide-column"),
    function(action) profile_lazy(1e7, action)),
  lapply(c("initial", "middle"), function(action) profile_lazy(1e8, action))
))
print(results, row.names = FALSE)
cat("\nMedian elapsed is over three fresh runs; structural counters come from\n",
    "the final run and do not depend on wall-clock precision.\n", sep = "")

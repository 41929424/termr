# DataTable: a virtualised, read-only viewer for data frames.
#
# Only the rows and columns that are visible are formatted and painted, so
# the cost of a repaint depends on the size of the viewport, not of the
# data. Column widths are estimated once from a sample (the first and last
# rows), never from the whole column.
#
# For data frames the table stores a view as row indices (all rows by
# default). A lazy source owns its current ordered/filtered view and DataTable
# uses positions into it. The cursor and scroll offsets are view positions.
# Events report both the position and the data-frame row number or source key.

#' Column options for data tables
#'
#' @param label Header text (defaults to the column name).
#' @param width Fixed width in cells; `NULL` estimates it from the data.
#' @param align `"left"`, `"right"` or `"center"`; numbers default to right.
#' @param formatter `function(x)` turning a vector of values into strings.
#' @param min_width,max_width Limits for the estimated width.
#' @param visible Show the column? (Hidden columns stay in the data; see
#'   `set_column_visible()`.)
#' @param sortable Can the table be sorted by this column?
#' @param editable Can cells be edited (when the table is `editable`)?
#'   `NULL`: yes for numeric, logical, character, factor and date columns.
#' @return An object of class `termr_column`.
#' @export
#' @examples
#' column(label = "Score", width = 8, align = "right",
#'        formatter = function(x) sprintf("%.1f", x))
column <- function(label = NULL, width = NULL, align = NULL, formatter = NULL,
                   min_width = NULL, max_width = NULL, visible = TRUE, sortable = TRUE,
                   editable = NULL) {
  check_scalar_character(label, "label", allow_null = TRUE)
  check_function(formatter, "formatter", allow_null = TRUE)
  if (is.character(width) && identical(width, "auto")) width <- NULL
  check_flag(visible)
  check_flag(sortable)
  if (!is.null(editable)) check_flag(editable)
  structure(
    list(
      label = label,
      width = check_count(width, "width"),
      align = check_choice(align, c("left", "right", "center"), "align"),
      formatter = formatter,
      min_width = check_count(min_width, "min_width"),
      max_width = check_count(max_width, "max_width"),
      visible = visible,
      sortable = sortable,
      editable = editable
    ),
    class = "termr_column"
  )
}

#' Create a DataTable column filter
#'
#' Filters combine with AND in `filter_columns()`. Predicates can
#' also be supplied directly as `function(values) logical`.
#' @param type One of `"equals"`, `"contains"`, `"regex"`, `"range"`, or
#'   `"missing"`.
#' @param value Comparison value, text pattern, or regular expression.
#' @param min,max Inclusive or exclusive range bounds.
#' @param case_sensitive Case-sensitive text matching?
#' @param inclusive Include range endpoints?
#' @return An object suitable for a named `filter_columns()` list.
#' @export
table_filter <- function(type = c("equals", "contains", "regex", "range", "missing"),
                         value = NULL, min = NULL, max = NULL,
                         case_sensitive = FALSE, inclusive = TRUE) {
  type <- match.arg(type)
  check_flag(case_sensitive)
  check_flag(inclusive)
  if (type == "equals" && length(value) != 1L) stop("Equality filters need one `value`.", call. = FALSE)
  if (type %in% c("contains", "regex") && (!is.character(value) || length(value) != 1L || is.na(value))) {
    stop("Text filters need one non-missing character `value`.", call. = FALSE)
  }
  if (type == "range" && (is.null(min) || is.null(max) || length(min) != 1L || length(max) != 1L ||
      is.na(min) || is.na(max) || min > max)) stop("Range filters need ordered `min` and `max` values.", call. = FALSE)
  if (type == "regex") search_query(value, case_sensitive = case_sensitive, regex = TRUE)
  structure(list(type = type, value = value, min = min, max = max,
                 case_sensitive = case_sensitive, inclusive = inclusive), class = "termr_table_filter")
}

#' @title DataTable widget
#' @description A virtualised table for data frames or lazy table sources. See [data_table()].
#' @rdname DataTable-class
#' @export
DataTable <- R6::R6Class(
  "DataTable",
  inherit = Widget,
  public = list(
    #' @field paint_states Cursor and scroll changes only repaint.
    paint_states = c("cursor_row", "cursor_col", "offset_row", "offset_col", "active_col"),
    #' @field focusable Tables can be focused.
    focusable = TRUE,
    #' @field cursor_type `"row"`, `"cell"` or `"none"`.
    cursor_type = "row",
    #' @field show_header Show the header row?
    show_header = TRUE,
    #' @field zebra Stripe alternate rows?
    zebra = FALSE,
    #' @field row_style `NULL` or `function(row, data)` returning a style for
    #'   a data row (`row` is the original row number or source position).
    row_style = NULL,
    #' @field cell_style `NULL` or `function(value, row, column)` returning a
    #'   style for a cell.
    cell_style = NULL,
    #' @field frozen_columns Number of leading columns that stay visible
    #'   while the others scroll horizontally.
    frozen_columns = 0L,
    #' @field header_sort Clicking a header sorts by that column (click again:
    #'   descending, then unsorted; Shift+click adds a column)?
    header_sort = FALSE,
    #' @field editable Allow editing cells (Enter / double click)?
    editable = FALSE,
    #' @field on_edit `NULL` or `function(row, column, value)` called before a
    #'   cell edit is applied; return `NULL` / `TRUE` to accept or a message
    #'   to reject the edit.
    on_edit = NULL,

    #' @description Create a table. See [data_table()].
    #' @param data A data frame, matrix, or [table_source()].
    #' @param cursor Cursor type.
    #' @param columns Named list of [column()] options.
    #' @param formatters Named list of formatter functions.
    #' @param row_names Show row names? `NA`: only when they are not 1..n.
    #' @param header Show the header?
    #' @param zebra Stripe rows?
    #' @param row_style,cell_style Style hooks.
    #' @param max_column_width Upper limit for estimated widths.
    #' @param frozen_columns,header_sort,editable,on_edit See the fields.
    #' @param filters Named column filters for `filter_columns()`.
    #' @param id,classes,style,disabled See [Widget].
    initialize = function(data, cursor = "row", columns = NULL, formatters = NULL, row_names = NA,
                          header = TRUE, zebra = FALSE, row_style = NULL, cell_style = NULL,
                          max_column_width = 40L, frozen_columns = 0L, header_sort = FALSE,
                          editable = FALSE, on_edit = NULL, id = NULL, classes = NULL, style = NULL,
                          disabled = FALSE) {
      super$initialize(id = id, classes = classes, style = style, disabled = disabled)
      check_flag(header_sort)
      check_flag(editable)
      check_function(on_edit, "on_edit", allow_null = TRUE)
      self$frozen_columns <- check_count(frozen_columns, "frozen_columns")
      self$header_sort <- header_sort
      self$editable <- editable
      self$on_edit <- on_edit
      self$cursor_type <- check_choice(cursor, c("row", "cell", "none"), "cursor")
      check_flag(header)
      check_flag(zebra)
      check_function(row_style, "row_style", allow_null = TRUE)
      check_function(cell_style, "cell_style", allow_null = TRUE)
      if (!(is.logical(row_names) && length(row_names) == 1L)) {
        stop("`row_names` must be TRUE, FALSE or NA.", call. = FALSE)
      }
      self$show_header <- header
      self$zebra <- zebra
      self$row_style <- row_style
      self$cell_style <- cell_style
      private$.options <- list(
        columns = check_named_list(columns, "columns"),
        formatters = check_named_functions(formatters, "formatters"),
        row_names = row_names,
        max_width = check_count(max_column_width, "max_column_width")
      )
      for (opt in private$.options$columns) {
        if (!inherits(opt, "termr_column")) stop("`columns` must contain column() options.", call. = FALSE)
      }
      self$set_data(data)
    },

    #' @description The built-in style.
    default_style = function() {
      style(width = "1fr", height = "1fr", focus = style(), disabled = style(foreground = "$muted"))
    },

    #' @description Navigation keys and Enter.
    default_bindings = function() {
      c(list(
        bind("up", "cursor_up"), bind("down", "cursor_down"),
        bind("left", "cursor_left"), bind("right", "cursor_right"),
        bind("pageup", "page_up"), bind("pagedown", "page_down"),
        bind("home,ctrl+home", "first_row"), bind("end,ctrl+end", "last_row"),
        bind("enter", "activate", "Open"),
        bind("ctrl+c", "copy_selection", "Copy rows"),
        bind("ctrl+f", "open_find", "Find"), bind("ctrl+g", "open_goto", "Go to row"),
        bind("f3", "find_next"), bind("shift+f3", "find_previous"),
        bind("shift+up", "extend_selection_up"), bind("shift+down", "extend_selection_down"),
        bind("ctrl+alt+left", "move_column_left"), bind("ctrl+alt+right", "move_column_right"),
        bind("alt+right", "widen_column"), bind("alt+left", "narrow_column")
      ), if (self$header_sort) list(bind("s", "sort_column", "Sort")))
    },

    #' @description Replace the data. Resets the view, cursor and scroll.
    #' @param data A data frame, matrix, or [table_source()].
    set_data = function(data) {
      opts <- private$.options
      private$.source_error <- NULL
      if (is_table_source(data)) {
        private$.source <- data
        private$.source_closed <- FALSE
        private$.source_cache <- new.env(parent = emptyenv())
        private$.source_lru <- character()
        private$.source_stats <- new.env(parent = emptyenv())
        private$.source_stats$fetch_calls <- 0L
        private$.source_stats$rows_requested <- 0L
        private$.source_stats$cache_hits <- 0L
        private$.source_stats$cache_misses <- 0L
        private$.source_stats$rows_rendered <- 0L
        private$.source_stats$cells_rendered <- 0L
        names_data <- private$source_column_names()
        n <- private$source_row_count()
        seed <- if (n) private$get_source_page(1L, min(100L, n), names_data) else
          private$get_source_page(1L, 0L, names_data)
        data <- seed[integer(), , drop = FALSE]
        private$.source_schema <- seed
      } else {
        data <- as_table_data(data)
        names_data <- names(data)
        private$.source <- NULL
        private$.source_closed <- FALSE
        private$.source_cache <- NULL
        private$.source_lru <- character()
        private$.source_schema <- NULL
      }
      unknown <- setdiff(c(names(opts$columns), names(opts$formatters)), names(data))
      if (length(unknown)) {
        stop(sprintf("Unknown column \"%s\" in data_table() options.", unknown[[1]]), call. = FALSE)
      }
      private$.data <- data
      private$.view <- NULL
      private$.column_order <- seq_along(data)
      private$.selection_anchor <- NA_integer_
      private$.selection_end <- NA_integer_
      private$.base_view <- NULL
      private$.filters <- list()
      private$.sort <- NULL
      private$.find <- NULL
      private$.find_hit <- NULL
      spec_data <- if (is_table_source(private$.source)) private$.source_schema else data
      private$.specs <- lapply(names_data %||% names(data), function(name) table_column_spec(spec_data[[name]], name, opts))
      if (is_table_source(private$.source) && is.null(private$.source$sort)) {
        for (i in seq_along(private$.specs)) private$.specs[[i]]$sortable <- FALSE
      }
      show_rn <- if (is_table_source(private$.source)) isTRUE(opts$row_names) else if (is.na(opts$row_names)) .row_names_info(data) > 0L else opts$row_names
      private$.row_names <- if (!show_rn) NULL else if (is_table_source(private$.source)) {
        list(auto = TRUE, width = as.integer(max(1L, nchar(as.character(private$source_row_count())))))
      } else table_row_name_spec(data)
      private$.state$offset_row <- 0L
      private$.state$offset_col <- 0L
      n <- private$row_count_data()
      private$.state$cursor_row <- if (self$cursor_type != "none" && n > 0L) 1L else 0L
      private$.state$cursor_col <- if (self$cursor_type == "cell" && ncol(data) > 0L) 1L else 0L
      self$invalidate()
      invisible(self)
    },

    #' @description Re-read the source row count and clear cached rows.
    refresh = function() {
      if (!is_table_source(private$.source)) {
        self$invalidate()
        return(invisible(self))
      }
      if (private$.source_closed) stop("This table source is closed.", call. = FALSE)
      if (!is.null(private$.source$refresh)) private$.source$refresh()
      if (!is.null(private$.source$filter)) private$.source$filter(NULL)
      if (!is.null(private$.source$sort)) private$.source$sort(NULL)
      self$set_data(private$.source)
      invisible(self)
    },

    #' @description Explicitly close this table's source resource.
    close = function() {
      if (is_table_source(private$.source) && !private$.source_closed) {
        if (!is.null(private$.source$close)) private$.source$close()
        private$.source_closed <- TRUE
        private$clear_source_cache()
      }
      invisible(self)
    },

    #' @description Read structural fetch/cache counters for diagnostics.
    source_stats = function() {
      if (!is_table_source(private$.source)) return(list(fetch_calls = 0L, rows_requested = 0L,
        cache_hits = 0L, cache_misses = 0L, rows_rendered = 0L, cells_rendered = 0L))
      as.list.environment(private$.source_stats, all.names = TRUE)
    },

    #' @description Sort the view by one or more columns (stable; `NA`s
    #'   last). Sorting always starts from the unsorted view (after
    #'   `filter()`), so `sort(c("cyl", "mpg"))` sorts by cyl, then mpg.
    #' @param by Column names or numbers.
    #' @param decreasing Sort in decreasing order? One value, or one per
    #'   column.
    sort = function(by, decreasing = FALSE) {
      if (is_table_source(private$.source) && is.null(private$.source$sort)) {
        stop("Sorting is not supported by this table source.", call. = FALSE)
      }
      js <- vapply(as.list(by), private$column_index, 1L)
      decreasing <- rep_len(as.logical(decreasing), length(js))
      if (anyNA(decreasing)) stop("`decreasing` must be TRUE or FALSE.", call. = FALSE)
      private$.sort <- list(columns = js, decreasing = decreasing)
      private$apply_sort()
    },

    #' @description Remove the sort order (back to the order before sorting).
    clear_sort = function() {
      private$.sort <- NULL
      private$apply_sort()
    },

    #' @description Header-click behaviour: sort by a column ascending, then
    #'   descending, then unsorted.
    #' @param column Column name or number.
    #' @param add Add the column to the existing sort (secondary key)?
    toggle_sort = function(column, add = FALSE) {
      j <- private$column_index(column)
      if (!private$.specs[[j]]$sortable) return(invisible(self))
      cur <- private$.sort
      k <- if (is.null(cur)) NA_integer_ else match(j, cur$columns)
      if (is.na(k)) {
        if (add && !is.null(cur)) {
          private$.sort <- list(columns = c(cur$columns, j), decreasing = c(cur$decreasing, FALSE))
        } else {
          private$.sort <- list(columns = j, decreasing = FALSE)
        }
      } else if (!cur$decreasing[[k]]) {
        cur$decreasing[[k]] <- TRUE
        private$.sort <- cur
      } else {
        cur$columns <- cur$columns[-k]
        cur$decreasing <- cur$decreasing[-k]
        private$.sort <- if (length(cur$columns)) cur
      }
      private$apply_sort()
    },

    #' @description Show a subset of rows (in the given order), or all rows.
    #' @param rows Row numbers of the data, a logical vector over the rows,
    #'   or `NULL` for all rows.
    filter = function(rows = NULL) {
      if (is_table_source(private$.source)) {
        if (is.null(private$.source$filter)) stop("Row filtering is not supported by this table source.", call. = FALSE)
        if (!is.null(rows)) stop("Position filters are unavailable for lazy sources; use `filter_columns()`.", call. = FALSE)
        private$.base_view <- NULL
        private$apply_sort(announce = FALSE)
        return(invisible(self))
      }
      n <- nrow(private$.data)
      if (is.logical(rows)) {
        if (length(rows) != n) stop("A logical filter must have one value per row.", call. = FALSE)
        rows <- which(rows)
      }
      if (!is.null(rows)) {
        rows <- as.integer(rows)
        if (anyNA(rows) || any(rows < 1L | rows > n)) stop("Row numbers are out of range.", call. = FALSE)
      }
      private$.base_view <- rows
      private$apply_sort(announce = FALSE)
    },

    #' @description Filter named columns; all predicates are combined with
    #'   AND and evaluated in row chunks. A predicate may be a
    #'   `table_filter()` or `function(values) logical`.
    #' @param filters Named list by column; `NULL` clears column filters.
    filter_columns = function(filters = NULL) {
      if (is_table_source(private$.source)) {
        if (is.null(private$.source$filter)) stop("Filtering is not supported by this table source.", call. = FALSE)
        if (!is.null(filters)) {
          filters <- check_named_list(filters, "filters")
          unknown <- setdiff(names(filters), private$source_column_names())
          if (length(unknown)) stop(sprintf("Unknown filter column \"%s\".", unknown[[1L]]), call. = FALSE)
        }
        result <- private$.source$filter(filters)
        if (identical(result, FALSE)) stop("The table source rejected the filter.", call. = FALSE)
        private$.filters <- filters %||% list()
        if (!is.null(private$.sort)) {
          sorted <- private$.source$sort(list(columns = names(private$.data)[private$.sort$columns],
                                               decreasing = private$.sort$decreasing))
          if (identical(sorted, FALSE)) stop("The table source rejected the current sort after filtering.", call. = FALSE)
        }
        private$clear_source_cache()
        private$set_view(NULL)
        self$invalidate()
        return(invisible(self))
      }
      if (is.null(filters)) {
        private$.filters <- list()
      } else {
        filters <- check_named_list(filters, "filters")
        unknown <- setdiff(names(filters), names(private$.data))
        if (length(unknown)) stop(sprintf("Unknown filter column \"%s\".", unknown[[1L]]), call. = FALSE)
        for (i in seq_along(filters)) {
          if (inherits(filters[[i]], "termr_table_filter")) next
          check_function(filters[[i]], sprintf("filters[[%d]]", i))
        }
        private$.filters <- filters
      }
      private$apply_sort(announce = FALSE)
    },

    #' @description Move the cursor (and scroll so it is visible).
    #' @param row Position in the view.
    #' @param column Column name or number (cell cursor).
    move_cursor = function(row = NULL, column = NULL) {
      if (self$cursor_type == "none") return(invisible(self))
      n <- self$row_count
      old <- c(private$.state$cursor_row, private$.state$cursor_col)
      if (!is.null(row) && n > 0L) self$set_state("cursor_row", as.integer(min(max(1L, row), n)))
      if (!is.null(column) && self$cursor_type == "cell") {
        self$set_state("cursor_col", private$column_index(column))
      }
      private$reveal_cursor()
      if (!identical(old, c(private$.state$cursor_row, private$.state$cursor_col))) private$announce_selection()
      invisible(self)
    },

    #' @description Scroll so that a view position is the first visible row.
    #' @param row Position in the view.
    scroll_to_row = function(row) {
      self$set_state("offset_row", private$clamp_row_offset(as.integer(row) - 1L))
      invisible(self)
    },

    #' @description The data-frame row number or source row key under the cursor (or `NA`).
    selected_row = function() {
      pos <- private$.state$cursor_row
      if (pos < 1L) NA_integer_ else private$row_at_position(pos)
    },

    #' @description One data row as a named list.
    #' @param row Original row number.
    row_data = function(row) {
      if (is_table_source(private$.source)) return(as.list(private$source_read(row, private$source_column_names())[1L, , drop = FALSE]))
      as.list(private$.data[row, , drop = FALSE])
    },

    #' @description Data for selected view rows, in view order. A single
    #'   cursor row is returned when no range is selected.
    selected_data = function() {
      pos <- private$selected_positions()
      if (is_table_source(private$.source)) return(private$source_read(private$positions_to_rows(pos), private$source_column_names()))
      private$.data[private$view_rows()[pos], , drop = FALSE]
    },

    #' @description Data in the current viewport, in display order.
    visible_data = function() {
      g <- private$geometry()
      if (is_table_source(private$.source)) {
        if (is.null(g) || !length(g$rows)) return(private$empty_source_data(private$visible_columns()))
        return(private$source_read(private$positions_to_rows(g$rows), names(private$.data)[g$columns]))
      }
      if (is.null(g) || !length(g$rows)) return(private$.data[integer(), private$visible_columns(), drop = FALSE])
      private$.data[private$view_rows()[g$rows], g$columns, drop = FALSE]
    },

    #' @description Move a column in the display order.
    #' @param column Column name or number.
    #' @param to New 1-based position in the complete column order.
    reorder_column = function(column, to) {
      j <- private$column_index(column)
      if (length(to) != 1L || is.na(to) || !is.numeric(to) || to < 1 || to > length(private$.column_order)) {
        stop("`to` must be a valid 1-based column position.", call. = FALSE)
      }
      to <- as.integer(to)
      from <- match(j, private$.column_order)
      if (from != to) {
        private$.column_order <- append(private$.column_order[-from], j, after = to - 1L)
        private$.state$offset_col <- 0L
        private$.find_hit <- NULL
        self$invalidate()
        self$post_message("datatable.columns_reordered", list(column = names(private$.data)[[j]], from = from, to = to))
      }
      invisible(self)
    },

    #' @description Select a range of rows by view position.
    #' @param from,to 1-based positions in the view.
    select_range = function(from, to = from) {
      n <- self$row_count
      if (!n) return(invisible(self))
      if (length(from) != 1L || length(to) != 1L || is.na(from) || is.na(to) ||
          !is.numeric(from) || !is.numeric(to) || from < 1 || to < 1 || from > n || to > n) {
        stop("Selection positions must be between 1 and the row count.", call. = FALSE)
      }
      private$.selection_anchor <- as.integer(from)
      private$.selection_end <- as.integer(to)
      self$move_cursor(row = to)
      self$invalidate()
      invisible(self)
    },

    #' @description Copy selected rows as tab-separated values.
    #' @param headers Include column names?
    #' @param system Also write to the system clipboard when supported?
    copy_selection = function(headers = TRUE, system = TRUE) {
      check_flag(headers)
      check_flag(system)
      app <- self$app
      if (is.null(app)) stop("The table is not attached to an app.", call. = FALSE)
      data <- self$selected_data()[, private$visible_columns(), drop = FALSE]
      rows <- lapply(seq_len(nrow(data)), function(i) vapply(data, function(col) {
        value <- col[[i]]
        text <- if (!length(value) || all(is.na(value))) "NA" else paste(as.character(value), collapse = ",")
        if (grepl('[\t\r\n"]', text)) paste0('"', gsub('"', '""', text, fixed = TRUE), '"') else text
      }, ""))
      lines <- vapply(rows, paste, "", collapse = "\t")
      if (headers) lines <- c(paste(names(data), collapse = "\t"), lines)
      app$clipboard_write(paste(lines, collapse = "\n"), system = system)
      invisible(self)
    },

    #' @description Jump to a row of the view (and scroll to it).
    #' @param row Position in the view (1-based).
    goto_row = function(row) {
      n <- self$row_count
      if (n == 0L) return(invisible(self))
      row <- as.integer(min(max(1L, row), n))
      if (self$cursor_type == "none") self$scroll_to_row(row) else self$move_cursor(row = row)
      invisible(self)
    },

    #' @description Width of a column in cells.
    #' @param column Column name or number.
    column_width = function(column) private$.specs[[private$column_index(column)]]$width,

    #' @description Set a column's width (at least 1 cell). Sends
    #'   `"datatable.column_resized"`.
    #' @param column Column name or number.
    #' @param width Width in cells.
    set_column_width = function(column, width) {
      j <- private$column_index(column)
      private$set_width(j, width, fixed = TRUE, notify = TRUE)
      invisible(self)
    },

    #' @description Fit a column to its content. Rows are sampled, never all
    #'   scanned unless asked for, so this is fast for huge tables.
    #' @param column Column name or number.
    #' @param rows `"sample"` (the visible rows plus the first and last 500;
    #'   default), `"visible"` (visible rows only) or `"all"` (every row of
    #'   the view; slow for millions of rows).
    auto_size_column = function(column, rows = c("sample", "visible", "all")) {
      rows <- match.arg(rows)
      j <- private$column_index(column)
      private$set_width(j, private$estimate_width(j, rows), fixed = FALSE, notify = TRUE)
      invisible(self)
    },

    #' @description Fit every visible column to its content.
    #' @param rows See `auto_size_column()`.
    auto_size_all = function(rows = c("sample", "visible", "all")) {
      rows <- match.arg(rows)
      for (j in private$visible_columns()) {
        private$set_width(j, private$estimate_width(j, rows), fixed = FALSE, notify = FALSE)
      }
      self$post_message("datatable.column_resized", list(column = NULL, width = NULL))
      invisible(self)
    },

    #' @description Show or hide a column.
    #' @param column Column name or number.
    #' @param visible `TRUE` to show, `FALSE` to hide.
    set_column_visible = function(column, visible = TRUE) {
      check_flag(visible)
      j <- private$column_index(column)
      if (private$.specs[[j]]$visible == visible) return(invisible(self))
      if (!visible && length(private$visible_columns()) <= 1L) {
        stop("A table needs at least one visible column.", call. = FALSE)
      }
      private$.specs[[j]]$visible <- visible
      vis <- private$visible_columns()
      if (private$.state$cursor_col > 0L && !(private$.state$cursor_col %in% vis)) {
        old_position <- match(j, private$.column_order)
        positions <- match(vis, private$.column_order)
        private$.state$cursor_col <- vis[[which.min(abs(positions - old_position))]]
      }
      private$.find_hit <- NULL
      self$invalidate()
      self$post_message("datatable.columns_changed", list(column = names(private$.data)[[j]], visible = visible))
      invisible(self)
    },

    #' @description Describe the columns: name, label, width, visible, sortable.
    column_info = function() {
      data.frame(
        name = vapply(private$.specs, `[[`, "", "name"),
        label = vapply(private$.specs, `[[`, "", "label"),
        display_position = match(seq_along(private$.specs), private$.column_order),
        width = vapply(private$.specs, `[[`, 1L, "width"),
        visible = vapply(private$.specs, `[[`, TRUE, "visible"),
        sortable = vapply(private$.specs, `[[`, TRUE, "sortable"),
        stringsAsFactors = FALSE
      )
    },

    #' @description Search the formatted cell text and move the cursor to
    #'   the first match at or after the cursor (reading order, wrapping
    #'   around). Large tables are scanned in chunks; the data is never
    #'   converted to one big string matrix.
    #' @param query Text or regular expression (PCRE).
    #' @param column Search only this column (name or number); `NULL`: all
    #'   visible columns.
    #' @param case_sensitive Match case?
    #' @param regex Is `query` a regular expression?
    #' @return `TRUE` if there is a match (invisibly).
    find = function(query, column = NULL, case_sensitive = FALSE, regex = FALSE) {
      private$.find <- search_query(query, case_sensitive, regex)
      private$.find_text <- query
      private$.find_column <- if (is.null(column)) NULL else private$column_index(column)
      private$.find_hit <- NULL
      invisible(private$goto_hit(1L, include_current = TRUE))
    },

    #' @description Go to the next match.
    find_next = function() invisible(private$goto_hit(1L)),

    #' @description Go to the previous match.
    find_previous = function() invisible(private$goto_hit(-1L)),

    #' @description Forget the search and close the find bar.
    clear_find = function() {
      private$.find <- NULL
      private$.find_hit <- NULL
      private$.bar <- NULL
      self$invalidate()
      invisible(self)
    },

    #' @description Edit a cell in a small dialog (needs `editable = TRUE`).
    #'   The default is the cursor cell.
    #' @param position Position in the view (default: the cursor row).
    #' @param column Column name or number (default: the cursor / active column).
    edit_cell = function(position = NULL, column = NULL) {
      if (!self$editable) stop("This table is not editable; create it with `editable = TRUE`.", call. = FALSE)
      app <- self$app
      if (is.null(app)) stop("The table is not attached to an app.", call. = FALSE)
      position <- position %||% private$.state$cursor_row
      if (position < 1L || position > self$row_count) return(invisible(self))
      j <- if (is.null(column)) private$active_column() else private$column_index(column)
      spec <- private$.specs[[j]]
      if (!private$cell_editable(j)) {
        app$notify(sprintf("Column \"%s\" cannot be edited.", spec$label), severity = "warning")
        return(invisible(self))
      }
      template <- private$.data[[j]]
      if (is_table_source(private$.source)) {
        row <- as.integer(position)
        template <- private$.source_schema[[j]]
        current <- private$source_read(row, names(private$.data)[[j]])[[1L]][[1L]]
      } else {
        row <- private$view_rows()[[position]]
        current <- template[[row]]
      }
      current <- edit_text(current)
      editor <- input(current, id = "cell_editor",
                      validate = function(txt) parse_cell(txt, template)$error)
      dlg <- modal(
        label(sprintf("Row %s", format(row)), style = style(foreground = "$muted")),
        editor,
        title = sprintf("Edit %s", spec$label), width = 44L
      )
      table <- self
      dlg$on("input.submitted", function(event, app) {
        if (event$data$valid) dlg$dismiss(list(text = event$data$value))
      })
      app$push_screen(dlg, callback = function(result, app) {
        if (is.null(result)) return(invisible())
        value <- parse_cell(result$text, template)$value
        tryCatch(
          table$set_cell(row, j, value),
          error = function(e) app$notify(conditionMessage(e), severity = "error")
        )
      })
      invisible(self)
    },

    #' @description Change one cell of the table's own copy of the data
    #'   (the data frame you passed in is never modified; read the result
    #'   with `$data`). Runs `on_edit`, then sends `"datatable.cell_changed"`
    #'   with `row`, `column`, `old` and `value`.
    #' @param row Original row number.
    #' @param column Column name or number.
    #' @param value New value (must fit the column type).
    set_cell = function(row, column, value) {
      j <- private$column_index(column)
      name <- names(private$.data)[[j]]
      if (is_table_source(private$.source)) {
        if (is.null(private$.source$set_value)) stop("Editing is not supported by this table source.", call. = FALSE)
        if (length(row) != 1L || is.na(row) || row < 1L || row > private$source_row_count()) stop("`row` is out of range.", call. = FALSE)
        template <- private$.source_schema[[j]]
        checked <- parse_cell(value, template)
        if (!is.null(checked$error)) stop(checked$error, call. = FALSE)
        value <- checked$value
        if (!is.null(self$on_edit)) {
          msg <- run_validator(function(v) self$on_edit(row, name, v), value)
          if (!is.null(msg)) stop(msg, call. = FALSE)
        }
        old <- private$source_read(row, name)[[1L]][[1L]]
        result <- private$.source$set_value(as.integer(row), name, value)
        if (identical(result, FALSE)) stop("The table source rejected the edit.", call. = FALSE)
        private$clear_source_cache()
        self$invalidate()
        self$post_message("datatable.cell_changed", list(row = row, column = name, old = old, value = value))
        return(invisible(self))
      }
      if (length(row) != 1L || is.na(row) || row < 1L || row > nrow(private$.data)) {
        stop("`row` is out of range.", call. = FALSE)
      }
      if (!private$cell_editable(j)) stop(sprintf("Column \"%s\" cannot be edited.", name), call. = FALSE)
      col <- private$.data[[j]]
      checked <- parse_cell(value, col)
      if (!is.null(checked$error)) stop(checked$error, call. = FALSE)
      value <- checked$value
      if (!is.null(self$on_edit)) {
        msg <- run_validator(function(v) self$on_edit(row, name, v), value)
        if (!is.null(msg)) stop(msg, call. = FALSE)
      }
      old <- col[[row]]
      col[row] <- value
      private$.data[[j]] <- col
      if (isTRUE(private$.specs[[j]]$auto_formatter)) private$.specs[[j]]$formatter <- default_formatter(col)
      if (isTRUE(private$.specs[[j]]$auto)) {
        need <- str_width(private$.specs[[j]]$formatter(col[row]))
        cap <- private$.specs[[j]]$max_width %||% private$.options$max_width %||% 1000L
        if (need > private$.specs[[j]]$width) private$.specs[[j]]$width <- as.integer(min(need, cap))
      }
      self$invalidate()
      self$post_message("datatable.cell_changed", list(row = row, column = name, old = old, value = value))
      invisible(self)
    },

    #' @description Actions used by the bindings.
    action_cursor_up = function() private$step_rows(-1L),
    #' @description Move down.
    action_cursor_down = function() private$step_rows(1L),
    #' @description Extend selection upward.
    action_extend_selection_up = function() private$extend_selection(-1L),
    #' @description Extend selection downward.
    action_extend_selection_down = function() private$extend_selection(1L),
    #' @description Move the active column left in display order.
    action_move_column_left = function() private$move_active_column(-1L),
    #' @description Move the active column right in display order.
    action_move_column_right = function() private$move_active_column(1L),
    #' @description Copy selected rows.
    action_copy_selection = function() self$copy_selection(),
    #' @description Move left (cell cursor) or scroll left.
    action_cursor_left = function() private$step_cols(-1L),
    #' @description Move right (cell cursor) or scroll right.
    action_cursor_right = function() private$step_cols(1L),
    #' @description Move one page up.
    action_page_up = function() private$step_rows(-max(1L, private$geometry()$body_h - 1L)),
    #' @description Move one page down.
    action_page_down = function() private$step_rows(max(1L, private$geometry()$body_h - 1L)),
    #' @description Go to the first row.
    action_first_row = function() private$step_rows(-.Machine$integer.max),
    #' @description Go to the last row.
    action_last_row = function() private$step_rows(.Machine$integer.max),
    #' @description Open the find bar (Ctrl+F).
    action_open_find = function() {
      private$.bar <- "find"
      self$invalidate()
    },
    #' @description Open the "go to row" bar (Ctrl+G).
    action_open_goto = function() {
      private$.bar <- "goto"
      private$.bar_text <- ""
      self$invalidate()
    },
    #' @description Go to the next match (F3).
    action_find_next = function() self$find_next(),
    #' @description Go to the previous match (Shift+F3).
    action_find_previous = function() self$find_previous(),
    #' @description Make the active column one cell wider (Alt+Right).
    action_widen_column = function() private$nudge_width(1L),
    #' @description Make the active column one cell narrower (Alt+Left).
    action_narrow_column = function() private$nudge_width(-1L),
    #' @description Sort by the active column (S): ascending, descending, off.
    action_sort_column = function() self$toggle_sort(private$active_column()),
    #' @description Send `"datatable.row_activated"` for the cursor row
    #'   (or edit the cell when the table is editable).
    action_activate = function() {
      pos <- private$.state$cursor_row
      if (pos < 1L) return(invisible(self))
      if (self$editable) return(self$edit_cell())
      self$post_message("datatable.row_activated", private$selection_data())
    },

    #' @description Mouse: clicking a row moves the cursor, clicking the
    #'   header sends `"datatable.header_selected"` (and sorts when
    #'   `header_sort` is on); dragging a column separator in the header
    #'   resizes the column; a double click edits a cell of an editable table.
    #' @param event A `MouseEvent`.
    on_mouse_down = function(event) {
      if (event$button != "left") return(invisible())
      private$.resizing <- NULL
      private$.column_drag <- NULL
      private$.selection_drag <- FALSE
      sep <- private$separator_at(event$screen_x, event$screen_y)
      if (!is.na(sep)) {
        private$.resizing <- list(column = sep, width = private$.specs[[sep]]$width)
        event$stop()
        return(invisible())
      }
      hit <- private$locate(event$screen_x, event$screen_y)
      if (is.null(hit)) return(invisible())
      if (identical(hit$part, "header") && !is.na(hit$column)) {
        spec <- private$.specs[[hit$column]]
        private$.state$active_col <- hit$column
        private$.column_drag <- list(column = hit$column, from = event$screen_x, moved = FALSE,
                                     shift = isTRUE(event$shift))
        if (self$header_sort) self$toggle_sort(hit$column, add = isTRUE(event$shift))
        self$post_message("datatable.header_selected", list(column = spec$name, index = hit$column))
      } else if (identical(hit$part, "body")) {
        if (!is.na(hit$column)) private$.state$active_col <- hit$column
        if (isTRUE(event$shift) && !is.na(private$.selection_anchor)) {
          private$.selection_end <- hit$row
          self$move_cursor(row = hit$row, column = if (!is.na(hit$column) && self$cursor_type == "cell") hit$column)
        } else {
          private$.selection_anchor <- hit$row
          private$.selection_end <- hit$row
          self$move_cursor(row = hit$row, column = if (!is.na(hit$column) && self$cursor_type == "cell") hit$column)
        }
        private$.selection_drag <- TRUE
        self$invalidate()
        last <- private$.last_click
        private$.last_click <- list(time = event$time, row = hit$row, column = hit$column)
        if (self$editable && !is.null(last) && event$time - last$time < 0.5 && identical(last$row, hit$row) &&
            identical(last$column, hit$column)) {
          private$.last_click <- NULL
          self$edit_cell(position = hit$row, column = if (!is.na(hit$column)) hit$column)
        }
      }
      event$stop()
    },

    #' @description Resizing a column by dragging its header separator.
    #' @param event A `MouseEvent`.
    on_drag_move = function(event) {
      rz <- private$.resizing
      if (is.null(rz)) {
        drag <- private$.column_drag
        if (!is.null(drag) && abs(event$screen_x - drag$from) >= 2L) {
          hit <- private$locate(event$screen_x, event$screen_y)
          if (!is.null(hit) && identical(hit$part, "header") && !is.na(hit$column)) {
            drag$moved <- TRUE
            private$.column_drag <- drag
            from <- match(drag$column, private$.column_order)
            to <- match(hit$column, private$.column_order)
            if (!is.na(from) && !is.na(to) && from != to) self$reorder_column(drag$column, to)
          }
          event$stop()
        } else if (isTRUE(private$.selection_drag)) {
          hit <- private$locate(event$screen_x, event$screen_y)
          if (!is.null(hit) && identical(hit$part, "body")) {
            private$.selection_end <- hit$row
            self$move_cursor(row = hit$row)
            self$invalidate()
          }
          event$stop()
        }
        return(invisible())
      }
      private$set_width(rz$column, rz$width + (event$screen_x - event$origin_x), fixed = TRUE, notify = FALSE)
      event$stop()
    },

    #' @description Ends a column resize.
    #' @param event A `MouseEvent`.
    on_drag_end = function(event) {
      rz <- private$.resizing
      if (is.null(rz)) {
        drag <- private$.column_drag
        private$.column_drag <- NULL
        private$.selection_drag <- FALSE
        return(invisible())
      }
      private$.resizing <- NULL
      spec <- private$.specs[[rz$column]]
      self$post_message("datatable.column_resized", list(column = spec$name, width = spec$width))
    },

    #' @description Keys for the find / go-to bar while it is open.
    #' @param event A `KeyEvent`.
    on_key = function(event) {
      if (is.null(private$.bar) || !self$is_enabled()) return(invisible())
      private$bar_key(event)
    },

    #' @description Pasted text goes into the find / go-to bar when open.
    #' @param event A `PasteEvent`.
    on_paste = function(event) {
      if (is.null(private$.bar)) return(invisible())
      text <- gsub("[[:space:]]+", " ", event$text)
      if (private$.bar == "find") private$find_set(paste0(private$.find_text, text))
      else private$.bar_text <- paste0(private$.bar_text, gsub("[^0-9]", "", text))
      self$invalidate()
      event$stop()
    },

    #' @description The mouse wheel scrolls three rows.
    #' @param event A `MouseEvent`.
    on_mouse_scroll = function(event) {
      delta <- switch(event$direction, up = -3L, down = 3L, 0L)
      if (delta != 0L) {
        self$set_state("offset_row", private$clamp_row_offset(private$.state$offset_row + delta))
        event$stop()
      }
    },

    #' @description Natural width: all columns.
    content_width = function() {
      widths <- vapply(private$.specs[private$visible_columns()], `[[`, integer(1), "width")
      rn <- private$.row_names
      sum(widths) + max(0L, length(widths) - 1L) + (if (is.null(rn)) 0L else rn$width + 1L) + 1L
    },

    #' @description Natural height: header plus rows (at most 1000).
    #' @param width Content width.
    content_height = function(width) {
      min(self$row_count, 1000L) + as.integer(self$show_header)
    },

    #' @description Draw the visible part of the table.
    #' @param buffer A [ScreenBuffer].
    #' @param area Visible part of the region.
    #' @param st Computed style.
    paint = function(buffer, area, st) {
      draw_background(buffer, area, st)
      draw_border(buffer, self$region, st, area, self$border_title)
      g <- private$geometry()
      if (is.null(g)) return(invisible())
      clip <- rect_intersect(g$inner, area)
      cols <- g$columns
      rn <- private$.row_names
      header_style <- resolve_style(table_styles$header, parent = st)
      if (self$show_header) {
        specs <- private$.specs[cols]
        cells <- pad_cells(vapply(specs, `[[`, "", "label"), g$widths, vapply(specs, `[[`, "", "align"))
        marks <- private$sort_marks(cols)
        for (i in which(nzchar(marks))) {
          w <- g$widths[[i]]
          mw <- str_width(marks[[i]])
          cells[[i]] <- if (w > mw) paste0(pad_cells(specs[[i]]$label, w - mw, specs[[i]]$align), marks[[i]]) else
            str_truncate(marks[[i]], w)
        }
        line <- table_join(if (!is.null(rn)) strrep(" ", rn$width), cells, g$frozen_shown)
        buffer$put_text(g$inner$x, g$inner$y, str_align(line, g$body_w), fg = header_style$foreground,
                        bg = header_style$background, attrs = header_style$attrs, clip = clip)
      }
      if (!is.null(private$.source_error) && g$body_h > 0L && g$body_w > 0L) {
        error_style <- resolve_style(style(foreground = "$error", bold = TRUE), parent = st)
        buffer$put_text(g$inner$x, g$body_y,
                        str_truncate(paste0("Source error: ", private$.source_error), g$body_w),
                        fg = error_style$foreground, bg = error_style$background,
                        attrs = error_style$attrs, clip = clip)
        private$paint_scrollbars(buffer, g, st, area)
        return(invisible())
      }
      rows_pos <- g$rows
      if (length(rows_pos) > 0L) {
        data_rows <- private$positions_to_rows(rows_pos)
        if (is_table_source(private$.source)) {
          private$.source_stats$rows_rendered <- private$.source_stats$rows_rendered + length(data_rows)
          private$.source_stats$cells_rendered <- private$.source_stats$cells_rendered + length(data_rows) * length(cols)
        }
        texts <- lapply(cols, function(j) pad_cells(private$format_values(j, data_rows), g$widths[[match(j, cols)]], private$.specs[[j]]$align))
        rn_text <- if (is.null(rn)) NULL else {
          rn_labels <- if (is_table_source(private$.source)) as.character(data_rows) else table_row_labels(private$.data, rn, data_rows)
          pad_cells(rn_labels, rn$width, "left")
        }
        focused <- self$focused
        for (k in seq_along(rows_pos)) {
          y <- g$body_y + k - 1L
          pieces <- vapply(texts, `[[`, "", k)
          line <- table_join(if (!is.null(rn)) rn_text[[k]], pieces, g$frozen_shown)
          row_st <- private$row_paint_style(rows_pos[[k]], data_rows[[k]], st, focused)
          buffer$put_text(g$inner$x, y, str_align(line, g$body_w), fg = row_st$foreground,
                          bg = row_st$background, attrs = row_st$attrs, clip = clip)
          if (!is.null(rn)) {
            rn_st <- resolve_style(table_styles$row_label, parent = row_st)
            buffer$put_text(g$inner$x, y, rn_text[[k]], fg = rn_st$foreground, bg = rn_st$background,
                            attrs = rn_st$attrs, clip = clip)
          }
          private$paint_cells(buffer, g, k, rows_pos[[k]], data_rows[[k]], pieces, row_st, focused, clip)
        }
      }
      private$paint_scrollbars(buffer, g, st, area)
      if (!is.null(private$.bar)) private$paint_bar(buffer, g, st, clip)
    }
  ),
  active = list(
    #' @field data The data frame or source (assigning calls `set_data()`).
    data = function(value) {
      if (missing(value)) return(if (is_table_source(private$.source)) private$.source else private$.data)
      self$set_data(value)
    },
    #' @field row_count Number of rows in the current view.
    row_count = function(value) if (missing(value)) private$row_count_view() else read_only("row_count"),
    #' @field column_names Names of the data columns.
    column_names = function(value) if (missing(value)) names(private$.data) else read_only("column_names"),
    #' @field cursor_row Cursor position in the view (0 = none).
    cursor_row = function(value) {
      if (missing(value)) return(private$.state$cursor_row)
      self$move_cursor(row = value)
    },
    #' @field cursor_column Cursor column number (cell cursor; 0 = none).
    cursor_column = function(value) {
      if (missing(value)) return(private$.state$cursor_col)
      self$move_cursor(column = value)
    },
    #' @field offset_row,offset_column Scroll offsets (rows / columns).
    offset_row = function(value) if (missing(value)) private$.state$offset_row else read_only("offset_row"),
    offset_column = function(value) if (missing(value)) private$.state$offset_col else read_only("offset_column"),
    #' @field view Row numbers or source view positions in display order.
    view = function(value) if (missing(value)) private$view_rows() else read_only("view"),
    #' @field sort_state `NULL` or a data frame (`column`, `decreasing`) of
    #'   the current sort keys.
    sort_state = function(value) {
      if (!missing(value)) read_only("sort_state")
      so <- private$.sort
      if (is.null(so)) return(NULL)
      data.frame(column = names(private$.data)[so$columns], decreasing = so$decreasing, stringsAsFactors = FALSE)
    },
    #' @field match_hit `NULL` or the current search result (`position`,
    #'   `row`, `column`).
    match_hit = function(value) {
      if (!missing(value)) read_only("match_hit")
      hit <- private$.find_hit
      if (is.null(hit)) return(NULL)
      list(position = hit$pos, row = private$row_at_position(hit$pos), column = names(private$.data)[[hit$column]])
    }
  ),
  private = list(
    .data = NULL,
    .source = NULL,
    .source_closed = FALSE,
    .source_error = NULL,
    .source_schema = NULL,
    .source_cache = NULL,
    .source_lru = character(),
    .source_stats = NULL,
    .view = NULL,
    .specs = list(),
    .row_names = NULL,
    .options = NULL,
    .base_view = NULL,
    .filters = list(),
    .sort = NULL,
    .find = NULL,
    .find_text = "",
    .find_column = NULL,
    .find_hit = NULL,
    .find_error = NULL,
    .bar = NULL,
    .bar_text = "",
    .resizing = NULL,
    .last_click = NULL,
    .column_order = integer(),
    .column_drag = NULL,
    .selection_anchor = NA_integer_,
    .selection_end = NA_integer_,
    .selection_drag = FALSE,

    view_rows = function() private$.view %||% seq_len(private$row_count_data()),

    row_count_data = function() if (is_table_source(private$.source)) private$source_row_count() else nrow(private$.data),

    row_count_view = function() if (is_table_source(private$.source)) private$source_row_count() else length(private$view_rows()),

    source_row_count = function() {
      if (isTRUE(private$.source_closed)) stop("This table source is closed.", call. = FALSE)
      n <- private$.source$row_count()
      if (!is.numeric(n) || length(n) != 1L || is.na(n) || !is.finite(n) ||
          n < 0 || n != floor(n) || n > .Machine$integer.max) {
        stop("`table_source$row_count()` must return a non-negative whole number no larger than .Machine$integer.max.", call. = FALSE)
      }
      as.integer(n)
    },

    source_column_names = function() {
      nm <- private$.source$column_names()
      if (!is.character(nm) || !length(nm) || anyNA(nm) || any(!nzchar(nm)) || anyDuplicated(nm)) {
        stop("`table_source$column_names()` must return unique, non-empty column names.", call. = FALSE)
      }
      nm
    },

    clear_source_cache = function() {
      if (is_table_source(private$.source)) {
        rm(list = ls(private$.source_cache, all.names = TRUE), envir = private$.source_cache)
        private$.source_lru <- character()
      }
      invisible()
    },

    get_source_page = function(start, count, columns) {
      if (isTRUE(private$.source_closed)) stop("This table source is closed.", call. = FALSE)
      n <- private$source_row_count()
      expected <- if (count == 0L || start > n) 0L else min(count, n - start + 1L)
      key <- paste(start, count, paste(columns, collapse = "\r"), sep = "|")
      if (exists(key, private$.source_cache, inherits = FALSE)) {
        private$.source_stats$cache_hits <- private$.source_stats$cache_hits + 1L
        private$.source_lru <- c(private$.source_lru[private$.source_lru != key], key)
        return(get(key, private$.source_cache, inherits = FALSE))
      }
      private$.source_stats$cache_misses <- private$.source_stats$cache_misses + 1L
      private$.source_stats$fetch_calls <- private$.source_stats$fetch_calls + 1L
      private$.source_stats$rows_requested <- private$.source_stats$rows_requested + expected
      failed <- FALSE
      result <- tryCatch(private$.source$get_rows(start, count, columns), error = function(e) {
        message <- sprintf("`table_source$get_rows(start=%d, count=%d)` failed: %s",
                           start, count, conditionMessage(e))
        if (is.null(private$.source$on_error)) stop(message, call. = FALSE)
        failed <<- TRUE
        private$.source_error <<- conditionMessage(e)
        self$post_message("datatable.source_error", list(message = message, start = start, count = count))
        tryCatch(private$.source$on_error(e, start, count, columns), error = function(handler_error) {
          stop(paste0(message, "; source error handler failed: ", conditionMessage(handler_error)), call. = FALSE)
        })
      })
      if (!is.data.frame(result)) stop("`table_source$get_rows()` must return a data.frame.", call. = FALSE)
      if (nrow(result) != expected) {
        stop(sprintf("`table_source$get_rows(start=%d, count=%d)` returned %d rows; expected %d.", start, count, nrow(result), expected), call. = FALSE)
      }
      if (!identical(names(result), columns)) {
        stop(sprintf("`table_source$get_rows(start=%d, count=%d)` returned inconsistent column names.", start, count), call. = FALSE)
      }
      if (!failed) private$.source_error <- NULL
      assign(key, result, private$.source_cache)
      private$.source_lru <- c(private$.source_lru, key)
      if (length(private$.source_lru) > 32L) {
        expired <- private$.source_lru[[1L]]
        private$.source_lru <- private$.source_lru[-1L]
        if (exists(expired, private$.source_cache, inherits = FALSE)) rm(list = expired, envir = private$.source_cache)
      }
      result
    },

    source_read = function(rows, columns) {
      if (!length(rows)) return(private$empty_source_data(columns))
      n <- private$source_row_count()
      if (anyNA(rows) || any(rows < 1L | rows > n)) stop("A table source row request is out of range.", call. = FALSE)
      chunk <- 100L
      ids <- (as.integer(rows) - 1L) %/% chunk
      pieces <- vector("list", length(rows))
      for (id in unique(ids)) {
        at <- which(ids == id)
        start <- id * chunk + 1L
        page <- private$get_source_page(start, min(chunk, n - start + 1L), columns)
        pieces[at] <- lapply(match(rows[at], seq.int(start, length.out = nrow(page))), function(i) page[i, , drop = FALSE])
      }
      out <- do.call(rbind, pieces)
      row.names(out) <- NULL
      out
    },

    empty_source_data = function(columns) {
      schema <- private$.source_schema
      out <- schema[integer(), columns, drop = FALSE]
      out
    },

    positions_to_rows = function(positions) {
      if (is_table_source(private$.source)) as.integer(positions) else private$view_rows()[positions]
    },

    row_at_position = function(position) {
      if (is_table_source(private$.source)) {
        if (is.null(private$.source$row_key)) return(as.integer(position))
        return(private$.source$row_key(as.integer(position)))
      }
      private$view_rows()[[position]]
    },

    set_view = function(rows) {
      selected <- self$selected_row()
      old_position <- private$.state$cursor_row
      private$.view <- rows
      private$.selection_anchor <- NA_integer_
      private$.selection_end <- NA_integer_
      private$.find_hit <- NULL
      view <- if (is_table_source(private$.source)) NULL else private$view_rows()
      pos <- if (is_table_source(private$.source)) old_position else if (is.na(selected)) NA_integer_ else match(selected, view)
      n <- private$row_count_view()
      new_cursor <- if (self$cursor_type == "none" || n == 0L) 0L else if (is.na(pos)) 1L else min(n, max(1L, pos))
      private$.state$cursor_row <- new_cursor
      private$.state$offset_row <- private$clamp_row_offset(private$.state$offset_row)
      private$reveal_cursor()
      self$invalidate()
      invisible(self)
    },

    column_index = function(column) {
      p <- length(private$.specs)
      j <- if (is.character(column)) match(column, names(private$.data)) else as.integer(column)
      if (length(j) != 1L || is.na(j) || j < 1L || j > p) {
        stop(sprintf("Unknown table column: %s.", format(column)), call. = FALSE)
      }
      j
    },

    format_values = function(j, rows) {
      spec <- private$.specs[[j]]
      values <- if (is_table_source(private$.source)) private$source_read(rows, names(private$.data)[[j]])[[1L]] else private$.data[[j]][rows]
      out <- spec$formatter(values)
      if (!is.character(out) || length(out) != length(rows)) {
        stop(sprintf("The formatter of column \"%s\" must return one string per value.", spec$name), call. = FALSE)
      }
      out[is.na(out)] <- "NA"
      out
    },

    visible_columns = function() private$.column_order[
      vapply(private$.column_order, function(j) private$.specs[[j]]$visible, TRUE)
    ],

    selected_positions = function() {
      n <- self$row_count
      if (!n) return(integer())
      a <- private$.selection_anchor
      b <- private$.selection_end
      if (is.na(a) || is.na(b)) return(if (private$.state$cursor_row > 0L) private$.state$cursor_row else integer())
      a <- min(n, max(1L, a))
      b <- min(n, max(1L, b))
      seq.int(min(a, b), max(a, b))
    },

    extend_selection = function(delta) {
      n <- self$row_count
      if (!n) return(invisible())
      if (is.na(private$.selection_anchor)) private$.selection_anchor <- max(1L, private$.state$cursor_row)
      private$.selection_end <- max(1L, min(n, private$.state$cursor_row + delta))
      self$move_cursor(row = private$.selection_end)
      self$invalidate()
    },

    move_active_column = function(delta) {
      vis <- private$visible_columns()
      k <- match(private$active_column(), vis)
      if (is.na(k)) return(invisible())
      target <- match(vis[[k]], private$.column_order) + delta
      if (target >= 1L && target <= length(private$.column_order)) self$reorder_column(vis[[k]], target)
      invisible()
    },

    # Layout of the visible part, from the last region. Columns: the frozen
    # ones first (always shown), then the scrollable ones from the column
    # offset on.
    geometry = function() {
      region <- self$region
      if (is.null(region)) return(NULL)
      st <- self$computed_style()
      inner <- content_rect(region, st)
      if (rect_is_empty(inner)) return(NULL)
      header_h <- as.integer(self$show_header)
      n <- self$row_count
      bar_h <- as.integer(!is.null(private$.bar) && inner$height > header_h + 1L)
      body_h <- max(0L, inner$height - header_h - bar_h)
      rn <- private$.row_names
      rn_w <- if (is.null(rn)) 0L else rn$width + 1L
      vis <- private$visible_columns()
      widths <- vapply(private$.specs[vis], `[[`, integer(1), "width")
      bar_y <- n > body_h
      body_w <- max(0L, inner$width - bar_y)
      avail <- max(0L, body_w - rn_w)
      # Horizontal overflow: reserve the last row for a scroll bar.
      bar_x <- sum(widths) + max(0L, length(widths) - 1L) > avail
      if (bar_x && body_h > 1L) {
        body_h <- body_h - 1L
        if (!bar_y && n > body_h) {
          bar_y <- TRUE
          body_w <- max(0L, inner$width - 1L)
          avail <- max(0L, body_w - rn_w)
        }
      }
      frozen_n <- min(as.integer(self$frozen_columns), length(vis))
      is_frozen <- seq_along(vis) <= frozen_n
      frozen_cols <- fit_columns(vis[is_frozen], widths[is_frozen], avail)
      frozen_w <- if (length(frozen_cols)) sum(widths[is_frozen][seq_along(frozen_cols)]) + length(frozen_cols) else 0L
      scroll_cols <- vis[!is_frozen]
      scroll_w <- widths[!is_frozen]
      scroll_avail <- max(0L, avail - frozen_w)
      max_col <- if (length(scroll_cols)) max_column_offset(scroll_w, scroll_avail) else 0L
      private$.state$offset_col <- as.integer(min(max(0L, private$.state$offset_col), max_col))
      private$.state$offset_row <- as.integer(min(max(0L, private$.state$offset_row), max(0L, n - body_h)))
      first <- private$.state$offset_col + 1L
      shown_scroll <- integer()
      if (length(scroll_cols) && first <= length(scroll_cols)) {
        shown_scroll <- seq.int(first, length(scroll_cols))[seq_along(fit_columns(scroll_cols[first:length(scroll_cols)],
                                                                                   scroll_w[first:length(scroll_cols)], scroll_avail))]
      }
      cols <- c(frozen_cols, scroll_cols[shown_scroll])
      shown <- c(widths[is_frozen][seq_along(frozen_cols)], scroll_w[shown_scroll])
      rows <- seq_len(min(body_h, n - private$.state$offset_row)) + private$.state$offset_row
      list(
        inner = inner, body_y = inner$y + header_h, body_h = body_h, body_w = body_w,
        rn_w = rn_w, columns = cols, widths = shown, rows = rows, bar_x = bar_x, bar_y = bar_y,
        all_widths = scroll_w, avail = scroll_avail, scroll_cols = scroll_cols, frozen_w = frozen_w,
        frozen_shown = length(frozen_cols), visible = vis, bar_row = inner$y + inner$height - 1L,
        has_bar = bar_h > 0L
      )
    },

    clamp_row_offset = function(offset) {
      g <- private$geometry()
      body_h <- if (is.null(g)) 1L else g$body_h
      as.integer(min(max(0L, offset), max(0L, self$row_count - body_h)))
    },

    reveal_cursor = function() {
      g <- private$geometry()
      if (is.null(g)) return(invisible())
      cur <- private$.state$cursor_row
      off <- private$.state$offset_row
      if (cur > 0L) {
        if (cur <= off) off <- cur - 1L
        if (cur > off + g$body_h) off <- cur - g$body_h
        self$set_state("offset_row", as.integer(max(0L, off)))
      }
      col <- private$.state$cursor_col
      if (self$cursor_type == "cell" && col > 0L) private$reveal_column(col, g)
      invisible()
    },

    # Scroll horizontally until data column `j` is visible.
    reveal_column = function(j, g = private$geometry()) {
      if (is.null(g)) return(invisible())
      k <- match(j, g$scroll_cols)
      if (is.na(k)) return(invisible())
      offc <- private$.state$offset_col
      if (k <= offc) offc <- k - 1L
      # Scroll right until the column fits entirely.
      while (offc < k - 1L && sum(g$all_widths[seq.int(offc + 1L, k)]) + (k - offc - 1L) > g$avail) offc <- offc + 1L
      self$set_state("offset_col", as.integer(offc))
      invisible()
    },

    step_rows = function(delta) {
      n <- self$row_count
      if (n == 0L) return(invisible())
      if (self$cursor_type == "none") {
        target <- as.numeric(private$.state$offset_row) + delta
        self$set_state("offset_row", private$clamp_row_offset(min(target, .Machine$integer.max)))
        return(invisible())
      }
      target <- as.numeric(private$.state$cursor_row) + delta
      self$move_cursor(row = max(1, min(n, target)))
    },

    step_cols = function(delta) {
      vis <- private$visible_columns()
      if (length(vis) == 0L) return(invisible())
      if (self$cursor_type == "cell") {
        k <- match(private$.state$cursor_col, vis)
        if (is.na(k)) k <- 1L
        self$move_cursor(column = vis[[min(length(vis), max(1L, k + delta))]])
      } else {
        self$set_state("offset_col", as.integer(max(0L, private$.state$offset_col + delta)))
      }
    },

    selection_data = function() {
      pos <- private$.state$cursor_row
      row <- private$row_at_position(pos)
      value <- if (is_table_source(private$.source)) as.list(private$source_read(pos, private$source_column_names())[1L, , drop = FALSE]) else self$row_data(row)
      out <- list(row = row, position = pos, value = value)
      col <- private$.state$cursor_col
      if (self$cursor_type == "cell" && col > 0L) {
        out$column <- names(private$.data)[[col]]
        out$value <- if (is_table_source(private$.source)) private$source_read(pos, names(private$.data)[[col]])[[1L]][[1L]] else private$.data[[col]][[row]]
      }
      out
    },

    announce_selection = function() {
      if (private$.state$cursor_row < 1L) return(invisible())
      type <- if (self$cursor_type == "cell") "datatable.cell_selected" else "datatable.row_selected"
      self$post_message(type, private$selection_data())
    },

    row_paint_style = function(pos, row, st, focused) {
      layered <- NULL
      if (pos %in% private$selected_positions()) layered <- table_styles$match
      if (self$zebra && pos %% 2L == 0L) layered <- table_styles$zebra
      if (!is.null(self$row_style)) {
        data <- if (is_table_source(private$.source)) private$source_read(row, private$source_column_names()) else private$.data
        layered <- merge_styles(layered, as_style(self$row_style(row, data)))
      }
      if (self$cursor_type == "row" && pos == private$.state$cursor_row) {
        layered <- merge_styles(layered, if (focused) table_styles$cursor else table_styles$cursor_blur)
      }
      resolve_style(layered %||% new_style(), parent = st) |> keep_background(st)
    },

    paint_cells = function(buffer, g, k, pos, row, pieces, row_st, focused, clip) {
      x <- g$inner$x + g$rn_w
      for (i in seq_along(g$columns)) {
        j <- g$columns[[i]]
        layered <- NULL
        if (!is.null(self$cell_style)) {
          value <- if (is_table_source(private$.source)) private$source_read(row, names(private$.data)[[j]])[[1L]][[1L]] else private$.data[[j]][[row]]
          layered <- as_style(self$cell_style(value, row, names(private$.data)[[j]]))
        }
        if (self$cursor_type == "cell" && pos == private$.state$cursor_row && j == private$.state$cursor_col) {
          layered <- merge_styles(layered, if (focused) table_styles$cursor else table_styles$cursor_blur)
        }
        hit <- private$.find_hit
        if (!is.null(hit) && hit$pos == pos && hit$column == j) layered <- merge_styles(layered, table_styles$match)
        if (!is.null(layered) && length(c(layered$props, layered$states))) {
          cst <- resolve_style(layered, parent = row_st) |> keep_background(row_st)
          buffer$put_text(x, g$body_y + k - 1L, pieces[[i]], fg = cst$foreground, bg = cst$background,
                          attrs = bitwOr(cst$attrs, row_st$attrs), clip = clip)
        }
        x <- x + g$widths[[i]] + 1L
      }
    },

    paint_scrollbars = function(buffer, g, st, area) {
      if (g$bar_y) {
        track <- rect(g$inner$x + g$body_w, g$body_y, 1L, g$body_h)
        draw_scrollbar(buffer, track, private$.state$offset_row, self$row_count, g$body_h, "vertical", st, area)
      }
      if (g$bar_x && g$body_h > 0L) {
        widths <- g$all_widths
        total <- sum(widths) + length(widths) - 1L
        before <- sum(widths[seq_len(private$.state$offset_col)]) + private$.state$offset_col
        track <- rect(g$inner$x + g$rn_w + g$frozen_w, g$body_y + g$body_h, g$avail, 1L)
        draw_scrollbar(buffer, track, before, total, g$avail, "horizontal", st, area)
      }
      invisible()
    },

    paint_bar = function(buffer, g, st, clip) {
      if (!g$has_bar) return(invisible())
      bar <- resolve_style(style(background = "$surface", foreground = "$foreground"), parent = st)
      err <- resolve_style(style(background = "$surface", foreground = "$error", bold = TRUE), parent = bar)
      width <- g$inner$width
      bad <- FALSE
      if (private$.bar == "goto") {
        left <- paste0(" Go to row (1-", self$row_count, "): ", private$.bar_text, "_")
        right <- ""
      } else {
        scope <- if (is.null(private$.find_column)) "all columns" else paste0("column ", private$.specs[[private$.find_column]]$label)
        flags <- paste0(if (isTRUE(private$.find_case)) "[Aa] " else "", if (isTRUE(private$.find_regex)) "[.*] " else "")
        status <- if (!is.null(private$.find_error)) {
          bad <- TRUE
          sub("^Invalid regular expression ", "bad regex: ", private$.find_error)
        } else if (is.null(private$.find) || !nzchar(private$.find_text)) {
          ""
        } else if (is.null(private$.find_hit)) {
          bad <- TRUE
          "no matches"
        } else {
          sprintf("row %d", private$.find_hit$pos)
        }
        left <- paste0(" Find: ", private$.find_text, "_")
        right <- paste0("(", scope, ") ", flags, status, " ")
      }
      room <- max(0L, width - str_width(right))
      line <- paste0(str_align(left, room), str_truncate(right, width))
      sty <- if (bad) err else bar
      buffer$put_text(g$inner$x, g$bar_row, str_align(line, width), fg = sty$foreground, bg = sty$background,
                      attrs = sty$attrs, clip = clip)
    },

    apply_sort = function(announce = TRUE) {
      so <- private$.sort
      if (is_table_source(private$.source)) {
        if (!is.null(private$.source$sort)) {
          result <- private$.source$sort(if (is.null(so)) NULL else
            list(columns = names(private$.data)[so$columns], decreasing = so$decreasing))
          if (identical(result, FALSE)) stop("The table source rejected the sort.", call. = FALSE)
        } else if (!is.null(so)) {
          stop("Sorting is not supported by this table source.", call. = FALSE)
        }
        private$clear_source_cache()
        private$set_view(NULL)
        if (announce) self$post_message("datatable.sorted", list(
          columns = if (!is.null(so)) names(private$.data)[so$columns], decreasing = so$decreasing
        ))
        return(invisible(self))
      }
      base <- private$.base_view
      base <- private$filter_rows(base)
      if (is.null(so)) {
        private$set_view(base)
      } else {
        rows <- base %||% seq_len(nrow(private$.data))
        keys <- lapply(so$columns, function(j) private$.data[[j]][rows])
        ord <- tryCatch(
          do.call(order, c(keys, list(decreasing = so$decreasing, na.last = TRUE, method = "radix"))),
          error = function(e) {
            private$.sort <- NULL
            stop("Cannot sort by this column: ", conditionMessage(e), call. = FALSE)
          }
        )
        private$set_view(rows[ord])
        # Auto-sized columns get room for the sort marker.
        marks <- private$sort_marks(so$columns)
        for (k in seq_along(so$columns)) {
          sp <- private$.specs[[so$columns[[k]]]]
          need <- str_width(sp$label) + str_width(marks[[k]])
          if (isTRUE(sp$auto) && sp$width < need) private$.specs[[so$columns[[k]]]]$width <- as.integer(need)
        }
      }
      if (announce) {
        self$post_message("datatable.sorted", list(
          columns = if (!is.null(so)) names(private$.data)[so$columns], decreasing = so$decreasing
        ))
      }
      invisible(self)
    },

    filter_rows = function(rows = NULL) {
      filters <- private$.filters
      if (!length(filters)) return(rows)
      n <- if (is.null(rows)) nrow(private$.data) else length(rows)
      if (!n) return(integer())
      chunk_size <- 5000L
      out <- list()
      count <- 0L
      changed <- FALSE
      starts <- seq.int(1L, n, by = chunk_size)
      for (start in starts) {
        positions <- seq.int(start, min(n, start + chunk_size - 1L))
        source_rows <- if (is.null(rows)) positions else rows[positions]
        keep <- rep(TRUE, length(source_rows))
        for (name in names(filters)) {
          j <- match(name, names(private$.data))
          values <- private$.data[[j]][source_rows]
          filter <- filters[[name]]
          if (inherits(filter, "termr_table_filter")) {
            type <- filter$type
            if (type == "missing") {
              matched <- is.na(values)
            } else if (type == "equals") {
              matched <- if (length(filter$value) == 1L && is.na(filter$value)) is.na(values) else !is.na(values) & values == filter$value
            } else if (type == "contains") {
              chars <- as.character(values)
              pattern <- if (filter$case_sensitive) filter$value else tolower(filter$value)
              text <- if (filter$case_sensitive) chars else tolower(chars)
              matched <- !is.na(text) & grepl(pattern, text, fixed = TRUE)
            } else if (type == "regex") {
              matched <- !is.na(values) & grepl(filter$value, as.character(values), perl = TRUE,
                                                 ignore.case = !filter$case_sensitive)
            } else {
              lower <- if (filter$inclusive) values >= filter$min else values > filter$min
              upper <- if (filter$inclusive) values <= filter$max else values < filter$max
              matched <- !is.na(values) & lower & upper
            }
          } else {
            matched <- filter(values)
          }
          if (!is.logical(matched) || length(matched) != length(keep)) {
            stop(sprintf("Filter for column `%s` must return one logical per value.", name), call. = FALSE)
          }
          matched[is.na(matched)] <- FALSE
          keep <- keep & matched
          if (!any(keep)) break
        }
        accepted <- source_rows[keep]
        if (length(accepted) != length(source_rows) && !changed) {
          changed <- TRUE
          if (count) out[[length(out) + 1L]] <- if (is.null(rows)) seq_len(count) else rows[seq_len(count)]
        }
        if (changed && length(accepted)) out[[length(out) + 1L]] <- accepted
        count <- count + length(accepted)
      }
      if (!changed) return(rows)
      unlist(out, use.names = FALSE)
    },

    # " ^", " v" (plus the rank with several keys) for sorted columns.
    sort_marks = function(cols) {
      so <- private$.sort
      marks <- rep("", length(cols))
      if (is.null(so)) return(marks)
      utf8 <- unicode_ok()
      for (k in seq_along(so$columns)) {
        i <- match(so$columns[[k]], cols)
        if (is.na(i)) next
        arrow <- if (so$decreasing[[k]]) (if (utf8) "\u25bc" else "v") else (if (utf8) "\u25b2" else "^")
        marks[[i]] <- paste0(" ", arrow, if (length(so$columns) > 1L) k else "")
      }
      marks
    },

    set_width = function(j, width, fixed, notify) {
      width <- suppressWarnings(as.integer(width))
      if (length(width) != 1L || is.na(width)) stop("`width` must be a number of cells.", call. = FALSE)
      width <- min(max(1L, width), 1000L)
      spec <- private$.specs[[j]]
      if (width == spec$width && identical(spec$auto, !fixed)) return(invisible())
      private$.specs[[j]]$width <- width
      private$.specs[[j]]$auto <- !fixed
      self$invalidate()
      if (notify) self$post_message("datatable.column_resized", list(column = spec$name, width = width))
      invisible()
    },

    # Width that fits the header and the formatted values of some rows.
    estimate_width = function(j, rows) {
      spec <- private$.specs[[j]]
      view <- if (is_table_source(private$.source)) NULL else private$view_rows()
      n <- private$row_count_view()
      pos <- switch(
        rows,
        all = if (is_table_source(private$.source)) stop("`rows = 'all'` is unavailable for lazy sources; use the backend to size the column.", call. = FALSE) else seq_len(n),
        visible = {
          g <- private$geometry()
          if (is.null(g)) sample_rows(n) else g$rows
        },
        sample = {
          g <- private$geometry()
          unique(c(sample_rows(n), if (!is.null(g)) g$rows))
        }
      )
      values <- if (length(pos)) private$format_values(j, if (is_table_source(private$.source)) pos else view[pos]) else character()
      w <- max(c(str_width(spec$label) + 2L * (!is.null(private$.sort) && j %in% private$.sort$columns),
                 str_width(values), 1L))
      cap <- spec$max_width %||% private$.options$max_width %||% 1000L
      w <- min(w, if (identical(rows, "all")) 1000L else cap)
      max(w, spec$min_width %||% 1L)
    },

    # The column that keyboard commands (sort, resize, edit) act on.
    active_column = function() {
      vis <- private$visible_columns()
      if (self$cursor_type == "cell" && private$.state$cursor_col %in% vis) return(private$.state$cursor_col)
      act <- private$.state$active_col
      if (!is.null(act) && act %in% vis) act else vis[[1]]
    },

    nudge_width = function(delta) {
      j <- private$active_column()
      private$set_width(j, private$.specs[[j]]$width + delta, fixed = TRUE, notify = TRUE)
    },

    # Data column whose header separator is at screen (x, y), or NA.
    separator_at = function(x, y) {
      g <- private$geometry()
      if (is.null(g) || !self$show_header || y != g$inner$y) return(NA_integer_)
      cx <- g$inner$x + g$rn_w
      for (i in seq_along(g$columns)) {
        right <- cx + g$widths[[i]]  # the gap cell after the column
        if (x == right || (g$widths[[i]] >= 3L && x == right - 1L)) return(g$columns[[i]])
        cx <- right + 1L
      }
      NA_integer_
    },

    cell_editable = function(j) {
      spec <- private$.specs[[j]]
      if (is_table_source(private$.source)) return(!is.null(private$.source$set_value) && isTRUE(spec$editable %||% TRUE))
      x <- private$.data[[j]]
      if (!is.null(spec$editable)) return(isTRUE(spec$editable) && !is.list(x))
      is.atomic(x) && !is.list(x) && (is.numeric(x) || is.logical(x) || is.character(x) || is.factor(x) ||
        inherits(x, "Date"))
    },

    # Columns the search looks at, in reading order.
    find_columns = function() {
      if (!is.null(private$.find_column)) private$.find_column else private$visible_columns()
    },

    # Move to the next / previous cell that matches the search.
    goto_hit = function(direction, include_current = FALSE) {
      q <- private$.find
      if (is_table_source(private$.source)) return(private$goto_source_hit(q, direction, include_current))
      view <- private$view_rows()
      n <- length(view)
      if (is.null(q) || !nzchar(q$pattern) || n == 0L) return(FALSE)
      cols <- private$find_columns()
      hit <- private$.find_hit
      pos0 <- hit$pos %||% max(1L, private$.state$cursor_row)
      col0 <- hit$column %||% if (self$cursor_type == "cell" && private$.state$cursor_col %in% cols) private$.state$cursor_col else cols[[1]]
      ci0 <- match(col0, cols)
      if (is.na(ci0)) ci0 <- 1L
      row_matches <- function(idx) {
        m <- vapply(cols, function(j) query_matches(q, private$format_values(j, view[idx])), logical(length(idx)))
        matrix(m, nrow = length(idx))
      }
      # 1. the rest of the current row
      found <- NULL
      m <- row_matches(pos0)[1, ]
      ok <- if (direction > 0L) which(seq_along(cols) > ci0 - (include_current && is.null(hit))) else which(seq_along(cols) < ci0)
      ok <- ok[m[ok]]
      if (length(ok)) found <- c(pos0, if (direction > 0L) cols[[min(ok)]] else cols[[max(ok)]])
      # 2. other rows, in chunks; then wrap around
      scan <- function(lo, hi) {
        if (lo > hi) return(NULL)
        chunk <- max(200L, 20000L %/% length(cols))
        starts <- if (direction > 0L) seq.int(lo, hi, by = chunk) else seq.int(hi, lo, by = -chunk)
        for (st in starts) {
          idx <- if (direction > 0L) seq.int(st, min(st + chunk - 1L, hi)) else seq.int(st, max(st - chunk + 1L, lo))
          mm <- row_matches(idx)
          rows_hit <- which(rowSums(mm) > 0L)
          if (length(rows_hit)) {
            r <- min(rows_hit)  # idx is already in scan order
            cc <- which(mm[r, ])
            return(c(idx[[r]], cols[[if (direction > 0L) min(cc) else max(cc)]]))
          }
        }
        NULL
      }
      if (is.null(found)) found <- if (direction > 0L) scan(pos0 + 1L, n) else scan(1L, pos0 - 1L)
      if (is.null(found)) found <- if (direction > 0L) scan(1L, pos0) else scan(pos0, n)
      if (is.null(found)) {
        private$.find_hit <- NULL
        self$invalidate_paint()
        return(FALSE)
      }
      private$.find_hit <- list(pos = found[[1]], column = found[[2]])
      private$.state$active_col <- found[[2]]
      if (self$cursor_type != "none") {
        private$.state$cursor_row <- found[[1]]
        if (self$cursor_type == "cell") private$.state$cursor_col <- found[[2]]
      }
      g <- private$geometry()
      if (!is.null(g)) {
        off <- private$.state$offset_row
        if (found[[1]] <= off || found[[1]] > off + g$body_h) {
          private$.state$offset_row <- as.integer(private$clamp_row_offset(found[[1]] - 1L - g$body_h %/% 2L))
        }
        private$reveal_column(found[[2]], g)
      }
      self$invalidate_paint()
      self$post_message("datatable.found", list(position = found[[1]], row = view[[found[[1]]]],
                                                column = names(private$.data)[[found[[2]]]]))
      TRUE
    },

    goto_source_hit = function(query, direction, include_current = FALSE) {
      if (is.null(query) || !nzchar(query$pattern)) return(FALSE)
      if (is.null(private$.source$search)) {
        private$.find_error <- "Search is not supported by this table source"
        private$.find_hit <- NULL
        self$invalidate_paint()
        return(FALSE)
      }
      start <- private$.find_hit$pos %||% max(1L, private$.state$cursor_row)
      result <- private$.source$search(query, names(private$.data)[private$find_columns()], start = start,
                                       direction = direction, include_current = include_current)
      if (is.null(result)) {
        private$.find_hit <- NULL
        self$invalidate_paint()
        return(FALSE)
      }
      if (!is.list(result) || length(result$position) != 1L || !is.numeric(result$position) ||
          is.na(result$position) || !is.finite(result$position) || result$position != floor(result$position) ||
          length(result$column) != 1L || is.na(result$column) ||
          result$position < 1L || result$position > private$source_row_count()) {
        stop("`table_source$search()` must return NULL or a valid `position` and `column`.", call. = FALSE)
      }
      j <- private$column_index(result$column)
      pos <- as.integer(result$position)
      private$.find_hit <- list(pos = pos, column = j)
      private$.state$active_col <- j
      if (self$cursor_type != "none") {
        private$.state$cursor_row <- pos
        if (self$cursor_type == "cell") private$.state$cursor_col <- j
      }
      g <- private$geometry()
      if (!is.null(g) && (pos <= private$.state$offset_row || pos > private$.state$offset_row + g$body_h)) {
        private$.state$offset_row <- as.integer(private$clamp_row_offset(pos - 1L - g$body_h %/% 2L))
      }
      self$invalidate_paint()
      self$post_message("datatable.found", list(position = pos, row = private$row_at_position(pos),
                                                  column = names(private$.data)[[j]]))
      TRUE
    },

    .find_case = FALSE,
    .find_regex = FALSE,

    find_set = function(text) {
      private$.find_text <- text
      private$.find_error <- NULL
      q <- tryCatch(
        search_query(text, isTRUE(private$.find_case), isTRUE(private$.find_regex)),
        error = function(e) {
          private$.find_error <- conditionMessage(e)
          NULL
        }
      )
      private$.find <- q
      private$.find_hit <- NULL
      if (!is.null(q)) private$goto_hit(1L, include_current = TRUE)
      self$invalidate()
      invisible()
    },

    bar_key = function(event) {
      key <- event$key
      if (private$.bar == "goto") {
        if (key == "escape") private$.bar <- NULL
        else if (key == "enter") {
          row <- suppressWarnings(as.integer(private$.bar_text))
          private$.bar <- NULL
          if (!is.na(row)) self$goto_row(row)
        } else if (key == "backspace") {
          private$.bar_text <- substr(private$.bar_text, 1L, nchar(private$.bar_text) - 1L)
        } else if (grepl("^[0-9]$", event$char)) {
          private$.bar_text <- paste0(private$.bar_text, event$char)
        } else return(invisible())
        self$invalidate()
        event$stop()
        return(invisible())
      }
      if (key == "escape") {
        private$.bar <- NULL
      } else if (key %in% c("enter", "down", "f3")) {
        self$find_next()
      } else if (key %in% c("up", "shift+f3")) {
        self$find_previous()
      } else if (key == "backspace") {
        chars <- split_graphemes(private$.find_text)
        private$find_set(paste(chars[-length(chars)], collapse = ""))
      } else if (key == "tab") {
        private$.find_column <- if (is.null(private$.find_column)) private$active_column() else NULL
        private$find_set(private$.find_text)
      } else if (key == "ctrl+r") {
        private$.find_regex <- !isTRUE(private$.find_regex)
        private$find_set(private$.find_text)
      } else if (key == "alt+c") {
        private$.find_case <- !isTRUE(private$.find_case)
        private$find_set(private$.find_text)
      } else if (event$is_printable()) {
        private$find_set(paste0(private$.find_text, event$char))
      } else {
        return(invisible())
      }
      self$invalidate()
      event$stop()
    },

    # Which part of the table is at screen position (x, y)?
    locate = function(x, y) {
      g <- private$geometry()
      if (is.null(g) || !rect_contains(g$inner, x, y)) return(NULL)
      column <- NA_integer_
      cx <- g$inner$x + g$rn_w
      for (i in seq_along(g$columns)) {
        if (x >= cx && x < cx + g$widths[[i]]) column <- g$columns[[i]]
        cx <- cx + g$widths[[i]] + 1L
      }
      if (self$show_header && y == g$inner$y) return(list(part = "header", column = column))
      k <- y - g$body_y + 1L
      if (k < 1L || k > length(g$rows)) return(NULL)
      list(part = "body", row = g$rows[[k]], column = column)
    }
  )
)

# The leading columns of `cols` that fit in `avail` cells (the last one may
# be cut off by the edge, as in scrolling tables).
fit_columns <- function(cols, widths, avail) {
  out <- integer()
  used <- 0L
  for (i in seq_along(cols)) {
    if (used >= avail) break
    out <- c(out, cols[[i]])
    used <- used + widths[[i]] + 1L
  }
  out
}

# Join cell strings with a space; after the frozen block a vertical bar.
table_join <- function(row_label, cells, frozen_shown = 0L) {
  n <- length(cells)
  if (n == 0L) return(row_label %||% "")
  seps <- rep(" ", n)
  seps[[n]] <- ""
  if (frozen_shown > 0L && frozen_shown < n) seps[[frozen_shown]] <- if (unicode_ok()) "\u2502" else "|"
  paste0(if (!is.null(row_label)) paste0(row_label, " "), paste0(cells, seps, collapse = ""))
}

# Text shown when editing a value, and the reverse: convert the edited text
# to the type of the column (a list with `value`, or an `error` message).
edit_text <- function(x) {
  if (is.na(x)) return("")
  if (is.numeric(x) && !is.factor(x)) return(format(x, digits = 15, scientific = FALSE, trim = TRUE))
  as.character(x)
}

parse_cell <- function(text, template) {
  fail <- function(msg) list(value = NULL, error = msg)
  if (is.factor(template)) {
    if (!length(text) || is.na(text[[1]]) || identical(text, "")) return(list(value = factor(NA, levels = levels(template)), error = NULL))
    if (!(as.character(text[[1]]) %in% levels(template))) {
      return(fail(paste0("Must be one of: ", paste(utils::head(levels(template), 6L), collapse = ", "),
                         if (nlevels(template) > 6L) ", ...")))
    }
    return(list(value = factor(as.character(text[[1]]), levels = levels(template)), error = NULL))
  }
  if (!is.character(text)) {
    # Already a value (set_cell from code): it must fit the column.
    ok <- if (is.numeric(template)) is.numeric(text) else if (is.logical(template)) is.logical(text) else
      if (inherits(template, "Date")) inherits(text, "Date") else is.character(text)
    if (length(text) != 1L || !ok) return(fail(sprintf("The value does not fit a %s column.", class(template)[[1]])))
    if (is.integer(template) && is.double(text)) {
      if (!is.na(text) && (text != round(text) || abs(text) > .Machine$integer.max)) return(fail("Enter a whole number."))
      text <- as.integer(text)
    }
    return(list(value = text, error = NULL))
  }
  if (length(text) != 1L) return(fail("Enter a single value."))
  text <- trimws(text)
  if (inherits(template, "Date")) {
    if (!nzchar(text)) return(list(value = as.Date(NA), error = NULL))
    v <- tryCatch(as.Date(text, optional = TRUE), error = function(e) as.Date(NA))
    if (is.na(v)) return(fail("Enter a date as YYYY-MM-DD."))
    return(list(value = v, error = NULL))
  }
  if (is.logical(template)) {
    if (!nzchar(text) || tolower(text) == "na") return(list(value = NA, error = NULL))
    v <- switch(tolower(text), `true` = , t = , yes = , y = , `1` = TRUE, `false` = , f = , no = , n = , `0` = FALSE,
                "invalid")
    if (identical(v, "invalid")) return(fail("Enter TRUE or FALSE."))
    return(list(value = v, error = NULL))
  }
  if (is.numeric(template)) {
    if (!nzchar(text) || identical(text, "NA")) return(list(value = if (is.integer(template)) NA_integer_ else NA_real_, error = NULL))
    v <- suppressWarnings(as.numeric(text))
    if (is.na(v)) return(fail("Enter a number."))
    if (is.integer(template)) {
      if (v != round(v) || abs(v) > .Machine$integer.max) return(fail("Enter a whole number."))
      v <- as.integer(v)
    }
    return(list(value = v, error = NULL))
  }
  list(value = sanitize_text(text), error = NULL)
}

# Styles of table parts (layered over the widget style).
table_styles <- list(
  match = style(background = "$warning", foreground = "$background", underline = TRUE),
  header = style(bold = TRUE, background = "$surface"),
  row_label = style(foreground = "$muted"),
  zebra = style(background = "$stripe"),
  cursor = style(background = "$primary", foreground = "$on_primary"),
  cursor_blur = style(background = "$surface")
)

# Rows without their own background use the widget's background.
keep_background <- function(st, parent) {
  if (is.null(st$background)) st$background <- parent$background
  st
}

as_table_data <- function(x) {
  if (is.matrix(x)) {
    if (is.null(colnames(x))) colnames(x) <- paste0("V", seq_len(ncol(x)))
    x <- as.data.frame(x, stringsAsFactors = FALSE)
  }
  if (!is.data.frame(x)) stop("`data` must be a data frame or a matrix.", call. = FALSE)
  if (anyDuplicated(names(x)) || any(!nzchar(names(x)))) {
    stop("Table columns must have unique, non-empty names.", call. = FALSE)
  }
  x
}

# Sample rows used to estimate widths: the first and last 500.
sample_rows <- function(n) {
  if (n <= 1000L) seq_len(n) else c(seq_len(500L), seq.int(n - 499L, n))
}

table_column_spec <- function(x, name, opts) {
  opt <- opts$columns[[name]] %||% column()
  formatter <- opt$formatter %||% opts$formatters[[name]] %||% default_formatter(x)
  label <- opt$label %||% name
  align <- opt$align %||% (if (is.numeric(x) && !is.factor(x)) "right" else "left")
  width <- opt$width
  if (is.null(width)) {
    sample <- formatter(x[sample_rows(length(x))])
    sample[is.na(sample)] <- "NA"
    width <- max(c(str_width(label), str_width(sample), 1L))
    width <- min(width, opt$max_width %||% opts$max_width)
    width <- max(width, opt$min_width %||% 1L)
  }
  list(name = name, label = label, align = align, width = as.integer(width), formatter = formatter,
       visible = opt$visible %||% TRUE, sortable = opt$sortable %||% TRUE, editable = opt$editable,
       auto = is.null(opt$width), min_width = opt$min_width, max_width = opt$max_width,
       auto_formatter = is.null(opt$formatter) && is.null(opts$formatters[[name]]))
}

table_row_name_spec <- function(data) {
  n <- nrow(data)
  auto <- .row_names_info(data) < 0L
  labels <- if (auto) as.character(sample_rows(n)) else row.names(data)[sample_rows(n)]
  list(auto = auto, width = as.integer(min(40L, max(c(1L, str_width(labels))))))
}

table_row_labels <- function(data, spec, rows) {
  if (spec$auto) as.character(rows) else row.names(data)[rows]
}

# A formatter chosen from the type of the column. Doubles get a fixed
# number of decimals (estimated from a sample) so a column lines up.
default_formatter <- function(x) {
  if (is.double(x) && !inherits(x, c("Date", "POSIXt", "difftime"))) {
    sample <- x[sample_rows(length(x))]
    digits <- decimals_needed(sample[is.finite(sample)])
    return(function(v) {
      out <- formatC(v, format = "f", digits = digits)
      out[is.na(v)] <- "NA"
      out
    })
  }
  if (inherits(x, c("Date", "POSIXt", "difftime"))) {
    return(function(v) {
      out <- format(v)
      out[is.na(v)] <- "NA"
      out
    })
  }
  if (is.list(x)) {
    return(function(v) vapply(v, function(e) paste(format(e), collapse = ", "), character(1)))
  }
  function(v) {
    out <- as.character(v)
    out[is.na(v)] <- "NA"
    out
  }
}

decimals_needed <- function(x) {
  if (length(x) == 0L) return(0L)
  s <- format(x, digits = 6, scientific = FALSE, trim = TRUE)
  frac <- ifelse(grepl(".", s, fixed = TRUE), nchar(sub("^[^.]*\\.", "", s)), 0L)
  as.integer(min(6L, max(frac)))
}

# Pad or truncate strings to `width` columns (vectorised over values).
pad_cells <- function(values, width, align = "left") {
  values <- sanitize_text(values)
  n <- length(values)
  width <- rep_len(width, n)
  align <- rep_len(align, n)
  w <- str_width(values)
  long <- which(w > width)
  for (i in long) {
    values[[i]] <- if (width[[i]] <= 1L) str_truncate(values[[i]], width[[i]]) else
      paste0(str_truncate(values[[i]], width[[i]] - 1L), "\u2026")
  }
  w[long] <- str_width(values[long])
  gap <- pmax(0L, width - w)
  left <- ifelse(align == "right", gap, ifelse(align == "center", gap %/% 2L, 0L))
  paste0(strrep(" ", left), values, strrep(" ", gap - left))
}

# Largest column offset that still fills the available width.
max_column_offset <- function(widths, avail) {
  p <- length(widths)
  if (p == 0L) return(0L)
  for (k in 0:(p - 1L)) {
    rest <- widths[seq.int(k + 1L, p)]
    if (sum(rest) + length(rest) - 1L <= avail) return(k)
  }
  p - 1L
}

#' Data table
#'
#' A fast, read-only viewer for data frames: a header, optional row names,
#' automatic column widths, keyboard and mouse navigation, horizontal and
#' vertical scrolling, and a row or cell cursor. Only the visible rows are
#' formatted and painted, so tables with millions of rows scroll smoothly.
#'
#' Keys: arrows move the cursor (with a row cursor, Left/Right scroll the
#' columns), Page Up/Down move a page, Home/End go to the first / last row,
#' Enter activates the row (or edits the cell when `editable`), Ctrl+F finds
#' text (Enter/Down next, Up previous, Tab all columns / this column,
#' Ctrl+R regex, Alt+C case, Esc close), F3 / Shift+F3 next / previous match,
#' Ctrl+G jumps to a row, `S` (with `header_sort`) sorts by the active column
#' (ascending, descending, off), Alt+Left / Alt+Right narrow / widen the active column.
#' Clicking a row selects it; clicking the header sends
#' `"datatable.header_selected"` (and sorts with `header_sort`); dragging the
#' separator between two header cells resizes a column; the wheel scrolls.
#' The header and the first `frozen_columns` columns stay in place while the
#' rest scrolls.
#'
#' Messages (`event$data`):
#' * `"datatable.row_selected"` / `"datatable.cell_selected"`: the cursor
#'   moved; `row` (original row number), `position` (in the view),
#'   `value` (the row as a list, or the cell value), `column` (cell cursor);
#' * `"datatable.row_activated"`: Enter; same data;
#' * `"datatable.header_selected"`: `column` (name) and `index`;
#' * `"datatable.sorted"` (`columns`, `decreasing`),
#'   `"datatable.column_resized"` (`column`, `width`),
#'   `"datatable.columns_changed"` (`column`, `visible`),
#'   `"datatable.found"` (`position`, `row`, `column`),
#'   `"datatable.cell_changed"` (`row`, `column`, `old`, `value`).
#'
#' Methods: `set_data()`, `sort(by, decreasing)` (several columns allowed),
#' `clear_sort()`, `toggle_sort()`, `filter(rows)`, `move_cursor(row, column)`,
#' `goto_row()`, `scroll_to_row()`, `selected_row()`, `row_data(row)`,
#' `set_column_width()`, `auto_size_column()`, `auto_size_all()`,
#' `set_column_visible()`, `column_info()`, `find()`, `find_next()`,
#' `find_previous()`, `edit_cell()`, `set_cell()`.
#'
#' @param data A data frame (tibbles work too) or a matrix.
#' @param cursor `"row"` (default), `"cell"` or `"none"`.
#' @param columns Named list of [column()] options, e.g.
#'   `list(score = column(width = 10, align = "right"))`.
#' @param formatters Named list of `function(x)` returning strings, e.g.
#'   `list(score = function(x) sprintf("%.2f", x))`.
#' @param row_names Show row names? `NA` (default) shows them only when they
#'   are not the automatic `1..n` (e.g. `mtcars`).
#' @param header Show the header row?
#' @param zebra Stripe alternate rows?
#' @param row_style `function(row, data)` returning a [style()] (or
#'   `NULL`) for a data row.
#' @param cell_style `function(value, row, column)` returning a [style()]
#'   (or `NULL`) for a cell.
#' @param max_column_width Upper limit for automatic column widths.
#' @param frozen_columns Number of leading columns that stay in place while
#'   the others scroll horizontally.
#' @param header_sort Sort when a header is clicked (again: descending, then
#'   off; Shift+click adds a secondary key)?
#' @param editable Allow editing cells (Enter or double click opens a small
#'   dialog)? The table edits its own copy of the data; read it with `$data`
#'   or react to `"datatable.cell_changed"`. **Experimental.**
#' @param on_edit `function(row, column, value)` called before an edit is
#'   applied; return `NULL`/`TRUE` to accept or a message to reject.
#' @param id Optional identifier.
#' @param classes Optional classes.
#' @param style A [style()]; by default the table fills its parent.
#' @return A `DataTable` widget.
#' @export
#' @examples
#' tbl <- data_table(mtcars, id = "cars", zebra = TRUE,
#'                   formatters = list(mpg = function(x) sprintf("%.1f", x)))
#' render_widget(tbl, 60, 6)
data_table <- function(data, cursor = "row", columns = NULL, formatters = NULL, row_names = NA,
                       header = TRUE, zebra = FALSE, row_style = NULL, cell_style = NULL,
                       max_column_width = 40L, frozen_columns = 0L, header_sort = FALSE,
                       editable = FALSE, on_edit = NULL, id = NULL, classes = NULL, style = NULL) {
  DataTable$new(
    data, cursor = cursor, columns = columns, formatters = formatters, row_names = row_names,
    header = header, zebra = zebra, row_style = row_style, cell_style = cell_style,
    max_column_width = max_column_width, frozen_columns = frozen_columns, header_sort = header_sort,
    editable = editable, on_edit = on_edit, id = id, classes = classes, style = style
  )
}

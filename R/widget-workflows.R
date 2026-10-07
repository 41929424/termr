# Compact data and developer workflow widgets.

#' @title StatusBar widget
#' @description See [status_bar()].
#' @rdname StatusBar-class
#' @export
StatusBar <- R6::R6Class(
  "StatusBar",
  inherit = Widget,
  public = list(
    #' @description State fields are reactive and can be updated directly.
    paint_states = c("left", "center", "right"),
    #' @description Create a status bar. See [status_bar()].
    #' @param left,center,right Region text.
    #' @param id,classes,style See [Widget].
    initialize = function(left = "", center = "", right = "", id = NULL,
                          classes = NULL, style = NULL, disabled = FALSE) {
      super$initialize(id = id, classes = classes, style = style, disabled = disabled)
      private$.state$left <- status_text(left)
      private$.state$center <- status_text(center)
      private$.state$right <- status_text(right)
    },
    #' @description The built-in one-row style.
    default_style = function() style(width = "1fr", height = 1,
                                     foreground = "$foreground", background = "$surface"),
    #' @description Paint the three text regions.
    #' @param buffer A [ScreenBuffer].
    #' @param area Visible rectangle.
    #' @param st Computed style.
    paint = function(buffer, area, st) {
      draw_background(buffer, area, st)
      inner <- content_rect(self$region, st)
      width <- inner$width
      if (width < 1L || inner$height < 1L) return(invisible())
      left <- private$.state$left
      center <- private$.state$center
      right <- private$.state$right
      # Preserve left, then right, then center when the available width is
      # insufficient. Each region is truncated on grapheme boundaries.
      left_width <- min(width, str_width(left))
      right_width <- min(width - left_width, str_width(right))
      center_width <- max(0L, width - left_width - right_width)
      left_text <- str_truncate(left, left_width)
      right_text <- str_truncate(right, right_width)
      center_text <- str_truncate(center, center_width)
      row <- strrep(" ", width)
      cells <- text_cells(left_text)
      if (length(cells$chars)) row <- paste0(paste(cells$chars, collapse = ""), substring(row, str_width(left_text) + 1L))
      if (right_width > 0L) {
        start <- width - right_width + 1L
        prefix <- if (start > 1L) str_truncate(row, start - 1L) else ""
        row <- paste0(prefix, strrep(" ", max(0L, width - str_width(prefix) - right_width)), right_text)
      }
      if (center_width > 0L && nzchar(center_text)) {
        slot_start <- left_width + 1L
        slot_end <- width - right_width
        slot_width <- max(0L, slot_end - slot_start + 1L)
        x <- slot_start + max(0L, (slot_width - str_width(center_text)) %/% 2L)
        left_gap <- max(0L, x - left_width - 1L)
        right_gap <- max(0L, width - right_width - x - str_width(center_text) + 1L)
        row <- paste0(left_text, strrep(" ", left_gap), center_text,
                      strrep(" ", right_gap), right_text)
      }
      buffer$put_text(inner$x, inner$y, str_truncate(row, width), fg = st$foreground,
                      bg = st$background, attrs = st$attrs, clip = area)
      invisible()
    }
  ),
  active = list(
    #' @field left Text in the left region.
    left = function(value) {
      if (missing(value)) return(private$.state$left)
      self$set_state("left", status_text(value))
    },
    #' @field center Text in the center region.
    center = function(value) {
      if (missing(value)) return(private$.state$center)
      self$set_state("center", status_text(value))
    },
    #' @field right Text in the right region.
    right = function(value) {
      if (missing(value)) return(private$.state$right)
      self$set_state("right", status_text(value))
    }
  )
)

status_text <- function(x) {
  if (is.null(x)) return("")
  paste(as.character(as_text(x)), collapse = " ")
}

#' One-line status bar
#'
#' Displays left, centered and right status text in one terminal row. On a
#' narrow screen it preserves the left region first, then the right region,
#' and truncates the center region first. Truncation respects terminal cell
#' widths and grapheme boundaries. Each value may be a no-argument reactive
#' function that reads [signal()]s.
#'
#' @param left,center,right Text for the three regions.
#' @param id Optional identifier.
#' @param classes Optional classes.
#' @param style A [style()].
#' @param disabled Disable the widget?
#' @return A `StatusBar` widget with reactive `left`, `center`, and `right`
#'   fields.
#' @export
#' @examples
#' status_bar("SQLite :memory:", "127 rows", "8 ms")
status_bar <- function(left = "", center = "", right = "", id = NULL,
                       classes = NULL, style = NULL, disabled = FALSE) {
  fns <- list(left = if (is.function(left)) left else NULL,
              center = if (is.function(center)) center else NULL,
              right = if (is.function(right)) right else NULL)
  w <- StatusBar$new(if (is.null(fns$left)) left else "",
                     if (is.null(fns$center)) center else "",
                     if (is.null(fns$right)) right else "",
                     id = id, classes = classes, style = style, disabled = disabled)
  for (field in names(fns)) if (!is.null(fns[[field]])) w$bind_reactive(field, fns[[field]])
  w
}

# Compact scalar formatting shared by inspectors and profiles.
format_compact_value <- function(x, max_items = 3L) {
  if (is.null(x)) return("NULL")
  if (!length(x)) return(paste0("<", paste(class(x), collapse = "/"), "(0)>") )
  if (is.list(x) || is.environment(x) || is.function(x) || isS4(x)) {
    kind <- if (is.environment(x)) "environment" else if (is.function(x)) "function" else
      if (isS4(x)) paste(class(x), collapse = "/") else "list"
    return(paste0("<", kind, if (length(x)) paste0("[", length(x), "]") else "", ">"))
  }
  if (length(x) == 1L) {
    if (is.na(x)) return("NA")
    if (inherits(x, "POSIXt")) return(format(x, usetz = TRUE))
    if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
    if (is.factor(x)) return(as.character(x))
    if (is.numeric(x)) return(format(x, digits = 4L, scientific = FALSE, trim = TRUE, big.mark = ","))
    return(as.character(x))
  }
  values <- utils::head(x, max_items)
  shown <- vapply(seq_along(values), function(i) format_compact_value(values[i], max_items = 1L), "")
  suffix <- if (length(x) > length(values)) ", \u2026" else ""
  paste0("[", paste(shown, collapse = ", "), suffix, "]")
}

normalize_property_data <- function(data) {
  if (is.null(data)) return(list())
  if (is.data.frame(data)) data <- as.list(data)
  else if (!is.list(data)) data <- as.list(data)
  if (!length(data)) return(data)
  nm <- names(data)
  if (is.null(nm) || anyNA(nm) || any(!nzchar(nm))) {
    stop("`data` must be a named list, named vector, or data frame.", call. = FALSE)
  }
  data
}

#' @title PropertyGrid widget
#' @description See [property_grid()].
#' @rdname PropertyGrid-class
#' @export
PropertyGrid <- R6::R6Class(
  "PropertyGrid",
  inherit = ScrollView,
  public = list(
    #' @field max_key_width Maximum key width in terminal cells.
    max_key_width = 20L,
    #' @field formatter Optional `function(name, value)`.
    formatter = NULL,
    #' @description Create a property grid. See [property_grid()].
    #' @param data Named list/vector or data frame.
    #' @param formatter Optional value formatter.
    #' @param max_key_width Maximum key width.
    #' @param id,classes,style,disabled See [property_grid()].
    initialize = function(data = list(), formatter = NULL, max_key_width = 20L,
                          id = NULL, classes = NULL, style = NULL, disabled = FALSE) {
      check_function(formatter, "formatter", allow_null = TRUE)
      self$max_key_width <- max(1L, check_count(max_key_width, "max_key_width"))
      self$formatter <- formatter
      super$initialize(direction = "vertical", scrollbars = TRUE, focusable = TRUE,
                       id = id, classes = classes, style = style, disabled = disabled)
      private$.state$data <- normalize_property_data(data)
      private$rebuild()
    },
    #' @description The built-in scrollable style.
    default_style = function() style(width = "1fr", height = "1fr", layout = "vertical"),
    #' @description Replace all rows.
    #' @param data New named data.
    set_data = function(data) {
      self$data <- data
      invisible(self)
    }
  ),
  active = list(
    #' @field data The current named data (reactive).
    data = function(value) {
      if (missing(value)) return(private$.state$data)
      private$.state$data <- normalize_property_data(value)
      private$rebuild()
      self$invalidate()
      invisible(value)
    }
  ),
  private = list(
    rebuild = function() {
      data <- private$.state$data
      if (!length(data)) {
        self$replace()
        return(invisible())
      }
      width <- min(self$max_key_width, max(1L, max(str_width(names(data)))))
      rows <- lapply(seq_along(data), function(i) {
        key <- names(data)[[i]]
        key_text <- str_truncate(key, width)
        value <- if (is.null(self$formatter)) format_compact_value(data[[i]]) else {
          result <- self$formatter(key, data[[i]])
          if (is.null(result)) "" else paste(as.character(as_text(result)), collapse = " ")
        }
        horizontal(
          label(key_text, style = style(width = width + 1L, foreground = "$muted")),
          label(value, wrap = "word", style = style(width = "1fr")),
          style = style(height = "auto")
        )
      })
      self$replace(rows)
      invisible()
    }
  )
)

#' Key/value property inspector
#'
#' Displays named list/vector entries in two aligned columns. Keys are sized
#' to their longest name up to `max_key_width`; values wrap and the view
#' scrolls when needed. `NULL`, `NA`, dates and vectors have compact default
#' formatting. Lists and other objects are shown as placeholders instead of
#' being recursively printed. A custom `formatter(name, value)` can replace
#' the default value formatting. `data` may be a reactive function of no
#' arguments.
#'
#' @param data Named list or vector; data frames are accepted as named
#'   columns. `NULL` displays an empty grid.
#' @param format Optional `function(name, value)` returning display text.
#' @param max_key_width Maximum key column width in terminal cells.
#' @param id Optional identifier.
#' @param classes Optional classes.
#' @param style A [style()].
#' @param disabled Disable focus and scrolling interaction?
#' @return A `PropertyGrid` (a focusable, scrollable widget) with reactive
#'   `$data` and `$set_data()`.
#' @export
#' @examples
#' property_grid(list(type = "numeric", missing = 12, unique = 345))
property_grid <- function(data = list(), format = NULL, max_key_width = 20L,
                          id = NULL, classes = NULL, style = NULL, disabled = FALSE) {
  fn <- if (is.function(data)) data
  w <- PropertyGrid$new(if (is.null(fn)) data else list(), formatter = format,
                        max_key_width = max_key_width, id = id, classes = classes,
                        style = style, disabled = disabled)
  if (!is.null(fn)) w$bind_reactive("data", fn)
  w
}

record_data <- function(data) {
  if (is.data.frame(data)) {
    if (nrow(data) != 1L) {
      stop(sprintf("`record_view()` expects one record; received %d rows.", nrow(data)), call. = FALSE)
    }
    out <- as.list(data[1L, , drop = FALSE])
  } else if (is.list(data)) {
    out <- data
  } else if (is.atomic(data) || is.factor(data)) {
    out <- as.list(data)
  } else if (is.null(data)) {
    return(list())
  } else {
    stop("`record_view()` expects a named list, named vector, or one-row data frame.", call. = FALSE)
  }
  normalize_property_data(out)
}

#' View one record
#'
#' `record_view()` accepts a named list/vector or exactly one data-frame row
#' and presents it through the same renderer as [property_grid()]. It is a
#' semantic convenience wrapper, not a separate rendering system. A reactive
#' function may return a new record whenever its signals change.
#'
#' @param data One named list, named vector, or one-row data frame. `NULL`
#'   displays an empty view.
#' @param format Optional `function(name, value)` formatter.
#' @param max_key_width Maximum key column width.
#' @param id Optional identifier.
#' @param classes Optional classes.
#' @param style A [style()].
#' @param disabled Disable focus and scrolling interaction?
#' @return A `PropertyGrid` widget.
#' @export
#' @examples
#' record_view(list(id = 42, name = "Alice", active = TRUE))
record_view <- function(data, format = NULL, max_key_width = 20L,
                        id = NULL, classes = NULL, style = NULL, disabled = FALSE) {
  fn <- if (is.function(data)) data
  initial <- if (is.null(fn)) record_data(data) else list()
  w <- PropertyGrid$new(initial, formatter = format, max_key_width = max_key_width,
                        id = id, classes = classes, style = style, disabled = disabled)
  if (!is.null(fn)) w$bind_reactive("data", function() record_data(fn()))
  w
}

profile_sample <- function(x, sample = NULL) {
  n <- length(x)
  if (is.null(sample)) return(list(values = x, n = n, sampled = FALSE))
  if (!is.numeric(sample) || length(sample) != 1L || is.na(sample) || !is.finite(sample) ||
      sample < 1 || sample != floor(sample)) stop("`sample` must be NULL or a positive whole number.", call. = FALSE)
  sample <- as.integer(sample)
  if (n <= sample) return(list(values = x, n = n, sampled = FALSE))
  idx <- unique(as.integer(round(seq(1, n, length.out = sample))))
  list(values = x[idx], n = n, sampled = TRUE)
}

profile_summary <- function(x, sample = NULL, top_n = 5L) {
  if (is_table_source(x)) {
    stop("`data_profile()` does not scan a lazy table source; supply an explicit vector or sample.", call. = FALSE)
  }
  if (is.data.frame(x) || is.list(x) || !is.atomic(x) && !is.factor(x)) {
    stop("`data_profile()` expects an atomic vector, factor, Date, or POSIXct vector.", call. = FALSE)
  }
  sampled <- profile_sample(x, sample)
  values <- sampled$values
  n <- sampled$n
  missing <- sum(is.na(values))
  non_missing <- values[!is.na(values)]
  unique_n <- length(unique(non_missing))
  type <- if (inherits(x, "POSIXt")) "POSIXct" else if (inherits(x, "Date")) "Date" else
    if (is.factor(x)) "factor" else if (is.logical(x)) "logical" else if (is.integer(x)) "integer" else
      if (is.numeric(x)) "numeric" else if (is.character(x)) "character" else paste(class(x), collapse = "/")
  out <- list(Type = type, Length = n)
  if (sampled$sampled) out$Analyzed <- length(values)
  out$Missing <- missing
  out$Unique <- unique_n
  chart <- NULL

  if (is.logical(x)) {
    out[["TRUE"]] <- sum(values %in% TRUE, na.rm = TRUE)
    out[["FALSE"]] <- sum(values %in% FALSE, na.rm = TRUE)
    out[["NA"]] <- missing
  } else if (inherits(x, "Date") || inherits(x, "POSIXt")) {
    finite <- non_missing[is.finite(as.numeric(non_missing))]
    if (length(finite)) {
      nums <- as.numeric(finite)
      restore <- if (inherits(x, "Date")) function(z) as.Date(z, origin = "1970-01-01") else
        function(z) as.POSIXct(z, origin = "1970-01-01", tz = "UTC")
      out$Min <- restore(min(nums))
      out$Median <- restore(stats::median(nums))
      out$Max <- restore(max(nums))
    } else {
      out$Min <- NA
      out$Median <- NA
      out$Max <- NA
    }
  } else if (is.numeric(x)) {
    finite <- non_missing[is.finite(non_missing)]
    out$`Non-finite` <- sum(!is.finite(non_missing))
    if (length(finite)) {
      q <- stats::quantile(finite, c(0, .25, .5, .75, 1), names = FALSE, type = 7)
      out$Mean <- mean(finite)
      out$SD <- if (length(finite) > 1L) stats::sd(finite) else NA_real_
      out$Min <- q[[1L]]
      out$Q25 <- q[[2L]]
      out$Median <- q[[3L]]
      out$Q75 <- q[[4L]]
      out$Max <- q[[5L]]
      chart <- profile_histogram(finite)
    } else {
      out$Mean <- out$SD <- out$Min <- out$Q25 <- out$Median <- out$Q75 <- out$Max <- NA_real_
      chart <- numeric(8L)
    }
  } else if (is.character(x) || is.factor(x)) {
    text <- as.character(non_missing)
    # Bound the frequency table for very high-cardinality vectors.
    count_values <- text
    if (length(count_values) > 100000L) {
      idx <- unique(as.integer(round(seq(1, length(count_values), length.out = 100000L))))
      count_values <- count_values[idx]
      out$`Top values sampled` <- length(count_values)
    }
    counts <- sort(table(count_values), decreasing = TRUE)
    if (length(counts)) {
      for (i in seq_len(min(top_n, length(counts)))) {
        out[[paste0("Top ", i)]] <- paste0(format_compact_value(names(counts)[[i]]), " (", counts[[i]], ")")
      }
    }
    out$`Min string width` <- if (length(text)) min(str_width(text)) else NA_integer_
    out$`Max string width` <- if (length(text)) max(str_width(text)) else NA_integer_
  }
  list(name = NULL, stats = out, chart = chart, length = n, sampled = sampled$sampled)
}

profile_histogram <- function(x, bins = 8L) {
  counts <- integer(bins)
  x <- x[is.finite(x)]
  if (!length(x)) return(counts)
  lo <- min(x)
  hi <- max(x)
  if (lo == hi) {
    counts[[ceiling(bins / 2)]] <- length(x)
  } else {
    index <- pmin(bins, pmax(1L, floor((x - lo) / (hi - lo) * bins) + 1L))
    counts <- tabulate(index, nbins = bins)
  }
  counts
}

#' @title DataProfile widget
#' @description See [data_profile()].
#' @rdname DataProfile-class
#' @export
DataProfile <- R6::R6Class(
  "DataProfile",
  inherit = Vertical,
  public = list(
    #' @field sample The deterministic sample size, or `NULL` for full scan.
    sample = NULL,
    #' @field top_n Number of frequent text values shown.
    top_n = 5L,
    #' @description Create a vector profile. See [data_profile()].
    #' @param data An in-memory supported vector.
    #' @param name Heading.
    #' @param sample Optional deterministic sample size.
    #' @param top_n Frequent values to show.
    #' @param id,classes,style See [Widget].
    initialize = function(data, name = "value", sample = NULL, top_n = 5L,
                          id = NULL, classes = NULL, style = NULL, disabled = FALSE) {
      check_scalar_character(name, "name")
      self$sample <- sample
      self$top_n <- max(1L, check_count(top_n, "top_n"))
      super$initialize(id = id, classes = classes, style = style, disabled = disabled)
      private$.state$data <- data
      private$.state$name <- name
      private$rebuild()
    },
    #' @description The built-in style.
    default_style = function() style(width = "1fr", height = "auto", layout = "vertical")
  ),
  active = list(
    #' @field data The vector being summarized (reactive).
    data = function(value) {
      if (missing(value)) return(private$.state$data)
      private$.state$data <- value
      private$rebuild()
      self$invalidate()
      invisible(value)
    },
    #' @field summary Computed summary statistics and histogram data.
    summary = function(value) {
      if (!missing(value)) read_only("summary")
      private$.summary
    }
  ),
  private = list(
    .summary = NULL,
    rebuild = function() {
      x <- private$.state$data
      if (is.function(x)) x <- x()
      private$.summary <- profile_summary(x, sample = self$sample, top_n = self$top_n)
      values <- private$.summary$stats
      rows <- list(label(private$.state$name, style = style(bold = TRUE)))
      rows[[length(rows) + 1L]] <- key_value(values)
      if (!is.null(private$.summary$chart)) {
        rows[[length(rows) + 1L]] <- label("Distribution", style = style(foreground = "$muted"))
        rows[[length(rows) + 1L]] <- sparkline(private$.summary$chart, min = 0,
                                               max = max(private$.summary$chart, 1))
      }
      self$replace(rows)
      invisible()
    }
  )
)

#' Compact vector profile
#'
#' Summarizes numeric/integer, character/factor, logical, Date, and POSIXct
#' vectors. Numeric summaries use base R type-7 quartiles and sample standard
#' deviation (`stats::sd`). Numeric data also gets an eight-bin histogram
#' rendered with the existing sparkline widget. Without `sample`, vector
#' summaries analyze the full in-memory vector. Character top-value counts
#' cap their frequency table at 100,000 analyzed non-missing values. Supplying
#' `sample` analyzes deterministic, evenly spaced positions. A `table_source`
#' is rejected to prevent an implicit full scan; fetch or sample data
#' explicitly first. `data` may also be a reactive function.
#'
#' @param data An atomic vector, factor, Date, or POSIXct vector.
#' @param name Heading for the profile.
#' @param sample Optional positive number of evenly spaced values to analyze.
#' @param top_n Number of frequent character/factor values to display.
#' @param id Optional identifier.
#' @param classes Optional classes.
#' @param style A [style()].
#' @param disabled Disable the widget?
#' @return A `DataProfile` widget with reactive `$data` and `$summary`.
#' @export
#' @examples
#' data_profile(mtcars$mpg, name = "mpg")
data_profile <- function(data, name = NULL, sample = NULL,
                         top_n = 5L, id = NULL, classes = NULL, style = NULL,
                         disabled = FALSE) {
  fn <- if (is.function(data)) data
  if (is.null(name)) name <- if (is.null(fn)) deparse(substitute(data)) else "value"
  if (is.function(name)) name <- "value"
  w <- DataProfile$new(if (is.null(fn)) data else numeric(), name = name, sample = sample,
                       top_n = top_n, id = id, classes = classes, style = style,
                       disabled = disabled)
  if (!is.null(fn)) w$bind_reactive("data", fn)
  w
}

json_scalar <- function(x) {
  if (is.null(x)) return("null")
  if (!length(x)) return("[]")
  if (length(x) != 1L) return(paste0("[", length(x), "]"))
  if (is.na(x)) return("NA")
  if (is.character(x) || is.factor(x)) return(encodeString(as.character(x), quote = "\""))
  if (is.logical(x)) return(if (x) "true" else "false")
  if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
  if (inherits(x, "POSIXt")) return(format(x, usetz = TRUE))
  format_compact_value(x)
}

json_container <- function(x) is.list(x) || is.atomic(x) && length(x) > 1L

#' Collapsible viewer for R list-like data
#'
#' Displays already-parsed R objects as a [tree_view()]. Named lists and
#' data frames are objects; unnamed lists and vectors are arrays. Children
#' are created when a node is expanded. At most `page_size` children are
#' created per page; a `Load next ...` node exposes the next page. Character
#' scalars are quoted, logical values use lowercase JSON spelling, `NULL` is
#' shown as `null`, and R `NA` is shown as `NA` to keep it distinct from JSON
#' `null`. Set `parse = TRUE` for JSON text when optional package `jsonlite`
#' is installed; this is a convenience over the primary R-object interface.
#'
#' @param x An R object, or one JSON string when `parse = TRUE`.
#' @param parse Parse a JSON character string with optional `jsonlite`?
#' @param page_size Maximum children created when a tree node is expanded.
#' @param id Optional identifier.
#' @param classes Optional classes.
#' @param style A [style()].
#' @param disabled Disable focus and tree interaction?
#' @return A `TreeView` with lazily loaded object/array children.
#' @export
#' @examples
#' json_view(list(user = list(id = 42, name = "Alice"), active = TRUE))
json_view <- function(x, parse = FALSE, page_size = 100L, id = NULL,
                      classes = NULL, style = NULL, disabled = FALSE) {
  check_flag(parse, "parse")
  page_size <- max(1L, check_count(page_size, "page_size"))
  if (parse) {
    if (!is.character(x) || length(x) != 1L || is.na(x)) stop("With `parse = TRUE`, `x` must be one JSON string.", call. = FALSE)
    if (!requireNamespace("jsonlite", quietly = TRUE)) stop("JSON parsing requires the optional jsonlite package.", call. = FALSE)
    x <- jsonlite::fromJSON(x, simplifyVector = FALSE)
  }

  value_node <- function(label, value) {
    if (!json_container(value)) return(tree_node(paste(label, json_scalar(value))))
    kind <- if (is.data.frame(value) || (!is.null(names(value)) && all(nzchar(names(value)))) ||
               is.atomic(value) && !is.null(names(value))) "object" else "array"
    count <- length(value)
    node_label <- paste0(label, " ", kind, " [", count, "]")
    tree_node(node_label, loader = function(node) page_nodes(value, kind, 1L, count))
  }
  page_nodes <- function(value, kind, start, total) {
    if (total == 0L) return(list(tree_node(if (kind == "object") "(empty object)" else "(empty array)")))
    end <- min(total, start + page_size - 1L)
    if (is.data.frame(value)) {
      get_value <- function(i) value[[i]]
      keys <- names(value)
      kind <- "object"
    } else {
      get_value <- function(i) value[[i]]
      keys <- names(value)
    }
    nodes <- lapply(seq.int(start, end), function(i) {
      key <- if (kind == "object") keys[[i]] else paste0("[", i, "]")
      value_node(key, get_value(i))
    })
    if (end < total) {
      next_start <- end + 1L
      nodes[[length(nodes) + 1L]] <- tree_node(
        sprintf("Load next %d (%d-%d of %d)", min(page_size, total - end), next_start, total, total),
        loader = function(node) page_nodes(value, kind, next_start, total)
      )
    }
    nodes
  }
  root <- value_node(if (is.null(names(x))) "root" else "root", x)
  out <- tree_view(root, show_root = TRUE, id = id, classes = classes, style = style)
  out$disabled <- disabled
  out
}

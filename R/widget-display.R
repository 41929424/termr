# Display widgets: progress bar, spinner, rule, sparkline, metric, key/value.



#' @title ProgressBar widget
#' @description See [progress_bar()].
#' @rdname ProgressBar-class
#' @export
ProgressBar <- R6::R6Class(
  "ProgressBar",
  inherit = Widget,
  public = list(
    #' @field paint_states Progress changes only repaint.
    paint_states = c("value", "phase"),
    #' @field total The value that means 100%.
    total = 1,
    #' @field show_percent Show the percentage?
    show_percent = TRUE,
    #' @description Create a progress bar. See [progress_bar()].
    #' @param value Progress (or `NA` for indeterminate).
    #' @param total Total.
    #' @param show_percent Show the percentage?
    #' @param id,classes,style See [Widget].
    initialize = function(value = 0, total = 1, show_percent = TRUE, id = NULL, classes = NULL, style = NULL) {
      if (!is_scalar_number(total) || total <= 0) stop("`total` must be a positive number.", call. = FALSE)
      check_flag(show_percent)
      super$initialize(id = id, classes = classes, style = style)
      self$total <- total
      self$show_percent <- show_percent
      private$.state$value <- check_progress(value)
      private$.state$phase <- 0L
    },
    #' @description The built-in style.
    default_style = function() style(width = "1fr", height = 1),
    #' @description Animate the indeterminate state.
    #' @param event A `MountEvent`.
    on_mount = function(event) {
      if (motion_reduced(self$app)) return(invisible())
      self$set_interval(0.1, function(self, app) {
        if (is.na(private$.state$value)) self$set_state("phase", private$.state$phase + 1L)
      })
    },
    #' @description Advance the progress.
    #' @param amount Amount to add.
    advance = function(amount = 1) {
      self$value <- min(self$total, (if (is.na(private$.state$value)) 0 else private$.state$value) + amount)
      invisible(self)
    },
    #' @description The bar for a given width.
    #' @param width Content width.
    render_lines = function(width = NA_integer_) {
      width <- if (is.na(width)) 20L else width
      value <- private$.state$value
      utf8 <- unicode_ok()
      fill_char <- if (utf8) "\u2501" else "#"
      empty_char <- if (utf8) "\u2501" else "-"
      label <- if (self$show_percent && !is.na(value)) sprintf(" %3.0f%%", 100 * value / self$total) else ""
      bar_w <- max(0L, width - nchar(label))
      if (is.na(value)) {
        seg <- max(1L, bar_w %/% 4L)
        span_w <- max(1L, bar_w - seg)
        pos <- private$.state$phase %% (2L * span_w)
        if (pos > span_w) pos <- 2L * span_w - pos
        parts <- c(pos, seg, max(0L, bar_w - pos - seg))
        return(text_lines(c(
          span(strrep(empty_char, parts[[1]]), style(foreground = "$muted")),
          span(strrep(fill_char, parts[[2]]), style(foreground = "$accent")),
          span(strrep(empty_char, parts[[3]]), style(foreground = "$muted"))
        )))
      }
      filled <- as.integer(round(bar_w * value / self$total))
      done <- value >= self$total
      text_lines(c(
        span(strrep(fill_char, filled), style(foreground = if (done) "$success" else "$accent")),
        span(strrep(empty_char, bar_w - filled), style(foreground = "$muted")),
        span(label)
      ))
    },
    #' @description Natural width.
    content_width = function() 20L
  ),
  active = list(
    #' @field value Progress between 0 and `total`, or `NA` (reactive).
    value = function(value) {
      if (missing(value)) return(private$.state$value)
      self$set_state("value", check_progress(value))
    },
    #' @field fraction Progress as a fraction (`NA` when indeterminate).
    fraction = function(value) if (missing(value)) private$.state$value / self$total else read_only("fraction")
  )
)

check_progress <- function(value) {
  if (length(value) != 1L || !(is.numeric(value) || is.na(value)) || (!is.na(value) && value < 0)) {
    stop("`value` must be a non-negative number or NA.", call. = FALSE)
  }
  as.numeric(value)
}

#' Progress bar
#'
#' A bar filling as `value` goes from 0 to `total`, with a percentage. Set
#' `value = NA` for an indeterminate (animated) bar.
#'
#' @param value Current progress, or `NA`.
#' @param total Value that means complete.
#' @param show_percent Show the percentage?
#' @param id Optional identifier.
#' @param classes Optional classes.
#' @param style A [style()].
#' @return A `ProgressBar` with a reactive `value` and method `advance()`.
#' @export
#' @examples
#' render_widget(progress_bar(0.42), 20, 1)
progress_bar <- function(value = 0, total = 1, show_percent = TRUE, id = NULL, classes = NULL, style = NULL) {
  fn <- if (is.function(value)) value
  w <- ProgressBar$new(if (is.null(fn)) value else 0, total = total, show_percent = show_percent,
                       id = id, classes = classes, style = style)
  if (!is.null(fn)) w$bind_reactive("value", fn)
  w
}

spinner_frames <- list(
  dots = c("\u280b", "\u2819", "\u2839", "\u2838", "\u283c", "\u2834", "\u2826", "\u2827", "\u2807", "\u280f"),
  line = c("-", "\\", "|", "/"),
  arc = c("\u25dc", "\u25e0", "\u25dd", "\u25de", "\u25e1", "\u25df")
)

#' @title Spinner widget
#' @description See [spinner()].
#' @rdname Spinner-class
#' @export
Spinner <- R6::R6Class(
  "Spinner",
  inherit = Widget,
  public = list(
    #' @field paint_states Animation frames only repaint.
    paint_states = "frame",
    #' @field frames Animation frames.
    frames = NULL,
    #' @field interval Seconds per frame.
    interval = 0.1,
    #' @description Create a spinner. See [spinner()].
    #' @param label Text after the spinner.
    #' @param type Animation name.
    #' @param interval Seconds per frame.
    #' @param id,classes,style See [Widget].
    initialize = function(label = "", type = "dots", interval = 0.1, id = NULL, classes = NULL, style = NULL) {
      type <- check_choice(type, names(spinner_frames), "type")
      if (!is_scalar_number(interval) || interval <= 0) stop("`interval` must be positive.", call. = FALSE)
      super$initialize(id = id, classes = classes, style = style)
      self$frames <- if (unicode_ok() || type == "line") spinner_frames[[type]] else spinner_frames$line
      self$interval <- interval
      private$.state$frame <- 1L
      private$.state$label <- check_text(label)
      private$.state$spinning <- TRUE
    },
    #' @description The built-in style.
    default_style = function() style(width = "auto", height = "auto"),
    #' @description Start the animation timer.
    #' @param event A `MountEvent`.
    on_mount = function(event) {
      # Reduced motion: a still symbol (the label still tells what is going on).
      if (motion_reduced(self$app)) return(invisible())
      self$set_interval(self$interval, function(self, app) {
        if (private$.state$spinning) self$set_state("frame", private$.state$frame %% length(self$frames) + 1L)
      })
    },
    #' @description The current frame and the label.
    render = function() {
      frame <- if (private$.state$spinning) self$frames[[private$.state$frame]] else " "
      c(span(frame, style(foreground = "$accent")), span(" "), as_text(private$.state$label))
    }
  ),
  active = list(
    #' @field label Text after the spinner (reactive).
    label = function(value) {
      if (missing(value)) return(private$.state$label)
      self$set_state("label", check_text(value))
    },
    #' @field spinning Is it animating? (reactive)
    spinning = function(value) {
      if (missing(value)) return(private$.state$spinning)
      check_flag(value, "spinning")
      self$set_state("spinning", value)
    }
  )
)

#' Spinner
#'
#' A small animation showing that work is in progress. It runs on a timer of
#' the widget, so it stops automatically when the widget is removed.
#'
#' @param label Text shown after the spinner.
#' @param type `"dots"`, `"line"` or `"arc"` (ASCII `"line"` is used when the
#'   terminal cannot show Unicode).
#' @param interval Seconds per frame.
#' @param id Optional identifier.
#' @param classes Optional classes.
#' @param style A [style()].
#' @return A `Spinner` with reactive `label` and `spinning`.
#' @export
#' @examples
#' spinner("Loading data...")
spinner <- function(label = "", type = "dots", interval = 0.1, id = NULL, classes = NULL, style = NULL) {
  Spinner$new(label, type = type, interval = interval, id = id, classes = classes, style = style)
}

#' @title Rule widget
#' @description See [rule()].
#' @rdname Rule-class
#' @export
Rule <- R6::R6Class(
  "Rule",
  inherit = Widget,
  public = list(
    #' @field orientation `"horizontal"` or `"vertical"`.
    orientation = "horizontal",
    #' @field title Optional title in the middle of a horizontal rule.
    title = NULL,
    #' @description Create a rule. See [rule()].
    #' @param orientation Orientation.
    #' @param title Title.
    #' @param id,classes,style See [Widget].
    initialize = function(orientation = "horizontal", title = NULL, id = NULL, classes = NULL, style = NULL) {
      self$orientation <- check_choice(orientation, c("horizontal", "vertical"), "orientation")
      check_scalar_character(title, "title", allow_null = TRUE)
      self$title <- title
      super$initialize(id = id, classes = classes, style = style)
    },
    #' @description The built-in style.
    default_style = function() {
      if (self$orientation == "horizontal") style(width = "1fr", height = 1, foreground = "$muted")
      else style(width = 1, height = "1fr", foreground = "$muted")
    },
    #' @description Paint the line.
    #' @param buffer A [ScreenBuffer].
    #' @param area Visible part of the region.
    #' @param st Computed style.
    paint = function(buffer, area, st) {
      draw_background(buffer, area, st)
      inner <- content_rect(self$region, st)
      utf8 <- unicode_ok()
      if (self$orientation == "horizontal") {
        line <- strrep(if (utf8) "\u2500" else "-", inner$width)
        if (!is.null(self$title) && inner$width > 4L) {
          text <- str_truncate(paste0(" ", self$title, " "), inner$width - 2L)
          start <- (inner$width - str_width(text)) %/% 2L
          line <- paste0(strrep(if (utf8) "\u2500" else "-", start), text,
                         strrep(if (utf8) "\u2500" else "-", inner$width - start - str_width(text)))
        }
        buffer$put_text(inner$x, inner$y, line, fg = st$foreground, attrs = st$attrs, clip = area)
      } else {
        for (y in seq_len(inner$height)) {
          buffer$put_text(inner$x, inner$y + y - 1L, if (utf8) "\u2502" else "|", fg = st$foreground, clip = area)
        }
      }
      invisible()
    }
  )
)

#' Rule (separator)
#'
#' A horizontal or vertical line, optionally with a title.
#'
#' @param orientation `"horizontal"` or `"vertical"`.
#' @param title Optional title for horizontal rules.
#' @param id Optional identifier.
#' @param classes Optional classes.
#' @param style A [style()].
#' @return A `Rule` widget.
#' @export
#' @examples
#' render_widget(rule(title = "Results"), 20, 1)
rule <- function(orientation = "horizontal", title = NULL, id = NULL, classes = NULL, style = NULL) {
  Rule$new(orientation, title = title, id = id, classes = classes, style = style)
}

#' @title Sparkline widget
#' @description See [sparkline()].
#' @rdname Sparkline-class
#' @export
Sparkline <- R6::R6Class(
  "Sparkline",
  inherit = Widget,
  public = list(
    #' @field min,max Fixed scale limits, or `NULL` to use the data range.
    min = NULL,
    max = NULL,
    #' @field max_length Values kept by `push()`.
    max_length = 1000L,
    #' @field summary How to fit more values than columns: `"tail"` (latest
    #'   values) or `"mean"` (average buckets).
    summary = "tail",
    #' @description Create a sparkline. See [sparkline()].
    #' @param data Numbers.
    #' @param min,max Scale limits.
    #' @param summary Fitting strategy.
    #' @param max_length Values kept by `push()`.
    #' @param id,classes,style See [Widget].
    initialize = function(data = numeric(), min = NULL, max = NULL, summary = "tail", max_length = 1000L,
                          id = NULL, classes = NULL, style = NULL) {
      super$initialize(id = id, classes = classes, style = style)
      self$min <- min
      self$max <- max
      self$summary <- check_choice(summary, c("tail", "mean"), "summary")
      self$max_length <- check_count(max_length, "max_length")
      private$.state$data <- check_numbers(data)
    },
    #' @description The built-in style.
    default_style = function() style(width = "1fr", height = 1, foreground = "$accent"),
    #' @description Append values (keeping at most `max_length`).
    #' @param x Numbers.
    push = function(x) {
      data <- c(private$.state$data, check_numbers(x))
      if (length(data) > self$max_length) data <- utils::tail(data, self$max_length)
      self$set_state("data", data)
      invisible(self)
    },
    #' @description The bars for a given width.
    #' @param width Content width.
    render_lines = function(width = NA_integer_) {
      text_lines(sparkline_text(private$.state$data, if (is.na(width)) length(private$.state$data) else width,
                                self$min, self$max, self$summary))
    },
    #' @description Natural width: one column per value.
    content_width = function() length(private$.state$data)
  ),
  active = list(
    #' @field data The values (reactive).
    data = function(value) {
      if (missing(value)) return(private$.state$data)
      self$set_state("data", check_numbers(value))
    }
  )
)

check_numbers <- function(x) {
  if (!is.numeric(x) && !all(is.na(x))) stop("Sparkline data must be numeric.", call. = FALSE)
  as.numeric(x)
}

# Unicode block characters for numbers; NA becomes a space.
sparkline_text <- function(x, width, min = NULL, max = NULL, summary = "tail") {
  if (width <= 0L || length(x) == 0L) return("")
  if (length(x) > width) {
    x <- if (summary == "tail") utils::tail(x, width) else {
      groups <- cut(seq_along(x), width, labels = FALSE)
      vapply(split(x, groups), function(v) if (all(is.na(v))) NA_real_ else mean(v, na.rm = TRUE), numeric(1))
    }
  }
  blocks <- if (unicode_ok()) c("\u2581", "\u2582", "\u2583", "\u2584", "\u2585", "\u2586", "\u2587", "\u2588") else
    c("_", ".", "-", "-", "=", "=", "#", "#")
  finite <- x[is.finite(x)]
  lo <- min %||% (if (length(finite)) base::min(finite) else 0)
  hi <- max %||% (if (length(finite)) base::max(finite) else 1)
  level <- if (hi > lo) (pmin(pmax(x, lo), hi) - lo) / (hi - lo) else rep(0.5, length(x))
  idx <- pmin(8L, as.integer(floor(level * 7.999)) + 1L)
  out <- blocks[idx]
  out[!is.finite(x)] <- " "
  paste(out, collapse = "")
}

#' Sparkline
#'
#' A tiny line chart made of block characters (`\u2581` to `\u2588`), one
#' column per value. With more values than columns, the latest values are
#' shown (`summary = "tail"`) or buckets are averaged (`"mean"`). Missing
#' values are blank. Use `push()` to stream values.
#'
#' @param data Numeric vector.
#' @param min,max Fixed scale; `NULL` uses the range of the data.
#' @param summary `"tail"` or `"mean"`.
#' @param max_length Number of values kept by `push()`.
#' @param id Optional identifier.
#' @param classes Optional classes.
#' @param style A [style()]; the foreground colour colours the bars.
#' @return A `Sparkline` with reactive `data` and method `push()`.
#' @export
#' @examples
#' render_widget(sparkline(c(1, 4, 2, 8, 5, NA, 7)), 7, 1)
sparkline <- function(data = numeric(), min = NULL, max = NULL, summary = "tail", max_length = 1000L,
                      id = NULL, classes = NULL, style = NULL) {
  fn <- if (is.function(data)) data
  w <- Sparkline$new(if (is.null(fn)) data else numeric(), min = min, max = max, summary = summary,
                     max_length = max_length, id = id, classes = classes, style = style)
  if (!is.null(fn)) w$bind_reactive("data", fn)
  w
}

#' @title Metric widget
#' @description See [metric()].
#' @rdname Metric-class
#' @export
Metric <- R6::R6Class(
  "Metric",
  inherit = Widget,
  public = list(
    #' @field formatter `function(value)` returning a string.
    formatter = NULL,
    #' @description Create a metric. See [metric()].
    #' @param label Label.
    #' @param value Value.
    #' @param delta Change.
    #' @param format Formatter.
    #' @param id,classes,style See [Widget].
    initialize = function(label, value = NA, delta = NULL, format = NULL, id = NULL, classes = NULL, style = NULL) {
      check_function(format, "format", allow_null = TRUE)
      super$initialize(id = id, classes = classes, style = style)
      self$formatter <- format %||% format_metric
      private$.state$label <- check_text(label)
      private$.state$value <- value
      private$.state$delta <- delta
    },
    #' @description The built-in style.
    default_style = function() style(width = "1fr", height = "auto", border = "round", padding = c(0, 1), min_width = 12),
    #' @description Set the value (and optionally the delta).
    #' @param value New value.
    #' @param delta New delta (`NULL` keeps none).
    update = function(value, delta = NULL) {
      self$set_state("value", value)
      self$set_state("delta", delta)
      invisible(self)
    },
    #' @description Label, value and delta lines.
    render = function() {
      lines <- c(
        span(as.character(as_text(private$.state$label)), style(foreground = "$muted")),
        span("\n"),
        span(self$formatter(private$.state$value), style(bold = TRUE))
      )
      delta <- private$.state$delta
      if (!is.null(delta) && !is.na(delta)) {
        up <- delta > 0
        arrow <- if (unicode_ok()) (if (up) "\u25b2 " else if (delta < 0) "\u25bc " else "") else (if (up) "+" else "")
        colour <- if (up) "$success" else if (delta < 0) "$error" else "$muted"
        lines <- c(lines, span("  "), span(paste0(arrow, self$formatter(abs(delta))), style(foreground = colour)))
      }
      lines
    }
  ),
  active = list(
    #' @field value The value (reactive).
    value = function(value) {
      if (missing(value)) return(private$.state$value)
      self$set_state("value", value)
    },
    #' @field delta The change shown next to the value (reactive).
    delta = function(value) {
      if (missing(value)) return(private$.state$delta)
      self$set_state("delta", value)
    }
  )
)

format_metric <- function(x) {
  if (is.null(x) || length(x) == 0L) return("")
  if (length(x) == 1L && is.na(x)) return("\u2014")
  if (is.numeric(x)) return(format(x, big.mark = ",", digits = 4, scientific = FALSE, trim = TRUE))
  paste(format(x), collapse = " ")
}

#' Metric
#'
#' A key number with a label and an optional change (delta) shown as a
#' green up or red down arrow. Useful in dashboards.
#'
#' @param label Label shown above the value.
#' @param value The value (any R value; numbers are formatted with
#'   thousands separators).
#' @param delta Optional change since the last value.
#' @param format Optional `function(value)` returning a string (also used
#'   for the delta).
#' @param id Optional identifier.
#' @param classes Optional classes.
#' @param style A [style()].
#' @return A `Metric` widget with reactive `value` and `delta`, and method
#'   `update(value, delta)`.
#' @export
#' @examples
#' render_widget(metric("Accuracy", 0.943, delta = 0.012), 20, 4)
metric <- function(label, value = NA, delta = NULL, format = NULL, id = NULL, classes = NULL, style = NULL) {
  fn <- if (is.function(value)) value
  w <- Metric$new(label, value = if (is.null(fn)) value else NA, delta = delta, format = format,
                  id = id, classes = classes, style = style)
  if (!is.null(fn)) w$bind_reactive("value", fn)
  w
}

#' @title KeyValue widget
#' @description See [key_value()].
#' @rdname KeyValue-class
#' @export
KeyValue <- R6::R6Class(
  "KeyValue",
  inherit = Widget,
  public = list(
    #' @description Create a key/value list. See [key_value()].
    #' @param data Named list or vector.
    #' @param id,classes,style See [Widget].
    initialize = function(data = list(), id = NULL, classes = NULL, style = NULL) {
      super$initialize(id = id, classes = classes, style = style)
      private$.state$data <- check_key_values(data)
    },
    #' @description The built-in style.
    default_style = function() style(width = "auto", height = "auto"),
    #' @description Aligned key and value lines.
    render = function() {
      data <- private$.state$data
      if (!length(data)) return("")
      keys <- names(data)
      width <- max(str_width(keys))
      pieces <- lapply(seq_along(data), function(i) {
        value <- format_metric(data[[i]])
        c(span(paste0(str_align(keys[[i]], width), "  "), style(foreground = "$muted")),
          span(value), if (i < length(data)) span("\n"))
      })
      do.call(c, pieces)
    }
  ),
  active = list(
    #' @field data Named list of values (reactive).
    data = function(value) {
      if (missing(value)) return(private$.state$data)
      self$set_state("data", check_key_values(value))
    }
  )
)

check_key_values <- function(data) {
  data <- as.list(data)
  if (length(data) && (is.null(names(data)) || any(!nzchar(names(data))))) {
    stop("`data` must be a named list or vector.", call. = FALSE)
  }
  data
}

#' Key/value list
#'
#' Shows names and values in two aligned columns, e.g. a summary of a data
#' set.
#'
#' @section Choosing a key/value widget:
#' * [key_value()]: a few known values shown in full, such as a summary
#'   panel. Not focusable, does not scroll; values are not wrapped.
#' * [property_grid()]: an inspector for many or arbitrary named values.
#'   Focusable and scrollable, wraps long values, formats vectors and lists
#'   compactly and accepts a `format` function.
#' * [record_view()]: [property_grid()] for exactly one record, such as the
#'   selected row of a [data_table()]; accepts a one-row data frame.
#' @param data A named list or vector.
#' @param id Optional identifier.
#' @param classes Optional classes.
#' @param style A [style()].
#' @return A `KeyValue` widget with a reactive `data` field.
#' @export
#' @examples
#' render_widget(key_value(list(Rows = 32, Columns = 11, Source = "mtcars")), 20, 3)
key_value <- function(data = list(), id = NULL, classes = NULL, style = NULL) {
  fn <- if (is.function(data)) data
  w <- KeyValue$new(if (is.null(fn)) data else list(), id = id, classes = classes, style = style)
  if (!is.null(fn)) w$bind_reactive("data", fn)
  w
}

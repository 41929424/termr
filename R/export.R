# Static export from the same framebuffer used by the terminal renderer.

export_buffer <- function(x, width = 80, height = 24) {
  if (inherits(x, "ScreenBuffer")) return(x$copy())
  render_widget(x, width, height)
}

#' Render a widget tree as plain text
#'
#' Static renderers use the normal layout and paint pipeline and do not
#' start an event loop or write to a terminal.
#'
#' @param x A widget, [Screen], [App], or [ScreenBuffer].
#' @param width,height Frame size in cells.
#' @param trim Remove trailing spaces from each line?
#' @return `render_text()` returns one string separated by newlines;
#'   `render_lines()` returns one string per row.
#' @export
#' @rdname render_text
#' @examples
#' render_text(panel(label("Finished"), title = "Job"), width = 20, height = 5)
render_text <- function(x, width = 80, height = 24, trim = TRUE) {
  check_flag(trim, "trim")
  paste(render_lines(x, width, height, trim), collapse = "\n")
}

#' @export
#' @rdname render_text
render_lines <- function(x, width = 80, height = 24, trim = TRUE) {
  check_flag(trim, "trim")
  lines <- export_buffer(x, width, height)$to_text()
  if (trim) sub(" +$", "", lines) else lines
}

#' Render a terminal frame as a Markdown code block
#'
#' @inheritParams render_text
#' @return A fenced Markdown text block.
#' @export
#' @examples
#' cat(render_markdown(label("Hello"), width = 20, height = 2))
render_markdown <- function(x, width = 80, height = 24, trim = TRUE) {
  lines <- render_lines(x, width, height, trim)
  body <- paste(lines, collapse = "\n")
  runs <- gregexpr("`+", body, perl = TRUE)[[1L]]
  longest <- if (identical(runs, -1L)) 0L else max(attr(runs, "match.length"))
  fence <- strrep("`", max(3L, longest + 1L))
  paste0(fence, "text\n", body, "\n", fence)
}

snapshot_runs <- function(buffer) {
  out <- list()
  for (y in seq_len(buffer$height)) {
    x <- 1L
    while (x <= buffer$width) {
      fg <- buffer$fg[y, x]; bg <- buffer$bg[y, x]; attrs <- buffer$attrs[y, x]
      end <- x
      while (end < buffer$width && identical(buffer$fg[y, end + 1L], fg) &&
             identical(buffer$bg[y, end + 1L], bg) &&
             identical(buffer$attrs[y, end + 1L], attrs)) end <- end + 1L
      text <- paste(buffer$chars[y, x:end], collapse = "")
      out[[length(out) + 1L]] <- list(x = as.integer(x), y = as.integer(y),
        width = as.integer(end - x + 1L), text = text, fg = fg, bg = bg,
        attrs = as.integer(attrs))
      x <- end + 1L
    }
  }
  out
}

#' Create a deterministic screen snapshot
#'
#' The returned base R list is independent of `jsonlite` and has a stable
#' `schema_version` field. Runs are row-major and represent adjacent cells
#' with the same foreground, background, and attribute mask.
#'
#' @inheritParams render_text
#' @return A `termr_screen_snapshot` list.
#' @export
#' @examples
#' screen_snapshot(label("Hello"), width = 20, height = 2)
screen_snapshot <- function(x, width = 80, height = 24) {
  buffer <- export_buffer(x, width, height)
  structure(list(schema_version = 1L, width = buffer$width,
    height = buffer$height, lines = buffer$to_text(), runs = snapshot_runs(buffer)),
    class = "termr_screen_snapshot")
}

#' Serialize a screen snapshot as JSON
#'
#' The JSON form of [screen_snapshot()]. Requires the optional `jsonlite`
#' package.
#'
#' @inheritParams render_text
#' @param pretty Format the JSON for readability?
#' @return A JSON string.
#' @export
screen_snapshot_json <- function(x, width = 80, height = 24, pretty = TRUE) {
  check_flag(pretty, "pretty")
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("`screen_snapshot_json()` requires the optional package `jsonlite`.", call. = FALSE)
  }
  jsonlite::toJSON(unclass(screen_snapshot(x, width, height)), auto_unbox = TRUE,
    null = "null", na = "null", pretty = pretty, digits = NA)
}

html_escape <- function(x, attribute = FALSE) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  if (attribute) x <- gsub("'", "&#39;", x, fixed = TRUE)
  x
}

export_css_color <- function(x) {
  if (!nzchar(x)) return("")
  idx <- match(x, ansi_color_names)
  if (!is.na(idx)) return(sprintf("#%02x%02x%02x", ansi16_rgb[idx, 1], ansi16_rgb[idx, 2], ansi16_rgb[idx, 3]))
  if (grepl("^[0-9]+$", x)) {
    rgb <- xterm256_rgb(as.integer(x))[1L, ]
    return(sprintf("#%02x%02x%02x", rgb[[1]], rgb[[2]], rgb[[3]]))
  }
  x
}

run_css <- function(run, color = TRUE) {
  fg <- if (color) run$fg else ""
  bg <- if (color) run$bg else ""
  attrs <- attrs_decode(run$attrs)
  if (isTRUE(attrs[["reverse"]])) { tmp <- fg; fg <- bg; bg <- tmp }
  css <- character()
  if (nzchar(fg)) css <- c(css, paste0("color:", export_css_color(fg)))
  if (nzchar(bg)) css <- c(css, paste0("background-color:", export_css_color(bg)))
  if (attrs[["bold"]]) css <- c(css, "font-weight:bold")
  if (attrs[["dim"]]) css <- c(css, "opacity:.65")
  if (attrs[["italic"]]) css <- c(css, "font-style:italic")
  deco <- c(if (attrs[["underline"]]) "underline", if (attrs[["strike"]]) "line-through")
  if (length(deco)) css <- c(css, paste0("text-decoration:", paste(deco, collapse = " ")))
  paste(css, collapse = ";")
}

#' Render a styled HTML fragment
#'
#' @inheritParams render_text
#' @param color Include framebuffer colours even when `NO_COLOR` is set?
#' @return An embeddable `<pre>` fragment. Styling is approximate and depends
#'   on the browser's monospace font and CSS environment.
#' @export
#' @examples
#' cat(render_html(label("Ready", style = style(foreground = "green", bold = TRUE))))
render_html <- function(x, width = 80, height = 24, color = TRUE, trim = TRUE) {
  check_flag(color, "color"); check_flag(trim, "trim")
  buffer <- export_buffer(x, width, height)
  snap <- snapshot_runs(buffer)
  by_row <- split(snap, vapply(snap, `[[`, integer(1), "y"))
  rows <- vapply(by_row, function(row) {
    last <- length(row)
    spans <- vapply(seq_along(row), function(i) {
      run <- row[[i]]
      text <- run$text
      # Only the end of the row is trimmed; inner spaces keep columns aligned.
      if (trim && i == last) text <- sub(" +$", "", text)
      if (!nzchar(text)) return("")
      css <- run_css(run, color)
      body <- html_escape(text, attribute = TRUE)
      if (nzchar(css)) paste0("<span style=\"", html_escape(css, TRUE), "\">", body, "</span>") else body
    }, character(1))
    paste0(spans, collapse = "")
  }, character(1))
  paste0('<pre class="termr-screen" style="font-family:monospace;white-space:pre;">',
    paste(rows, collapse = "\n"), "</pre>")
}

xml_escape <- function(x, attribute = FALSE) html_escape(x, attribute = attribute)

#' Render a static frame as SVG
#'
#' @inheritParams render_text
#' @param cell_width Width of a terminal cell in SVG user units.
#' @param line_height Height of a row in SVG user units.
#' @param font_family CSS font family.
#' @param color Include framebuffer colours?
#' @return An SVG document string.
#' @export
render_svg <- function(x, width = 80, height = 24, cell_width = 9,
                       line_height = 18, font_family = "monospace", color = TRUE) {
  check_flag(color, "color")
  check_scalar_character(font_family, "font_family")
  if (!grepl("^[A-Za-z0-9 ,_-]+$", font_family)) {
    stop("`font_family` may contain only letters, numbers, spaces, commas, underscores, and hyphens.", call. = FALSE)
  }
  if (!is.numeric(cell_width) || length(cell_width) != 1L || !is.finite(cell_width) || cell_width <= 0 ||
      !is.numeric(line_height) || length(line_height) != 1L || !is.finite(line_height) || line_height <= 0) {
    stop("`cell_width` and `line_height` must be positive finite numbers.", call. = FALSE)
  }
  buffer <- export_buffer(x, width, height)
  runs <- snapshot_runs(buffer)
  w <- buffer$width * cell_width; h <- buffer$height * line_height
  font_size <- line_height * 0.72
  pieces <- character()
  for (run in runs) {
    attrs <- attrs_decode(run$attrs)
    fg <- if (color) run$fg else ""
    bg <- if (color) run$bg else ""
    if (attrs[["reverse"]]) { tmp <- fg; fg <- bg; bg <- tmp }
    x0 <- (run$x - 1L) * cell_width; y0 <- (run$y - 1L) * line_height
    if (nzchar(bg)) pieces <- c(pieces, sprintf('<rect x="%s" y="%s" width="%s" height="%s" fill="%s"/>',
      x0, y0, run$width * cell_width, line_height, xml_escape(export_css_color(bg), TRUE)))
    text <- run$text
    if (!nzchar(text)) next
    style <- c(paste0("font-family:", xml_escape(font_family, TRUE)),
      paste0("font-size:", font_size, "px"),
      if (nzchar(fg)) paste0("fill:", export_css_color(fg)),
      if (attrs[["bold"]]) "font-weight:bold",
      if (attrs[["dim"]]) "opacity:.65",
      if (attrs[["italic"]]) "font-style:italic",
      if (attrs[["underline"]]) "text-decoration:underline",
      if (attrs[["strike"]]) "text-decoration:line-through")
    pieces <- c(pieces, sprintf('<text x="%s" y="%s" style="%s">%s</text>',
      x0, y0 + font_size, paste(style, collapse = ";"), xml_escape(text)))
  }
  paste0('<svg xmlns="http://www.w3.org/2000/svg" xml:space="preserve" width="', w, '" height="', h,
    '" viewBox="0 0 ', w, ' ', h, '"><g font-family="', xml_escape(font_family, TRUE), '">',
    paste(pieces, collapse = ""), "</g></svg>")
}

widget_node_snapshot <- function(widget) {
  region <- widget$region
  list(type = widget$type, id = widget$id, classes = as.character(widget$classes),
    region = if (is.null(region)) NULL else unclass(region),
    visible = isTRUE(widget$visible), enabled = isTRUE(widget$is_enabled()),
    focused = isTRUE(widget$focused),
    children = lapply(widget$children, widget_node_snapshot))
}

#' Snapshot the safe, structural parts of a widget tree
#'
#' Arbitrary widget state and field values are intentionally omitted, so
#' passwords, database credentials, and worker data are not serialized.
#'
#' @param x A widget, [Screen], or [App].
#' @return A deterministic nested list of widget descriptions.
#' @export
widget_snapshot <- function(x) {
  if (inherits(x, "App")) x <- x$screen
  if (!is_widget(x)) stop("`x` must be a widget, screen, or app.", call. = FALSE)
  structure(c(list(schema_version = 1L), widget_node_snapshot(x)), class = "termr_widget_snapshot")
}

#' Serialize a structural widget snapshot as JSON
#'
#' The JSON form of [widget_snapshot()]. Requires the optional `jsonlite`
#' package.
#' @param x A widget, [Screen], or [App].
#' @param pretty Format JSON for readability?
#' @return A JSON string.
#' @export
widget_snapshot_json <- function(x, pretty = TRUE) {
  check_flag(pretty, "pretty")
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("`widget_snapshot_json()` requires the optional package `jsonlite`.", call. = FALSE)
  }
  jsonlite::toJSON(unclass(widget_snapshot(x)), auto_unbox = TRUE, null = "null",
    na = "null", pretty = pretty)
}

#' Render a static representation for knitr or Quarto
#'
#' @inheritParams render_text
#' @param format Explicit output format: `markdown` or `html`.
#' @return A `knitr::asis_output()` object.
#' @export
knit_termr <- function(x, width = 80, height = 24,
                       format = c("markdown", "html")) {
  format <- match.arg(format)
  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("`knit_termr()` requires the optional package `knitr`.", call. = FALSE)
  }
  out <- if (format == "markdown") render_markdown(x, width, height) else render_html(x, width, height)
  knitr::asis_output(out)
}

#' Write a static rendering to a UTF-8 file
#'
#' @inheritParams render_text
#' @param file Output path.
#' @param format One of `text`, `markdown`, `html`, `svg`, or `json`.
#' @export
write_rendered <- function(x, file, format = c("text", "markdown", "html", "svg", "json"),
                           width = 80, height = 24) {
  format <- match.arg(format)
  check_scalar_character(file, "file")
  output <- switch(format,
    text = render_text(x, width, height),
    markdown = render_markdown(x, width, height),
    html = render_html(x, width, height),
    svg = render_svg(x, width, height),
    json = as.character(screen_snapshot_json(x, width, height)))
  writeLines(output, file, useBytes = TRUE)
  invisible(file)
}

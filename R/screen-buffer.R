#' Virtual screen (framebuffer)
#'
#' A `ScreenBuffer` is an in-memory grid of terminal cells. Widgets are
#' painted into a buffer, buffers are compared with [diff_screen()] and only
#' the differences are sent to the terminal.
#'
#' Cells are stored in four `height x width` matrices (`chars`, `fg`, `bg`,
#' `attrs`) indexed as `[row, column]`. A wide character occupies two
#' columns: the first holds the character, the second holds `""` (a
#' continuation cell). All writes keep this invariant, so a wide character
#' is never left half-overwritten.
#'
#' Use [screen_buffer()] to create one.
#'
#' @export
ScreenBuffer <- R6::R6Class(
  "ScreenBuffer",
  public = list(
    #' @field width Number of columns.
    width = 0L,
    #' @field height Number of rows.
    height = 0L,
    #' @field chars Character matrix of cell contents.
    chars = NULL,
    #' @field fg Character matrix of canonical foreground colours.
    fg = NULL,
    #' @field bg Character matrix of canonical background colours.
    bg = NULL,
    #' @field attrs Integer matrix of attribute bit masks.
    attrs = NULL,

    #' @description Create a blank buffer.
    #' @param width,height Size in cells.
    initialize = function(width, height) {
      if (!is_scalar_number(width) || !is_scalar_number(height) || width < 0 || height < 0) {
        stop("`width` and `height` must be non-negative numbers.", call. = FALSE)
      }
      self$width <- as.integer(width)
      self$height <- as.integer(height)
      self$clear()
    },

    #' @description Reset every cell to a blank default cell.
    clear = function() {
      dims <- c(self$height, self$width)
      self$chars <- matrix(" ", dims[[1]], dims[[2]])
      self$fg <- matrix("", dims[[1]], dims[[2]])
      self$bg <- matrix("", dims[[1]], dims[[2]])
      self$attrs <- matrix(0L, dims[[1]], dims[[2]])
      invisible(self)
    },

    #' @description The full-buffer rectangle.
    bounds = function() rect(1L, 1L, self$width, self$height),

    #' @description Read one cell.
    #' @param x,y Column and row.
    get_cell = function(x, y) {
      private$check_position(x, y)
      structure(
        list(
          char = self$chars[y, x],
          fg = self$fg[y, x],
          bg = self$bg[y, x],
          attrs = self$attrs[y, x]
        ),
        class = "termr_cell"
      )
    },

    #' @description Write one cell (see [cell()]).
    #' @param x,y Column and row.
    #' @param cell A `termr_cell`.
    set_cell = function(x, y, cell) {
      private$check_position(x, y)
      if (!inherits(cell, "termr_cell")) stop("`cell` must be created with cell().", call. = FALSE)
      wide <- grapheme_width(cell$char) == 2L && x < self$width
      private$fix_wide_edges(y, x, if (wide) x + 1L else x)
      cols <- if (wide) c(x, x + 1L) else x
      self$chars[y, cols] <- c(cell$char, "")[seq_along(cols)]
      self$fg[y, cols] <- cell$fg
      self$bg[y, cols] <- cell$bg
      self$attrs[y, cols] <- cell$attrs
      invisible(self)
    },

    #' @description Fill a region with one character and style.
    #' @param region A [region()]; defaults to the whole buffer.
    #' @param char A single-width character.
    #' @param fg,bg Colours (see [style()]).
    #' @param attrs Attribute bit mask.
    fill = function(region = NULL, char = " ", fg = NULL, bg = NULL, attrs = 0L) {
      region <- rect_intersect(region %||% self$bounds(), self$bounds())
      if (rect_is_empty(region)) return(invisible(self))
      rows <- seq.int(region$y, rect_bottom(region))
      cols <- seq.int(region$x, rect_right(region))
      for (y in rows) private$fix_wide_edges(y, region$x, rect_right(region))
      self$chars[rows, cols] <- char
      self$fg[rows, cols] <- buffer_color(fg)
      self$bg[rows, cols] <- buffer_color(bg)
      self$attrs[rows, cols] <- as.integer(attrs)
      invisible(self)
    },

    #' @description Set only the background colour of a region.
    #' @param region A [region()].
    #' @param bg Colour.
    paint_background = function(region, bg) {
      region <- rect_intersect(region, self$bounds())
      if (rect_is_empty(region)) return(invisible(self))
      rows <- seq.int(region$y, rect_bottom(region))
      cols <- seq.int(region$x, rect_right(region))
      self$bg[rows, cols] <- buffer_color(bg)
      invisible(self)
    },

    #' @description Write a single line of text.
    #'
    #' Text is clipped to `clip` (and to the buffer). A wide character that
    #' would straddle the clip edge is replaced by a space. Control
    #' characters are removed.
    #' @param x,y Column and row of the first character.
    #' @param text A single string (no newlines).
    #' @param fg Foreground colour.
    #' @param bg Background colour. `NULL` keeps the existing background,
    #'   so text can be drawn over a painted widget background.
    #' @param attrs Attribute bit mask.
    #' @param clip Optional clipping [region()].
    #' @return The column after the last character, invisibly.
    put_text = function(x, y, text, fg = NULL, bg = NULL, attrs = 0L, clip = NULL) {
      cells <- text_cells(text)
      n <- length(cells$chars)
      next_col <- as.integer(x) + sum(cells$widths)
      if (n == 0L) return(invisible(next_col))
      clip <- rect_intersect(clip %||% self$bounds(), self$bounds())
      if (rect_is_empty(clip) || y < clip$y || y > rect_bottom(clip)) {
        return(invisible(next_col))
      }
      cl <- clip$x
      cr <- rect_right(clip)
      widths <- cells$widths
      starts <- as.integer(x) + c(0L, cumsum(widths)[-n])
      ends <- starts + widths - 1L

      visible <- starts >= cl & ends <= cr
      chars <- cells$chars[visible]
      cols <- starts[visible]
      wide <- widths[visible] == 2L
      # Wide characters cut by the clip edge become a single space.
      cut_left <- widths == 2L & starts == cl - 1L
      cut_right <- widths == 2L & starts == cr
      if (any(cut_left)) {
        chars <- c(" ", chars)
        cols <- c(cl, cols)
        wide <- c(FALSE, wide)
      }
      if (any(cut_right)) {
        chars <- c(chars, " ")
        cols <- c(cols, cr)
        wide <- c(wide, FALSE)
      }
      if (length(cols) == 0L) return(invisible(next_col))

      first <- min(cols)
      last <- max(cols + wide)
      private$fix_wide_edges(y, first, last)
      all_cols <- c(cols, cols[wide] + 1L)
      self$chars[y, cols] <- chars
      self$chars[y, cols[wide] + 1L] <- ""
      self$fg[y, all_cols] <- buffer_color(fg)
      if (!is.null(bg)) self$bg[y, all_cols] <- buffer_color(bg)
      self$attrs[y, all_cols] <- as.integer(attrs)
      invisible(next_col)
    },

    #' @description Change the buffer size, keeping the top-left content.
    #' @param width,height New size.
    resize = function(width, height) {
      old <- self$clone()
      self$width <- as.integer(width)
      self$height <- as.integer(height)
      self$clear()
      rows <- seq_len(min(old$height, self$height))
      cols <- seq_len(min(old$width, self$width))
      if (length(rows) && length(cols)) {
        self$chars[rows, cols] <- old$chars[rows, cols]
        self$fg[rows, cols] <- old$fg[rows, cols]
        self$bg[rows, cols] <- old$bg[rows, cols]
        self$attrs[rows, cols] <- old$attrs[rows, cols]
        # A wide character cut by the new right edge becomes a space.
        last <- self$width
        if (last > 0L && last < old$width) {
          cut <- old$chars[rows, last + 1L] == ""
          self$chars[rows[cut], last] <- " "
        }
      }
      invisible(self)
    },

    #' @description An independent copy of the buffer.
    copy = function() self$clone(),

    #' @description Do two buffers hold exactly the same cells?
    #' @param other Another `ScreenBuffer`.
    equals = function(other) {
      identical(self$chars, other$chars) && identical(self$fg, other$fg) &&
        identical(self$bg, other$bg) && identical(self$attrs, other$attrs)
    },

    #' @description The buffer as plain text, one string per row.
    to_text = function() {
      if (self$height == 0L) return(character())
      apply(self$chars, 1L, paste, collapse = "")
    },

    #' @description Print the buffer contents framed by a border.
    #' @param ... Ignored.
    print = function(...) {
      cat(sprintf("<ScreenBuffer %dx%d>\n", self$width, self$height))
      bar <- strrep("-", self$width)
      cat("+", bar, "+\n", sep = "")
      for (line in self$to_text()) cat("|", line, "|\n", sep = "")
      cat("+", bar, "+\n", sep = "")
      invisible(self)
    }
  ),
  private = list(
    check_position = function(x, y) {
      if (x < 1L || y < 1L || x > self$width || y > self$height) {
        stop(sprintf("Position (%d, %d) is outside the %dx%d buffer.", x, y, self$width, self$height),
             call. = FALSE)
      }
    },

    # Before columns first..last of row y are overwritten, break any wide
    # character that is only partially covered by the write.
    fix_wide_edges = function(y, first, last) {
      if (first > 1L && first <= self$width && self$chars[y, first] == "") {
        self$chars[y, first - 1L] <- " "
      }
      if (last < self$width && last >= 1L && self$chars[y, last + 1L] == "") {
        self$chars[y, last + 1L] <- " "
      }
    }
  )
)

# Canonical colour for the framebuffer (theme tokens resolved).
buffer_color <- function(x) {
  col <- normalize_color(x)
  if (startsWith(col, "$")) theme_color(col) else col
}

#' Create a virtual screen buffer
#'
#' @param width,height Size in cells.
#' @return A [ScreenBuffer].
#' @export
#' @examples
#' buf <- screen_buffer(10, 2)
#' buf$put_text(2, 1, "hello", fg = "red")
#' buf$to_text()
screen_buffer <- function(width, height) {
  ScreenBuffer$new(width, height)
}

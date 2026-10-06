# A tiny terminal emulator.
#
# VirtualTerminal interprets the subset of ANSI that termr emits (cursor
# positioning, SGR, clearing and a few private modes) into a ScreenBuffer.
# The headless driver writes into one, so tests can check what a real
# terminal would display after applying termr's output.

VirtualTerminal <- R6::R6Class(
  "VirtualTerminal",
  public = list(
    width = 0L,
    height = 0L,
    screen = NULL,
    cursor_x = 1L,
    cursor_y = 1L,
    cursor_visible = TRUE,
    autowrap = TRUE,
    alt_screen = FALSE,
    # Private modes that are currently enabled (e.g. 1000, 1006 for mouse).
    modes = integer(),
    # Payloads of the OSC sequences received so far (e.g. "52;c;<base64>").
    osc = character(),

    initialize = function(width = 80L, height = 24L) {
      self$width <- as.integer(width)
      self$height <- as.integer(height)
      self$screen <- ScreenBuffer$new(width, height)
      private$main <- ScreenBuffer$new(width, height)
    },

    resize = function(width, height) {
      self$width <- as.integer(width)
      self$height <- as.integer(height)
      self$screen$resize(width, height)
      private$main$resize(width, height)
      self$cursor_x <- min(self$cursor_x, max(1L, self$width))
      self$cursor_y <- min(self$cursor_y, max(1L, self$height))
      invisible(self)
    },

    feed = function(text) {
      if (!nzchar(text)) return(invisible(self))
      chars <- strsplit(enc2utf8(text), "", fixed = TRUE)[[1]]
      n <- length(chars)
      control <- grepl("[\001-\037\177]", chars, useBytes = TRUE)
      final_byte <- grepl("^[@-~]$", chars)
      i <- 1L
      while (i <= n) {
        ch <- chars[[i]]
        if (ch == "\033" && i < n) {
          nxt <- chars[[i + 1L]]
          if (nxt == "[") {
            j <- i + 2L
            while (j <= n && !final_byte[[j]]) j <- j + 1L
            if (j > n) break
            params <- paste(chars[seq_len(j - i - 2L) + i + 1L], collapse = "")
            private$csi(params, chars[[j]])
            i <- j + 1L
          } else if (nxt == "]") {
            j <- i + 2L
            while (j <= n && chars[[j]] != "\007" && chars[[j]] != "\033") j <- j + 1L
            if (j > i + 2L) self$osc <- c(self$osc, paste(chars[(i + 2L):(j - 1L)], collapse = ""))
            i <- j + (if (j <= n && chars[[j]] == "\033") 2L else 1L)
          } else {
            i <- i + 2L
          }
          next
        }
        if (!control[[i]]) {
          # Print a run of text one grapheme cluster at a time.
          j <- i
          while (j < n && !control[[j + 1L]]) j <- j + 1L
          cells <- text_cells(paste(chars[i:j], collapse = ""))
          for (k in seq_along(cells$chars)) private$print_char(cells$chars[[k]], cells$widths[[k]])
          i <- j + 1L
          next
        }
        if (ch == "\r") {
          self$cursor_x <- 1L
        } else if (ch == "\n") {
          self$cursor_y <- min(self$height, self$cursor_y + 1L)
        } else if (ch == "\b") {
          self$cursor_x <- max(1L, self$cursor_x - 1L)
        }
        i <- i + 1L
      }
      invisible(self)
    },

    to_text = function() self$screen$to_text(),

    # Text of the most recent OSC 52 clipboard write, or NULL.
    clipboard_text = function() {
      hits <- grep("^52;", self$osc, value = TRUE)
      if (!length(hits)) return(NULL)
      text <- rawToChar(base64_decode(sub("^52;[^;]*;", "", hits[[length(hits)]])))
      Encoding(text) <- "UTF-8"
      text
    }
  ),

  private = list(
    main = NULL,
    fg = "",
    bg = "",
    attrs = 0L,

    print_char = function(ch, w) {
      if (self$cursor_x + w - 1L > self$width) {
        if (!self$autowrap) {
          self$cursor_x <- max(1L, self$width - w + 1L)
        } else {
          self$cursor_x <- 1L
          self$cursor_y <- min(self$height, self$cursor_y + 1L)
        }
      }
      if (self$width == 0L || self$height == 0L) return()
      self$screen$put_text(
        self$cursor_x, self$cursor_y, ch,
        fg = private$fg, bg = private$bg, attrs = private$attrs
      )
      self$cursor_x <- self$cursor_x + w
    },

    csi = function(params, final) {
      private_mode <- startsWith(params, "?")
      if (private_mode) params <- substring(params, 2L)
      nums <- if (nzchar(params)) suppressWarnings(as.integer(strsplit(params, ";", fixed = TRUE)[[1]])) else integer()
      nums[is.na(nums)] <- 0L
      arg <- function(i, default) if (length(nums) >= i && nums[[i]] > 0L) nums[[i]] else default
      if (private_mode) {
        if (final %in% c("h", "l")) private$set_mode(nums, final == "h")
        return()
      }
      switch(
        final,
        H = ,
        f = {
          self$cursor_y <- min(max(1L, arg(1L, 1L)), self$height)
          self$cursor_x <- min(max(1L, arg(2L, 1L)), self$width)
        },
        A = self$cursor_y <- max(1L, self$cursor_y - arg(1L, 1L)),
        B = self$cursor_y <- min(self$height, self$cursor_y + arg(1L, 1L)),
        C = self$cursor_x <- min(self$width, self$cursor_x + arg(1L, 1L)),
        D = self$cursor_x <- max(1L, self$cursor_x - arg(1L, 1L)),
        J = {
          mode <- if (length(nums)) nums[[1]] else 0L
          if (mode %in% c(2L, 3L)) {
            self$screen$fill(bg = private$bg)
          } else if (mode == 0L) {
            private$erase_line_from_cursor()
            if (self$cursor_y < self$height) {
              self$screen$fill(rect(1L, self$cursor_y + 1L, self$width, self$height - self$cursor_y), bg = private$bg)
            }
          }
        },
        K = private$erase_line_from_cursor(),
        m = private$sgr(nums)
      )
    },

    erase_line_from_cursor = function() {
      if (self$cursor_x <= self$width) {
        self$screen$fill(
          rect(self$cursor_x, self$cursor_y, self$width - self$cursor_x + 1L, 1L),
          bg = private$bg
        )
      }
    },

    set_mode = function(modes, on) {
      for (m in modes) {
        self$modes <- if (on) union(self$modes, m) else setdiff(self$modes, m)
        if (m == 25L) self$cursor_visible <- on
        if (m == 7L) self$autowrap <- on
        if (m == 1049L && on != self$alt_screen) {
          if (on) {
            private$main <- self$screen$copy()
            self$screen <- ScreenBuffer$new(self$width, self$height)
          } else {
            self$screen <- private$main$copy()
          }
          self$alt_screen <- on
        }
      }
    },

    sgr = function(nums) {
      if (length(nums) == 0L) nums <- 0L
      i <- 1L
      set_attr <- function(name, on) {
        bit <- attr_bits[[name]]
        private$attrs <- if (on) bitwOr(private$attrs, bit) else bitwAnd(private$attrs, bitwNot(bit))
      }
      while (i <= length(nums)) {
        code <- nums[[i]]
        if (code == 0L) {
          private$fg <- ""
          private$bg <- ""
          private$attrs <- 0L
        } else if (code %in% c(1L, 2L, 3L, 4L, 5L, 7L, 9L)) {
          set_attr(names(attr_sgr)[match(as.character(code), attr_sgr)], TRUE)
        } else if (code == 22L) {
          set_attr("bold", FALSE)
          set_attr("dim", FALSE)
        } else if (code == 23L) {
          set_attr("italic", FALSE)
        } else if (code == 24L) {
          set_attr("underline", FALSE)
        } else if (code == 27L) {
          set_attr("reverse", FALSE)
        } else if (code == 29L) {
          set_attr("strike", FALSE)
        } else if (code >= 30L && code <= 37L) {
          private$fg <- ansi_color_names[[code - 29L]]
        } else if (code >= 90L && code <= 97L) {
          private$fg <- ansi_color_names[[code - 81L]]
        } else if (code >= 40L && code <= 47L) {
          private$bg <- ansi_color_names[[code - 39L]]
        } else if (code >= 100L && code <= 107L) {
          private$bg <- ansi_color_names[[code - 91L]]
        } else if (code == 39L) {
          private$fg <- ""
        } else if (code == 49L) {
          private$bg <- ""
        } else if (code %in% c(38L, 48L)) {
          color <- ""
          if (i + 2L <= length(nums) && nums[[i + 1L]] == 5L) {
            color <- normalize_color(nums[[i + 2L]])
            i <- i + 2L
          } else if (i + 4L <= length(nums) && nums[[i + 1L]] == 2L) {
            color <- sprintf("#%02x%02x%02x", nums[[i + 2L]], nums[[i + 3L]], nums[[i + 4L]])
            i <- i + 4L
          }
          if (code == 38L) private$fg <- color else private$bg <- color
        }
        i <- i + 1L
      }
    }
  )
)

# Colours.
#
# Colours are stored in the framebuffer in a canonical character form:
#   ""                    terminal default colour
#   "red", "bright_blue"  one of the 16 ANSI palette colours
#   "196"                 xterm 256-colour index (16-255)
#   "#ff8700"             24-bit colour
#
# Any colour accepted by `grDevices::col2rgb()` (e.g. "steelblue") is
# converted to 24-bit. ANSI names take precedence because they follow the
# user's terminal theme.

ansi_color_names <- c(
  "black", "red", "green", "yellow", "blue", "magenta", "cyan", "white",
  "bright_black", "bright_red", "bright_green", "bright_yellow",
  "bright_blue", "bright_magenta", "bright_cyan", "bright_white"
)

color_aliases <- c(
  grey = "bright_black", gray = "bright_black",
  bright_grey = "white", bright_gray = "white"
)

#' Normalise a colour specification
#'
#' @param x A colour: `NULL`/`NA` (terminal default), an ANSI name such as
#'   `"red"` or `"bright_blue"`, an integer 0-255, a hex string `"#rrggbb"`
#'   or any R colour name.
#' @return The canonical colour string used by the framebuffer.
#' @keywords internal
#' @noRd
normalize_color <- function(x) {
  if (is.null(x) || length(x) == 0L) return("")
  if (length(x) != 1L) stop("A colour must be a single value.", call. = FALSE)
  if (is.na(x)) return("")
  if (is.numeric(x)) {
    if (x < 0 || x > 255 || x != round(x)) {
      stop("Numeric colours must be integers between 0 and 255.", call. = FALSE)
    }
    x <- as.integer(x)
    return(if (x < 16L) ansi_color_names[[x + 1L]] else as.character(x))
  }
  if (!is.character(x)) stop("Invalid colour.", call. = FALSE)
  if (grepl("^[0-9]+$", x)) return(normalize_color(as.numeric(x)))
  key <- tolower(gsub("[ -]", "_", x))
  if (key %in% c("", "default", "none")) return("")
  # Theme colour tokens are resolved later (see theme_color()).
  if (grepl("^[$][a-z_][a-z0-9_]*$", key)) return(key)
  if (key %in% names(color_aliases)) return(color_aliases[[key]])
  if (key %in% ansi_color_names) return(key)
  if (grepl("^#[0-9a-f]{6}$", key)) return(key)
  if (grepl("^#[0-9a-f]{3}$", key)) {
    digits <- strsplit(substring(key, 2L), "")[[1]]
    return(paste0("#", paste0(digits, digits, collapse = "")))
  }
  rgb <- tryCatch(grDevices::col2rgb(x), error = function(e) NULL)
  if (is.null(rgb)) stop(sprintf("Unknown colour: \"%s\".", x), call. = FALSE)
  sprintf("#%02x%02x%02x", rgb[[1]], rgb[[2]], rgb[[3]])
}

# Convert hex colours to 0-255 RGB components (matrix with 3 columns).
hex_to_rgb <- function(hex) {
  cbind(
    strtoi(substr(hex, 2L, 3L), 16L),
    strtoi(substr(hex, 4L, 5L), 16L),
    strtoi(substr(hex, 6L, 7L), 16L)
  )
}

# Approximate RGB of the 16 ANSI colours (xterm defaults) for downgrading.
ansi16_rgb <- rbind(
  c(0, 0, 0), c(205, 0, 0), c(0, 205, 0), c(205, 205, 0),
  c(0, 0, 238), c(205, 0, 205), c(0, 205, 205), c(229, 229, 229),
  c(127, 127, 127), c(255, 0, 0), c(0, 255, 0), c(255, 255, 0),
  c(92, 92, 255), c(255, 0, 255), c(0, 255, 255), c(255, 255, 255)
)

xterm256_rgb <- function(index) {
  index <- as.integer(index)
  out <- matrix(0, nrow = length(index), ncol = 3L)
  low <- index < 16L
  out[low, ] <- ansi16_rgb[index[low] + 1L, , drop = FALSE]
  cube <- index >= 16L & index < 232L
  if (any(cube)) {
    i <- index[cube] - 16L
    steps <- c(0, 95, 135, 175, 215, 255)
    out[cube, ] <- cbind(steps[i %/% 36L + 1L], steps[(i %/% 6L) %% 6L + 1L], steps[i %% 6L + 1L])
  }
  grey <- index >= 232L
  if (any(grey)) {
    level <- 8 + (index[grey] - 232L) * 10
    out[grey, ] <- cbind(level, level, level)
  }
  out
}

nearest_color <- function(rgb, palette) {
  vapply(seq_len(nrow(rgb)), function(i) {
    d <- colSums((t(palette) - rgb[i, ])^2)
    which.min(d) - 1L
  }, integer(1))
}

rgb_to_256 <- function(rgb) {
  to_level <- function(v) ifelse(v < 48, 0L, ifelse(v < 115, 1L, as.integer((v - 35) %/% 40)))
  idx <- 16L + 36L * to_level(rgb[, 1]) + 6L * to_level(rgb[, 2]) + to_level(rgb[, 3])
  as.integer(idx)
}

# SGR parameter string for canonical colours (vectorised).
#
# @param mode "truecolor", "256", "16" or "none".
color_sgr <- function(colors, background = FALSE, mode = "truecolor") {
  out <- character(length(colors))
  if (mode == "none" || length(colors) == 0L) return(out)
  uniq <- unique(colors)
  codes <- vapply(uniq, color_sgr_one, character(1), background = background, mode = mode)
  out[] <- codes[match(colors, uniq)]
  out
}

color_sgr_one <- function(color, background, mode) {
  if (!nzchar(color)) return("")
  idx <- match(color, ansi_color_names)
  if (!is.na(idx)) {
    i <- idx - 1L
    base <- if (i < 8L) (if (background) 40L else 30L) else (if (background) 100L else 90L)
    return(as.character(base + i %% 8L))
  }
  prefix <- if (background) "48" else "38"
  if (startsWith(color, "#")) {
    rgb <- hex_to_rgb(color)
    if (mode == "truecolor") {
      return(sprintf("%s;2;%d;%d;%d", prefix, rgb[1], rgb[2], rgb[3]))
    }
    if (mode == "256") {
      return(sprintf("%s;5;%d", prefix, rgb_to_256(rgb)))
    }
    return(color_sgr_one(ansi_color_names[[nearest_color(rgb, ansi16_rgb) + 1L]], background, mode))
  }
  index <- as.integer(color)
  if (mode == "16") {
    nearest <- nearest_color(xterm256_rgb(index), ansi16_rgb)
    return(color_sgr_one(ansi_color_names[[nearest + 1L]], background, mode))
  }
  sprintf("%s;5;%d", prefix, index)
}

# Guess how many colours the terminal supports. `env` is the environment
# (a named character vector or list), so detection can be tested.
detect_color_mode <- function(env = Sys.getenv(), windows = .Platform$OS.type == "windows") {
  opt <- getOption("termr.color_mode")
  if (!is.null(opt)) return(opt)
  if (nzchar(env_value(env, "NO_COLOR"))) return("none")
  colorterm <- tolower(env_value(env, "COLORTERM"))
  remote_session <- nzchar(env_value(env, "SSH_TTY")) ||
    nzchar(env_value(env, "SSH_CONNECTION")) || nzchar(env_value(env, "SSH_CLIENT"))
  if (colorterm %in% c("truecolor", "24bit") ||
      (nzchar(env_value(env, "WT_SESSION")) && !remote_session)) return("truecolor")
  if (windows) return("truecolor")
  term <- tolower(env_value(env, "TERM"))
  if (grepl("256", term, fixed = TRUE)) return("256")
  if (grepl("^vt100($|-)", term)) return("none")
  if (term %in% c("dumb", "")) return("none")
  "16"
}

env_value <- function(env, name) {
  value <- if (name %in% names(env)) env[[name]] else NULL
  if (is.null(value) || length(value) != 1L || is.na(value)) "" else as.character(value)
}

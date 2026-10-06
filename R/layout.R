# Layout engine.
#
# `layout_tree()` assigns a `region` (border box) to every visible widget.
# It never paints. A container arranges its children with the algorithm
# named by its `layout` style property; "vertical" and "horizontal" are
# built in, and new algorithms (grid, dock, ...) can be registered in
# `layout_algorithms`.
#
# Box model: margin (outside the region) | border | padding | content.
#
# Main axis sizes:   fixed -> n, auto -> natural size, "x%" -> share of the
#                    parent content size, "nfr" -> share of what is left.
# Cross axis sizes:  fixed -> n, auto -> natural size, "x%" -> percentage,
#                    "nfr" -> fill the available space.
# Limits: min/max are applied after sizing; space taken back by a max limit
# on a fr child is not redistributed (documented limitation).

layout_tree <- function(widget, region, viewport = NULL) {
  profile_add("widgets_laid_out")
  widget$region <- region
  st <- widget$computed_style()
  if (indexed_layout_tree(widget, region, st, viewport)) return(invisible(widget))
  p <- widget_private(widget)
  p$.render_children <- NULL
  kids <- visible_children(widget)
  for (child in widget$children) {
    if (!child$visible) clear_regions(child)
  }
  if (length(kids) == 0L) return(invisible(widget))
  inner <- content_rect(region, st)
  rects <- widget$arrange_children(kids, inner, st)
  descend <- widget$layout_descend(rects)
  if (inherits(widget, "ScrollView")) {
    viewport <- if (is.null(viewport)) widget$viewport else rect_intersect(viewport, widget$viewport)
    p$.render_children <- kids[descend]
  }
  for (i in seq_along(kids)) {
    if (descend[[i]]) layout_tree(kids[[i]], rects[[i]], viewport) else kids[[i]]$region <- rects[[i]]
  }
  invisible(widget)
}

clear_regions <- function(widget) {
  widget$region <- NULL
  for (child in widget$children) clear_regions(child)
}

visible_children <- function(widget) {
  Filter(function(w) w$visible, widget$children)
}

clamp_size <- function(value, min = NULL, max = NULL) {
  if (!is.null(max)) value <- min(value, max)
  if (!is.null(min)) value <- max(value, min)
  as.integer(max(0L, value))
}

# Natural border-box width of a widget.
natural_width <- function(widget, st = widget$computed_style()) {
  spec <- st$width
  if (spec$type == "fixed") return(clamp_size(spec$value, st$min_width, st$max_width))
  cached <- size_cache_get(widget, "natural_width")
  if (!is.null(cached)) return(cached)
  profile_add("widgets_measured")
  chrome <- chrome_edges(st)
  value <- clamp_size(widget$content_width() + chrome[[2]] + chrome[[4]], st$min_width, st$max_width)
  size_cache_set(widget, "natural_width", value)
}

# Natural border-box height of a widget given its border-box width.
natural_height <- function(widget, width, st = widget$computed_style()) {
  spec <- st$height
  if (spec$type == "fixed") return(clamp_size(spec$value, st$min_height, st$max_height))
  key <- paste0("natural_height_", width)
  cached <- size_cache_get(widget, key)
  if (!is.null(cached)) return(cached)
  profile_add("widgets_measured")
  chrome <- chrome_edges(st)
  inner_width <- max(0L, width - chrome[[2]] - chrome[[4]])
  value <- clamp_size(widget$content_height(inner_width) + chrome[[1]] + chrome[[3]], st$min_height, st$max_height)
  size_cache_set(widget, key, value)
}

# Split `total` cells between items in proportion to `weights`, using the
# largest remainder method so the parts always add up to `total`.
distribute <- function(weights, total) {
  if (length(weights) == 0L) return(integer())
  total <- max(0L, as.integer(total))
  if (sum(weights) <= 0) return(rep(0L, length(weights)))
  shares <- total * weights / sum(weights)
  out <- as.integer(floor(shares))
  rest <- total - sum(out)
  if (rest > 0L) {
    idx <- order(-(shares - out), seq_along(out))[seq_len(rest)]
    out[idx] <- out[idx] + 1L
  }
  out
}

# Size along an axis for every non-fr child; fr children get NA and their
# weight is returned separately.
main_sizes <- function(kids, styles, axis, available, cross_sizes = NULL) {
  n <- length(kids)
  sizes <- rep(NA_integer_, n)
  weights <- rep(0, n)
  for (i in seq_len(n)) {
    st <- styles[[i]]
    spec <- st[[axis]]
    lim <- size_limits(st, axis)
    sizes[[i]] <- switch(
      spec$type,
      fixed = clamp_size(spec$value, lim$min, lim$max),
      percent = clamp_size(floor(spec$value * available), lim$min, lim$max),
      auto = if (axis == "width") {
        natural_width(kids[[i]], st)
      } else {
        natural_height(kids[[i]], cross_sizes[[i]], st)
      },
      fr = {
        weights[[i]] <- spec$value
        NA_integer_
      }
    )
  }
  list(sizes = sizes, weights = weights)
}

cross_size <- function(kid, st, axis, available, total, main_size = NULL) {
  spec <- st[[axis]]
  lim <- size_limits(st, axis)
  value <- switch(
    spec$type,
    fixed = spec$value,
    percent = floor(spec$value * total),
    fr = available,
    auto = if (axis == "width") natural_width(kid, st) else natural_height(kid, main_size, st)
  )
  min(clamp_size(value, lim$min, lim$max), max(0L, available))
}

size_limits <- function(st, axis) {
  if (axis == "width") list(min = st$min_width, max = st$max_width) else list(min = st$min_height, max = st$max_height)
}

resolve_fr <- function(main, styles, axis, remaining) {
  sizes <- main$sizes
  is_fr <- is.na(sizes)
  if (any(is_fr)) {
    sizes[is_fr] <- distribute(main$weights[is_fr], remaining)
    for (i in which(is_fr)) {
      lim <- size_limits(styles[[i]], axis)
      sizes[[i]] <- clamp_size(sizes[[i]], lim$min, lim$max)
    }
  }
  sizes
}

offset_for <- function(free, align) {
  max(0L, switch(align, center = , middle = free %/% 2L, right = , bottom = free, 0L))
}

arrange_vertical <- function(kids, inner, parent_st) {
  styles <- lapply(kids, function(k) k$computed_style(parent_st))
  margins <- lapply(styles, `[[`, "margin")
  widths <- vapply(seq_along(kids), function(i) {
    m <- margins[[i]]
    cross_size(kids[[i]], styles[[i]], "width", inner$width - m[[2]] - m[[4]], inner$width)
  }, integer(1))
  main <- main_sizes(kids, styles, "height", inner$height, widths)
  margin_total <- sum(vapply(margins, function(m) m[[1]] + m[[3]], integer(1)))
  remaining <- inner$height - sum(main$sizes, na.rm = TRUE) - margin_total
  heights <- resolve_fr(main, styles, "height", remaining)
  y <- inner$y + if (any(is.na(main$sizes))) 0L else offset_for(inner$height - sum(heights) - margin_total, parent_st$valign)
  rects <- vector("list", length(kids))
  for (i in seq_along(kids)) {
    m <- margins[[i]]
    y <- y + m[[1]]
    free <- inner$width - m[[2]] - m[[4]] - widths[[i]]
    x <- inner$x + m[[4]] + offset_for(free, parent_st$align)
    rects[[i]] <- rect(x, y, widths[[i]], heights[[i]])
    y <- y + heights[[i]] + m[[3]]
  }
  rects
}

arrange_horizontal <- function(kids, inner, parent_st) {
  styles <- lapply(kids, function(k) k$computed_style(parent_st))
  margins <- lapply(styles, `[[`, "margin")
  main <- main_sizes(kids, styles, "width", inner$width)
  margin_total <- sum(vapply(margins, function(m) m[[2]] + m[[4]], integer(1)))
  remaining <- inner$width - sum(main$sizes, na.rm = TRUE) - margin_total
  widths <- resolve_fr(main, styles, "width", remaining)
  heights <- vapply(seq_along(kids), function(i) {
    m <- margins[[i]]
    cross_size(kids[[i]], styles[[i]], "height", inner$height - m[[1]] - m[[3]], inner$height, widths[[i]])
  }, integer(1))
  x <- inner$x + if (any(is.na(main$sizes))) 0L else offset_for(inner$width - sum(widths) - margin_total, parent_st$align)
  rects <- vector("list", length(kids))
  for (i in seq_along(kids)) {
    m <- margins[[i]]
    x <- x + m[[4]]
    free <- inner$height - m[[1]] - m[[3]] - heights[[i]]
    y <- inner$y + m[[1]] + offset_for(free, parent_st$valign)
    rects[[i]] <- rect(x, y, widths[[i]], heights[[i]])
    x <- x + widths[[i]] + m[[2]]
  }
  rects
}

# Natural content size of a container, per layout algorithm.
measure_vertical <- function(kids, parent_st) {
  list(
    width = function() {
      max(0L, vapply(kids, function(k) {
        st <- k$computed_style(parent_st)
        natural_width(k, st) + st$margin[[2]] + st$margin[[4]]
      }, integer(1)))
    },
    height = function(width) {
      sum(vapply(kids, function(k) {
        st <- k$computed_style(parent_st)
        m <- st$margin
        w <- cross_size(k, st, "width", width - m[[2]] - m[[4]], width)
        natural_height(k, w, st) + m[[1]] + m[[3]]
      }, integer(1)))
    }
  )
}

measure_horizontal <- function(kids, parent_st) {
  list(
    width = function() {
      sum(vapply(kids, function(k) {
        st <- k$computed_style(parent_st)
        natural_width(k, st) + st$margin[[2]] + st$margin[[4]]
      }, integer(1)))
    },
    height = function(width) {
      styles <- lapply(kids, function(k) k$computed_style(parent_st))
      main <- main_sizes(kids, styles, "width", width)
      margin_total <- sum(vapply(styles, function(s) s$margin[[2]] + s$margin[[4]], integer(1)))
      widths <- resolve_fr(main, styles, "width", width - sum(main$sizes, na.rm = TRUE) - margin_total)
      max(0L, vapply(seq_along(kids), function(i) {
        m <- styles[[i]]$margin
        natural_height(kids[[i]], widths[[i]], styles[[i]]) + m[[1]] + m[[3]]
      }, integer(1)))
    }
  )
}

# Registry of layout algorithms. Each entry provides `arrange(kids, inner,
# parent_style)` returning one rect per child and `measure(kids,
# parent_style)` returning natural content `width()` / `height(width)`.
layout_algorithms <- list2env(list(
  vertical = list(arrange = arrange_vertical, measure = measure_vertical),
  horizontal = list(arrange = arrange_horizontal, measure = measure_horizontal),
  grid = list(arrange = arrange_grid, measure = measure_grid)
), parent = emptyenv())

#' Register an experimental custom container layout
#'
#' A registered name can be used as `layout` in [style()]. Custom names start
#' with `custom_` to keep built-in names reserved.
#' @param name A unique `custom_` name.
#' @param arrange `function(children, inner, parent_style)` returning one
#'   [region()] per child.
#' @param measure `function(children, parent_style)` returning `width()` and
#'   `height(width)` functions.
#' @return The registered name, invisibly.
#' @export
register_layout <- function(name, arrange, measure) {
  check_scalar_character(name, "name")
  if (!grepl("^custom_[a-z][a-z0-9_]*$", name)) {
    stop("Custom layout names must start with `custom_` and use lowercase letters, digits, and underscores.", call. = FALSE)
  }
  if (exists(name, layout_algorithms, inherits = FALSE)) stop(sprintf("Layout `%s` is already registered.", name), call. = FALSE)
  check_function(arrange, "arrange")
  check_function(measure, "measure")
  assign(name, list(arrange = arrange, measure = measure), envir = layout_algorithms)
  invisible(name)
}

#' Remove an experimental custom layout
#'
#' Built-in layouts cannot be removed.
#' @param name A registered `custom_` layout name.
#' @return `TRUE` when the layout was removed.
#' @export
unregister_layout <- function(name) {
  check_scalar_character(name, "name")
  if (!startsWith(name, "custom_")) stop("Built-in layouts cannot be removed.", call. = FALSE)
  if (!exists(name, layout_algorithms, inherits = FALSE)) return(invisible(FALSE))
  rm(list = name, envir = layout_algorithms)
  invisible(TRUE)
}

# Grid layout: a practical subset of CSS grid.
#
# The container's style gives the column tracks (`grid_columns`), optional
# row tracks (`grid_rows`, default "auto", extended as needed) and the gap.
# Children are auto-placed in row-major order; `column_span` / `row_span`
# make a child cover several cells (spans larger than the grid are
# clamped). Track sizes:
#   fixed     exactly n cells
#   percent   share of the container's content size
#   auto      the largest natural size of the single-span children in it
#   fr        a share of what is left (largest remainder, sums exactly)
# Children fill their cell area (minus margins); a fixed width or height
# smaller than the area is honoured and placed at the start.
# Simplification: spanning children do not influence auto track sizes.

grid_placement <- function(kids, styles, ncol) {
  occupied <- matrix(FALSE, nrow = 0L, ncol = ncol)
  cells <- vector("list", length(kids))
  r <- 1L
  c <- 1L
  for (i in seq_along(kids)) {
    cs <- min(styles[[i]]$column_span, ncol)
    rs <- styles[[i]]$row_span
    repeat {
      if (c + cs - 1L > ncol) {
        r <- r + 1L
        c <- 1L
      }
      need <- r + rs - 1L
      if (nrow(occupied) < need) {
        occupied <- rbind(occupied, matrix(FALSE, need - nrow(occupied), ncol))
      }
      if (!any(occupied[r:need, c:(c + cs - 1L)])) break
      c <- c + 1L
    }
    occupied[r:(r + rs - 1L), c:(c + cs - 1L)] <- TRUE
    cells[[i]] <- c(row = r, col = c, rows = rs, cols = cs)
    c <- c + cs
  }
  list(cells = cells, nrow = nrow(occupied))
}

# Sizes of tracks along one axis. `natural(track)` gives the natural size
# of an auto track.
grid_track_sizes <- function(specs, total, gap, natural, fill_fr = TRUE) {
  n <- length(specs)
  sizes <- integer(n)
  weights <- numeric(n)
  avail <- if (is.na(total)) NA_integer_ else max(0L, total - gap * (n - 1L))
  for (i in seq_len(n)) {
    spec <- specs[[i]]
    sizes[[i]] <- switch(
      spec$type,
      fixed = as.integer(spec$value),
      percent = if (is.na(avail)) as.integer(natural(i)) else as.integer(floor(spec$value * avail)),
      auto = as.integer(natural(i)),
      fr = {
        weights[[i]] <- spec$value
        if (fill_fr) 0L else as.integer(natural(i))
      }
    )
  }
  fr <- weights > 0
  if (fill_fr && any(fr)) {
    sizes[fr] <- distribute(weights[fr], avail - sum(sizes[!fr]))
  }
  sizes
}

grid_row_specs <- function(st, nrow) {
  rows <- st$grid_rows %||% list()
  if (length(rows) < nrow) rows <- c(rows, rep(list(size_spec("auto")), nrow - length(rows)))
  rows
}

track_offsets <- function(sizes, gap) {
  if (length(sizes) == 0L) return(integer())
  c(0L, cumsum(sizes + gap))[seq_along(sizes)]
}

span_size <- function(sizes, first, n, gap) {
  sum(sizes[first:(first + n - 1L)]) + gap * (n - 1L)
}

grid_plan <- function(kids, width, height, parent_st, fill_rows = TRUE) {
  styles <- lapply(kids, function(k) k$computed_style(parent_st))
  col_specs <- parent_st$grid_columns
  ncol <- length(col_specs)
  gap <- parent_st$grid_gap
  placement <- grid_placement(kids, styles, ncol)
  cells <- placement$cells
  margins_h <- function(i) styles[[i]]$margin[[2]] + styles[[i]]$margin[[4]]
  margins_v <- function(i) styles[[i]]$margin[[1]] + styles[[i]]$margin[[3]]
  single_in <- function(axis, track) {
    which(vapply(cells, function(cell) {
      if (axis == "col") cell[["col"]] == track && cell[["cols"]] == 1L else cell[["row"]] == track && cell[["rows"]] == 1L
    }, logical(1)))
  }
  col_natural <- function(track) {
    idx <- single_in("col", track)
    max(0L, vapply(idx, function(i) natural_width(kids[[i]], styles[[i]]) + margins_h(i), integer(1)))
  }
  col_sizes <- grid_track_sizes(col_specs, width, gap[[2]], col_natural, fill_fr = !is.na(width))
  row_specs <- grid_row_specs(parent_st, placement$nrow)
  row_natural <- function(track) {
    idx <- single_in("row", track)
    max(0L, vapply(idx, function(i) {
      cell <- cells[[i]]
      w <- span_size(col_sizes, cell[["col"]], cell[["cols"]], gap[[2]]) - margins_h(i)
      natural_height(kids[[i]], max(0L, w), styles[[i]]) + margins_v(i)
    }, integer(1)))
  }
  row_sizes <- grid_track_sizes(row_specs, height, gap[[1]], row_natural, fill_fr = fill_rows && !is.na(height))
  list(styles = styles, cells = cells, cols = col_sizes, rows = row_sizes, gap = gap)
}

arrange_grid <- function(kids, inner, parent_st) {
  plan <- grid_plan(kids, inner$width, inner$height, parent_st)
  col_x <- track_offsets(plan$cols, plan$gap[[2]])
  row_y <- track_offsets(plan$rows, plan$gap[[1]])
  lapply(seq_along(kids), function(i) {
    cell <- plan$cells[[i]]
    st <- plan$styles[[i]]
    m <- st$margin
    area_w <- span_size(plan$cols, cell[["col"]], cell[["cols"]], plan$gap[[2]]) - m[[2]] - m[[4]]
    area_h <- span_size(plan$rows, cell[["row"]], cell[["rows"]], plan$gap[[1]]) - m[[1]] - m[[3]]
    w <- if (st$width$type == "fixed") min(st$width$value, area_w) else area_w
    h <- if (st$height$type == "fixed") min(st$height$value, area_h) else area_h
    rect(
      inner$x + col_x[[cell[["col"]]]] + m[[4]], inner$y + row_y[[cell[["row"]]]] + m[[1]],
      max(0L, w), max(0L, h)
    )
  })
}

measure_grid <- function(kids, parent_st) {
  list(
    width = function() {
      plan <- grid_plan(kids, NA_integer_, NA_integer_, parent_st, fill_rows = FALSE)
      sum(plan$cols) + plan$gap[[2]] * max(0L, length(plan$cols) - 1L)
    },
    height = function(width) {
      plan <- grid_plan(kids, width, NA_integer_, parent_st, fill_rows = FALSE)
      sum(plan$rows) + plan$gap[[1]] * max(0L, length(plan$rows) - 1L)
    }
  )
}

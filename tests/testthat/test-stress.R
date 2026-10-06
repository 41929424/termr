# Randomised stress tests: sizes and widget-tree operations. Failures report
# the seed and the operation sequence so they can be replayed.

stress_ui <- function() {
  vertical(
    horizontal(
      grid_layout(label("a"), label("b"), label("c", style = style(column_span = 2)), columns = c("1fr", "2fr"), gap = 1,
                  id = "grid", style = style(height = 4)),
      scroll_view(vertical(lapply(1:12, function(i) button(paste("Button", i), id = paste0("btn", i)))), id = "scroll",
                  style = style(height = 6, width = 20)),
      panel(label("panel"), title = "P", id = "panel", style = style(width = 12, height = 4))
    ),
    tabs(
      tab("Tree", tree_view(tree_node("root", tree_node("a", "a1", "a2"), "b", expanded = TRUE), id = "tree")),
      tab("Table", data_table(data.frame(n = 1:200, s = "x"), id = "tbl", cursor = "cell", frozen_columns = 1)),
      tab("Edit", text_area(paste("line", 1:30, collapse = "\n"), id = "ta")),
      tab("Misc", vertical(dropdown(c("one", "two", "three"), id = "dd"), input("x", id = "in"),
                           checkbox("c", id = "cb"), radio_set(radio_button("r1"), radio_button("r2"), id = "rs"))),
      id = "tabs"
    )
  )
}

check_geometry <- function(a, width, height) {
  for (screen in a$screens) {
    for (w in screen$walk()) {
      r <- w$region
      if (is.null(r)) next
      if (r$width < 0L || r$height < 0L || is.na(r$x) || is.na(r$y)) return(sprintf("invalid region for %s", w$format()))
    }
  }
  tbl <- a$query("#tbl")
  for (t in tbl) if (t$offset_row < 0L || t$offset_row > max(0L, t$row_count)) return("table offset out of range")
  sc <- a$query("#scroll")
  for (s in sc) if (any(unlist(s$offsets) < 0)) return("scroll offset negative")
  NULL
}

well_formed_ansi <- function(output) {
  text <- paste(output, collapse = "")
  stripped <- strip_ansi(text)
  !grepl("\033", stripped, fixed = TRUE)
}

test_that("resizing through extreme sizes never breaks the UI", {
  sizes <- list(c(1, 1), c(2, 2), c(5, 3), c(10, 5), c(40, 10), c(80, 24), c(120, 40), c(200, 60), c(300, 3), c(3, 80))
  for (seed in 1:4) {
    set.seed(seed)
    a <- app(stress_ui())
    pilot <- test_app(a, 80, 24)
    log <- character()
    for (i in 1:30) {
      size <- if (runif(1) < 0.6) sample(sizes, 1)[[1]] else c(sample(1:150, 1), sample(1:50, 1))
      log <- c(log, paste(size, collapse = "x"))
      res <- tryCatch({
        pilot$resize(size[[1]], size[[2]])
        pilot$press(sample(c("tab", "down", "pagedown", "right", "end", "enter"), 1))
        NULL
      }, error = function(e) conditionMessage(e))
      problem <- res %||% check_geometry(a, size[[1]], size[[2]])
      if (is.null(problem)) {
        text <- pilot$screen_text()
        if (length(text) != size[[2]] || any(nchar(text) != size[[1]])) problem <- "screen text has wrong dimensions"
      }
      if (!is.null(problem)) {
        fail(sprintf("seed %d, sizes %s: %s", seed, paste(log, collapse = " "), problem))
        break
      }
    }
    expect_true(well_formed_ansi(pilot$driver$output), info = paste("seed", seed))
    expect_true(a$frame$equals(full_frame(a)), info = paste("seed", seed))
  }
})

focus_valid <- function(a) {
  f <- a$focused
  if (is.null(f)) return(TRUE)
  active <- a$screen
  isTRUE(f$is_displayed()) && f$is_enabled() && isTRUE(f$focusable) &&
    (identical(f, active) || any(vapply(f$ancestors(), identical, logical(1), active))) &&
    !is.null(f$app)
}

test_that("focus is always a valid widget under random tree changes", {
  for (seed in 1:5) {
    set.seed(seed)
    a <- app(stress_ui())
    pilot <- test_app(a, 80, 30)
    n <- 0L
    focusable <- function() Filter(function(w) isTRUE(w$focusable), a$screen$walk())
    pick <- function(xs) if (length(xs)) xs[[sample(length(xs), 1)]] else NULL
    ops <- list(
      focus_next = function() pilot$press("tab"),
      focus_prev = function() pilot$press("shift+tab"),
      click = function() pilot$click(sample(1:80, 1), sample(1:30, 1)),
      disable = function() { w <- pick(focusable()); if (!is.null(w)) w$set(disabled = TRUE) },
      enable = function() for (w in a$screen$walk()) if (w$disabled) { w$set(disabled = FALSE); break },
      hide = function() { w <- pick(focusable()); if (!is.null(w)) w$set(visible = FALSE) },
      show = function() for (w in a$screen$walk()) if (!w$visible) { w$set(visible = TRUE); break },
      mount = function() { n <<- n + 1L; a$query_one("#grid")$mount(button("new", id = paste0("new", n))) },
      remove = function() { w <- pick(focusable()); if (!is.null(w) && !is.null(w$parent)) w$remove() },
      remove_focused = function() if (!is.null(a$focused) && !is.null(a$focused$parent)) a$focused$remove(),
      disable_focused = function() if (!is.null(a$focused)) a$focused$set(disabled = TRUE),
      push = function() a$push_screen(vertical(button("s1", id = paste0("s", sample(1e6, 1))), input(id = paste0("i", sample(1e6, 1))))),
      modal = function() a$push_screen(modal(button("m", id = paste0("m", sample(1e6, 1))))),
      pop = function() if (length(a$screens) > 1) a$pop_screen(),
      tab = function() { t <- a$query("#tabs"); if (length(t)) t[[1]]$activate(sample(1:4, 1)) },
      clear = function() { w <- a$query("#grid"); if (length(w) && runif(1) < 0.3) w[[1]]$clear() },
      resize = function() pilot$resize(sample(10:100, 1), sample(5:40, 1))
    )
    log <- character()
    for (i in 1:80) {
      nm <- sample(names(ops), 1)
      log <- c(log, nm)
      err <- tryCatch({ ops[[nm]](); pilot$step(); NULL }, error = function(e) conditionMessage(e))
      if (!is.null(err) || !focus_valid(a)) {
        fail(sprintf("seed %d step %d: %s\nops: %s", seed, i, err %||% "invalid focus", paste(log, collapse = " ")))
        break
      }
    }
    expect_true(focus_valid(a), info = paste("seed", seed))
  }
})

test_that("a focused child scrolls into view and stays valid across resizes", {
  a <- app(vertical(scroll_view(vertical(lapply(1:30, function(i) button(paste("B", i), id = paste0("b", i)))),
                                id = "sv"), label("footer")))
  pilot <- test_app(a, 30, 12)
  sv <- a$query_one("#sv")
  for (i in 1:12) pilot$press("tab")
  b <- a$focused
  area <- visible_area(b)
  expect_false(is.null(area))
  expect_gt(area$width * area$height, 0)
  for (size in list(c(30, 5), c(10, 3), c(60, 20), c(30, 12))) {
    pilot$resize(size[[1]], size[[2]])
    expect_true(focus_valid(a))
    if (size[[2]] >= 5) expect_false(is.null(visible_area(a$focused)))
  }
})

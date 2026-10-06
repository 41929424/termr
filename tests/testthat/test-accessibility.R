widgets_under_test <- function() {
  list(
    button = function() button("Press"),
    input = function() input("text"),
    checkbox = function() checkbox("Check"),
    radio = function() radio_set(radio_button("a"), radio_button("b")),
    dropdown = function() dropdown(c("one", "two")),
    option_list = function() option_list(c("one", "two", "three")),
    data_table = function() data_table(data.frame(a = 1:5, b = letters[1:5])),
    tree = function() tree_view(tree_node("root", "a", "b", expanded = TRUE)),
    tabs = function() tabs(tab("One", label("1")), tab("Two", label("2"))),
    text_area = function() text_area("hello\nworld")
  )
}

cells_of <- function(pilot) {
  s <- pilot$driver$terminal$screen
  list(chars = s$chars, attrs = s$attrs)
}

test_that("focus is visible without colour for every interactive widget", {
  for (name in names(widgets_under_test())) {
    make <- widgets_under_test()[[name]]
    a <- app(vertical(make(), label("pad")))
    pilot <- test_app(a, 30, 12, color_mode = "none")
    expect_false(is.null(a$focused), info = name)
    focused <- cells_of(pilot)
    a$set_focus(NULL)
    pilot$step()
    plain <- cells_of(pilot)
    differs <- !identical(focused$chars, plain$chars) || !identical(focused$attrs, plain$attrs)
    expect_true(differs, info = paste(name, "looks the same focused and unfocused without colour"))
    # No colour sequences were sent.
    out <- paste(pilot$driver$output, collapse = "")
    expect_false(grepl("\u001b\\[[0-9;]*(3[0-7]|4[0-7]|38|48|9[0-7]|10[0-7])m", out), info = name)
  }
})

test_that("selection and cursor are reverse video without colour", {
  a <- app(text_area("select me"))
  pilot <- test_app(a, 20, 3, color_mode = "none")
  pilot$press("shift+right", "shift+right")
  attrs <- pilot$driver$terminal$screen$attrs
  plain <- attrs_encode(reverse = TRUE)
  expect_true(any(bitwAnd(attrs[2, 2:3], plain) > 0))
  expect_false(any(bitwAnd(attrs[2, 6:8], plain) > 0))
})

test_that("errors and disabled controls are marked without colour", {
  a <- app(vertical(input("", id = "i", validate = function(v) "required"), button("Off", id = "b", disabled = TRUE)))
  pilot <- test_app(a, 30, 8, color_mode = "none")
  st <- a$query_one("#i")$computed_style()
  expect_identical(st$border, "heavy")  # focused: stronger border
  b <- a$query_one("#b")$computed_style()
  expect_gt(bitwAnd(b$attrs, attrs_encode(dim = TRUE)), 0L)
})

test_that("NO_COLOR selects the monochrome profile", {
  withr::local_envvar(NO_COLOR = "1", COLORTERM = "", TERM = "xterm-256color")
  expect_identical(detect_color_mode(), "none")
  expect_identical(terminal_capabilities(windows = FALSE)$colors, "none")
})

test_that("the high-contrast theme exists and strengthens focus", {
  th <- termr_theme("high-contrast")
  expect_true(th$strong_focus)
  expect_identical(th$colors$background, "#000000")
  a <- app(vertical(input("x", id = "i"), button("b")), theme = "high-contrast")
  pilot <- test_app(a, 30, 8)
  expect_identical(a$query_one("#i")$computed_style()$border, "heavy")
  pilot$press("tab")
  expect_identical(a$query_one("#i")$computed_style()$border, "round")
  # Contrast between foreground and background is maximal.
  contrast <- function(a, b) {
    lum <- function(hex) {
      v <- strtoi(substring(hex, c(2, 4, 6), c(3, 5, 7)), 16L) / 255
      v <- ifelse(v <= 0.03928, v / 12.92, ((v + 0.055) / 1.055)^2.4)
      sum(v * c(0.2126, 0.7152, 0.0722))
    }
    l <- sort(c(lum(a), lum(b)), decreasing = TRUE)
    (l[[1]] + 0.05) / (l[[2]] + 0.05)
  }
  for (pair in list(c("foreground", "background"), c("on_primary", "primary"), c("muted", "background"),
                    c("error", "background"), c("accent", "background"))) {
    expect_gte(contrast(th$colors[[pair[[1]]]], th$colors[[pair[[2]]]]), 7, label = paste(pair, collapse = "/"))
  }
})

test_that("themes render: default, dark, light, high-contrast", {
  for (theme in c("default", "dark", "light", "high-contrast")) {
    a <- app(vertical(button("OK", variant = "primary"), input("abc"), data_table(data.frame(x = 1:3))), theme = theme)
    pilot <- test_app(a, 30, 12)
    expect_gt(sum(nzchar(trimws(pilot$screen_text()))), 3, label = theme)
  }
})

test_that("snapshots: profiles and themes", {
  ui <- function() vertical(
    button("Save", id = "save", variant = "primary"),
    input("name", id = "name", validate = function(v) if (v == "bad") "no"),
    checkbox("Flag", value = TRUE),
    option_list(c("alpha", "beta"), id = "ol", style = style(height = 3))
  )
  for (mode in c("truecolor", "256", "16", "none")) {
    pilot <- test_app(app(ui()), 24, 12, color_mode = mode)
    pilot$press("tab")
    expect_snapshot(pilot$snapshot())
    if (mode == "none") expect_snapshot(cat(attr_map(pilot), sep = "\n"))
    # Distinct states keep distinct renderings in every profile.
    expect_gt(length(unique(c(pilot$driver$terminal$screen$attrs))), 1L)
  }
  for (theme in c("dark", "light", "high-contrast")) {
    pilot <- test_app(app(ui(), theme = theme), 24, 12)
    expect_snapshot(pilot$snapshot())
  }
})

test_that("reduce motion: animate jumps to the final value", {
  bar <- progress_bar(0, id = "pb")
  a <- app(bar, reduce_motion = TRUE)
  pilot <- test_app(a, 20, 2)
  done <- FALSE
  anim <- animate(bar, "value", 1, duration = 2, on_complete = function(w, app) done <<- TRUE)
  expect_identical(bar$value, 1)
  expect_true(done)
  expect_false(anim$active)
  # Without it the value moves over time.
  bar2 <- progress_bar(0)
  b <- app(bar2, reduce_motion = FALSE)
  p2 <- test_app(b, 20, 2)
  animate(bar2, "value", 1, duration = 2)
  p2$advance(0.5)
  expect_gt(bar2$value, 0)
  expect_lt(bar2$value, 1)
})

test_that("reduce motion follows the option and environment variable", {
  expect_false(motion_reduced())
  withr::with_options(list(termr.reduce_motion = TRUE), expect_true(motion_reduced()))
  withr::with_envvar(c(TERMR_REDUCE_MOTION = "1"), expect_true(motion_reduced()))
  a <- app(label("x"), reduce_motion = FALSE)
  withr::with_options(list(termr.reduce_motion = TRUE), expect_false(motion_reduced(a)))
})

test_that("reduce motion keeps spinners still and timers working", {
  sp <- spinner("working", id = "sp")
  a <- app(vertical(sp, label("x")), reduce_motion = TRUE)
  pilot <- test_app(a, 20, 2)
  before <- pilot$screen_text()[[1]]
  pilot$advance(2)
  expect_identical(pilot$screen_text()[[1]], before)
  expect_match(before, "working")
  ticks <- 0L
  a$set_interval(1, function(app) ticks <<- ticks + 1L)
  pilot$advance(3)
  expect_identical(ticks, 3L)
  moving <- spinner("working")
  b <- app(moving, reduce_motion = FALSE)
  p2 <- test_app(b, 20, 1)
  first <- p2$screen_text()[[1]]
  p2$advance(0.35)
  expect_false(identical(p2$screen_text()[[1]], first))
})

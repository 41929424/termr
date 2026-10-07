test_that("plain text exports use the static framebuffer and trim only row tails", {
  ui <- label("a  \u754c", style = style(foreground = "red"))
  expect_identical(render_text(ui, width = 8, height = 2), "a  \u754c\n")
  expect_identical(render_lines(ui, width = 8, height = 2, trim = FALSE)[[1]], "a  \u754c   ")
  expect_identical(render_lines(ui, width = 8, height = 2)[[1]], "a  \u754c")
  expect_identical(render_text(ui, width = 8, height = 2), render_text(ui, width = 8, height = 2))
})

test_that("Markdown chooses a fence longer than screen backtick runs", {
  ui <- label("``` <hello>")
  md <- render_markdown(ui, width = 20, height = 1)
  expect_match(md, "^````text\\n")
  expect_true(endsWith(md, "\n````"))
})

test_that("HTML and SVG escape painted user text and preserve style runs", {
  ui <- label("<script>&\"'", style = style(foreground = "red", bold = TRUE, underline = TRUE))
  html <- render_html(ui, width = 20, height = 1)
  expect_match(html, "&lt;script&gt;&amp;&quot;&#39;")
  expect_match(html, "font-weight:bold")
  expect_match(html, "text-decoration:underline")
  withr::local_envvar(NO_COLOR = "1")
  expect_match(render_html(label("styled", style = style(foreground = "red"))), "color:#cd0000")
  svg <- render_svg(ui, width = 20, height = 1)
  expect_match(svg, "&lt;script&gt;&amp;&quot;'")
  expect_false(grepl("<script>", svg, fixed = TRUE))
  expect_error(render_svg(label("x"), font_family = "x\"><script>"), "font_family")
  expect_true(grepl("<rect", render_svg(label("x", style = style(background = "blue")), 2, 1), fixed = TRUE))
})

test_that("screen snapshots are deterministic and record styles in row-major runs", {
  ui <- label("ok", style = style(foreground = "green", bold = TRUE))
  a <- screen_snapshot(ui, width = 5, height = 1)
  b <- screen_snapshot(ui, width = 5, height = 1)
  expect_identical(a, b)
  expect_equal(a$schema_version, 1L)
  expect_equal(a$width, 5L)
  expect_true(length(a$runs) >= 1L)
  expect_true(all(vapply(a$runs, function(run) all(c("x", "y", "width", "text", "fg", "bg", "attrs") %in% names(run)), logical(1))))
})

test_that("widget snapshots omit arbitrary and password input values", {
  password <- input(value = "not-for-export", password = TRUE, id = "secret")
  tree <- widget_snapshot(vertical(password))
  json <- widget_snapshot_json(vertical(input(value = "another-secret", password = TRUE)))
  expect_false("state" %in% names(tree))
  expect_false(grepl("not-for-export|another-secret", json))
  expect_equal(tree$children[[1]]$id, "secret")
  expect_equal(tree$schema_version, 1L)
  expect_null(tree$children[[1]]$schema_version)
  expect_false(exists("snapshot_json", envir = asNamespace("termr"), inherits = FALSE))
  expect_false(exists("inspect_json", envir = asNamespace("termr"), inherits = FALSE))
})

test_that("JSON and knitr integrations work when their optional packages exist", {
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    json <- screen_snapshot_json(label("ok"), width = 4, height = 1)
    expect_match(json, '"schema_version"')
    parsed <- jsonlite::fromJSON(json)
    expect_equal(parsed$schema_version, 1L)
  }
  if (requireNamespace("knitr", quietly = TRUE)) {
    out <- knit_termr(label("ok"), width = 4, height = 1)
    expect_s3_class(out, "knit_asis")
    html <- knit_termr(label("ok"), width = 4, height = 1, format = "html")
    expect_s3_class(html, "knit_asis")
  }
})

test_that("static exporters accept a ScreenBuffer and writer writes expected text", {
  buf <- screen_buffer(4, 1)
  buf$put_text(1, 1, "yes")
  expect_identical(render_text(buf), "yes")
  path <- tempfile()
  write_rendered(buf, path, "text")
  expect_identical(readLines(path, warn = FALSE), "yes")
})

test_that("HTML export keeps inner spaces and SVG preserves whitespace", {
  w <- horizontal(label("ab", style = style(width = 6)), label("cd", style = style(foreground = "red")))
  html <- render_html(w, 12, 1)
  expect_match(html, "ab {4}<")
  expect_match(render_svg(label("a   b"), 10, 1), 'xml:space="preserve"', fixed = TRUE)
})

test_that("stylesheets parse rules, comments and selector lists", {
  sheet <- stylesheet("
    /* buttons */
    Button { border: round; padding: 0 2; }
    Button.primary, #save { background: $primary; foreground: white; }
    Input:focus { border: heavy cyan; }
    Vertical > Label { bold: true; text-align: center; }
  ")
  expect_s3_class(sheet, "termr_stylesheet")
  expect_length(sheet, 5) # the list counts as two rules
  rules <- unclass(sheet)
  expect_identical(rules[[1]]$style$props$padding, c(0L, 2L, 0L, 2L))
  expect_identical(rules[[2]]$style$props$background, "$primary")
  expect_identical(rules[[2]]$specificity, c(0L, 1L, 1L))
  expect_identical(rules[[3]]$specificity, c(1L, 0L, 0L))
  expect_identical(rules[[4]]$style$props[c("border", "border_color")], list(border = "heavy", border_color = "cyan"))
  expect_identical(rules[[5]]$style$props$align, "center")
  expect_true(rules[[5]]$style$props$bold)
})

test_that("stylesheet errors report line, column and property", {
  expect_error(stylesheet("Button {\n  border: wavy;\n}"), "<stylesheet>:2:3: property \"border\"")
  expect_error(stylesheet("Button { bold: maybe; }"), "expected true or false")
  expect_error(stylesheet("Button { sparkle: 1; }"), "property \"sparkle\": unknown property")
  expect_error(stylesheet("Button { width: 1 2; }"), "expected one value")
  expect_error(stylesheet("Button { color red; }"), "expected \"property: value\"")
  expect_error(stylesheet("Button { border: round;"), "missing \"\\}\"")
  expect_error(stylesheet("a b c"), "expected \"\\{\"")
  expect_error(stylesheet("La!bel { bold: true; }"), "invalid selector")
  expect_error(stylesheet("Button { color: $; }"), "Unknown colour")
  expect_error(stylesheet("} Button {}"), "1:1: unexpected")
})

test_that("rules style matching widgets with the documented cascade", {
  a <- app(
    vertical(
      button("Save", id = "save", classes = "primary"),
      button("Other", id = "other", style = style(border = "double")),
      label("note", classes = "note")
    ),
    stylesheet = "
      Button { border: heavy; }
      #save { border: ascii; }
      .primary { border: single; }  /* less specific than #save */
      .note { foreground: red; }
      Label { foreground: green; }  /* less specific than .note */
    "
  )
  pilot <- test_app(a, 30, 8)
  expect_identical(a$query_one("#save")$computed_style()$border, "ascii")
  # The widget's own style beats the stylesheet.
  expect_identical(a$query_one("#other")$computed_style()$border, "double")
  expect_identical(a$query_one(".note")$computed_style()$foreground, "red")
})

test_that("later rules win on equal specificity and stylesheets stack", {
  a <- app(label("x", id = "x"), stylesheet = list("Label { foreground: red; }", "Label { foreground: blue; }"))
  pilot <- test_app(a, 10, 2)
  expect_identical(a$query_one("#x")$computed_style()$foreground, "blue")
  a$add_stylesheet("Label { foreground: green; } Label { foreground: yellow; }")
  pilot$step()
  expect_identical(a$query_one("#x")$computed_style()$foreground, "yellow")
  a$clear_stylesheets()
  expect_identical(a$query_one("#x")$computed_style()$foreground, "")
})

test_that("pseudo classes follow focus, hover and widget states", {
  a <- app(
    vertical(input(id = "a"), input(id = "b"), button("Go", id = "go")),
    stylesheet = "Input:focus { border-color: red; } Button:hover { background: green; } Button:pressed { background: yellow; }"
  )
  pilot <- test_app(a, 30, 10)
  expect_identical(a$query_one("#a")$computed_style()$border_color, "red")
  expect_identical(a$query_one("#b")$computed_style()$border_color, "bright_black")
  pilot$press("tab")
  expect_identical(a$query_one("#a")$computed_style()$border_color, "bright_black")
  expect_identical(a$query_one("#b")$computed_style()$border_color, "red")
  pilot$hover("#go")
  expect_identical(a$query_one("#go")$computed_style()$background, "green")
  # The screen shows it.
  expect_identical(pilot$driver$terminal$screen$get_cell(3, 8)$bg, "green")
  expect_true(a$query_one("#go")$matches("Button:hover"))
  expect_length(a$query("Input:focus"), 1)
})

test_that("descendant selectors and state styles of ancestors", {
  a <- app(
    vertical(horizontal(label("in row", id = "in_row")), label("outside", id = "outside")),
    stylesheet = "Horizontal Label { italic: true; } Vertical > Label { underline: true; }"
  )
  pilot <- test_app(a, 20, 4)
  st_in <- a$query_one("#in_row")$computed_style()
  st_out <- a$query_one("#outside")$computed_style()
  expect_true(attrs_decode(st_in$attrs)[["italic"]])
  expect_false(attrs_decode(st_in$attrs)[["underline"]])
  expect_true(attrs_decode(st_out$attrs)[["underline"]])
})

test_that("stylesheet files and grid properties", {
  path <- tempfile(fileext = ".rtcss")
  writeLines(c("Grid { grid-columns: 1fr 2fr; grid-gap: 0 1; }", "Label { column-span: 2; }"), path)
  sheet <- stylesheet_file(path)
  rules <- unclass(sheet)
  expect_identical(vapply(rules[[1]]$style$props$grid_columns, format, ""), c("1fr", "2fr"))
  expect_identical(rules[[2]]$style$props$column_span, 2L)
  expect_error(stylesheet_file(file.path(tempdir(), "missing.rtcss")), "does not exist")
  a <- app(grid_layout(label("a"), label("b")), stylesheet = path)
  expect_length(a$stylesheets, 1)
})

test_that("themes resolve colour tokens and restyle running apps", {
  th <- termr_theme("dark", accent = "orange")
  expect_identical(th$colors$accent, "#ffa500")
  expect_error(termr_theme("neon"), "Unknown theme")
  expect_error(termr_theme(primary = "$accent"), "cannot refer")
  b <- button("Go", id = "go", variant = "primary")
  a <- app(b)
  pilot <- test_app(a, 20, 4)
  expect_identical(b$computed_style()$background, "blue")
  a$theme <- "dark"
  pilot$step()
  expect_identical(b$computed_style()$background, "#0178d4")
  # The screen background follows the theme.
  expect_identical(pilot$driver$terminal$screen$get_cell(20, 4)$bg, "#121212")
  a$theme <- termr_theme("light", primary = "#ff0000")
  pilot$step()
  expect_identical(pilot$driver$terminal$screen$get_cell(3, 2)$bg, "#ff0000")
  expect_identical(normalize_color("$primary"), "$primary")
  expect_error(resolve_style(style(foreground = "$nope")), "Unknown theme colour")
})

test_that("stylesheets can use theme tokens", {
  a <- app(label("x", id = "x"), stylesheet = "Label { foreground: $warning; }", theme = "light")
  pilot <- test_app(a, 10, 2)
  expect_identical(a$query_one("#x")$computed_style()$foreground, "#b26a00")
})

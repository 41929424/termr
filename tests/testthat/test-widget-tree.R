test_that("constructors build a widget tree", {
  title <- label("Title", id = "title")
  ui <- vertical(title, horizontal(label("a"), label("b")), id = "root")
  expect_identical(ui$type, "Vertical")
  expect_length(ui$children, 2)
  expect_identical(title$parent, ui)
  expect_length(ui$walk(), 5)
  expect_identical(ui$children[[2]]$children[[1]]$ancestors()[[2]], ui)
})

test_that("mount moves widgets and rejects cycles", {
  a <- vertical(id = "a")
  b <- vertical(id = "b")
  x <- label("x")
  a$mount(x)
  b$mount(x)
  expect_length(a$children, 0)
  expect_identical(x$parent, b)
  a$mount(b)
  expect_error(b$mount(a), "inside itself")
  expect_error(a$mount("text"), "Children must be widgets")
})

test_that("remove() and remove_children() detach widgets", {
  x <- label("x")
  y <- label("y")
  root <- vertical(x, y)
  x$remove()
  expect_null(x$parent)
  expect_length(root$children, 1)
  root$remove_children()
  expect_length(root$children, 0)
})

test_that("ids and classes are validated", {
  expect_error(label("x", id = "1bad"), "Invalid widget id")
  w <- label("x", classes = "a b")
  expect_identical(w$classes, c("a", "b"))
  w$add_class("c")
  w$remove_class("a")
  expect_identical(w$classes, c("b", "c"))
  w$toggle_class("b")
  expect_false(w$has_class("b"))
  expect_error(label("x", classes = "ok 9no"), "Invalid class")
})

test_that("read-only fields cannot be assigned", {
  w <- label("x")
  expect_error(w$parent <- vertical(), "read-only")
  expect_error(w$children <- list(), "read-only")
})

test_that("set_state invalidates only on change", {
  w <- label("x")
  w$render_lines()
  expect_false(widget_private(w)$.dirty)
  w$text <- "x"
  expect_false(widget_private(w)$.dirty)
  w$text <- "y"
  expect_true(widget_private(w)$.dirty)
  expect_identical(as.character(as_text(w$render())), "y")
})

test_that("printing shows the tree", {
  ui <- vertical(label("Hello", id = "greeting"), horizontal(label("a", classes = "x")))
  expect_snapshot(print(ui))
})

test_that("labels render plain and styled text", {
  ui <- vertical(
    label("termr demo", style = style(bold = TRUE)),
    label(c(span("status: "), span("ok", style(foreground = "green"))))
  )
  buf <- render_widget(ui, 20, 3)
  expect_identical(buf$to_text(), c("termr demo          ", "status: ok          ", "                    "))
  expect_true(attrs_decode(buf$get_cell(1, 1)$attrs)[["bold"]])
  expect_identical(buf$get_cell(9, 2)$fg, "green")
  expect_null(ui$parent)
})

test_that("render_widget snapshot of a nested layout", {
  ui <- vertical(
    label("termr", style = style(bold = TRUE, align = "center", width = "1fr")),
    horizontal(
      vertical(label("left pane"), style = style(border = "round", height = 5)),
      vertical(label("right pane", style = style(align = "right", width = "1fr")),
               style = style(border = "single", height = 5, padding = c(0, 1)))
    ),
    label("footer", style = style(margin = c(0, 2)))
  )
  expect_snapshot(print(render_widget(ui, 40, 8)))
})

test_that("borders use the colour and background of the widget", {
  ui <- vertical(style = style(border = "double", border_color = "cyan", background = "blue"))
  buf <- render_widget(ui, 6, 3)
  expect_identical(buf$to_text(), c("\u2554\u2550\u2550\u2550\u2550\u2557", "\u2551    \u2551", "\u255a\u2550\u2550\u2550\u2550\u255d"))
  expect_identical(buf$get_cell(1, 1)$fg, "cyan")
  expect_identical(buf$get_cell(3, 2)$bg, "blue")
})

test_that("content is clipped to the widget region", {
  ui <- vertical(label("a very long line of text", style = style(width = 6)))
  buf <- render_widget(ui, 10, 1)
  expect_identical(buf$to_text(), "a very    ")
})

test_that("set() assigns fields of widgets returned by function calls", {
  ui <- vertical(label("a", id = "l"), input(id = "i"))
  ui$query_one("#l")$set(text = "b")
  ui$query_one("#i")$set(value = "typed", disabled = TRUE)
  expect_identical(ui$query_one("#l")$text, "b")
  expect_identical(ui$query_one("#i")$value, "typed")
  expect_true(ui$query_one("#i")$disabled)
  expect_error(ui$query_one("#l")$set(nope = 1), "has no field")
  expect_error(ui$query_one("#l")$set(update = 1), "is a method")
  expect_error(ui$query_one("#l")$set("x"), "must be named")
  ui$query_one("#i")$clear()
  expect_identical(ui$query_one("#i")$value, "")
})

test_that("mount inserts at positions; replace and clear swap children", {
  a <- label("a", id = "a")
  b <- label("b", id = "b")
  root <- vertical(a, b)
  root$mount(label("first", id = "first"), before = a)
  root$mount(label("mid", id = "mid"), after = 2)
  ids <- function() vapply(root$children, function(w) w$id, "")
  expect_identical(ids(), c("first", "a", "mid", "b"))
  root$mount(b, before = 1)
  expect_identical(ids(), c("b", "first", "a", "mid"))
  root$replace(label("x", id = "x"))
  expect_identical(ids(), "x")
  root$clear()
  expect_length(root$children, 0)
  expect_error(root$mount(label("y"), before = a), "must be a child")
  expect_error(root$mount(label("y"), before = 1, after = 1), "either")
})

test_that("duplicate ids are rejected within a tree", {
  root <- vertical(label("a", id = "same"))
  expect_error(root$mount(label("b", id = "same")), "Duplicate widget id \"same\"")
  expect_error(vertical(label("a", id = "x"), label("b", id = "x")), "Duplicate")
  # Moving a widget within the tree is fine.
  inner <- vertical(id = "inner")
  root$mount(inner)
  inner$mount(root$query_one("#same"))
  expect_identical(root$query_one("#same")$parent, inner)
  # Different screens may reuse ids.
  a <- app(label("x", id = "dup"))
  pilot <- test_app(a, 10, 2)
  a$push_screen(vertical(label("y", id = "dup")))
  pilot$step()
  expect_identical(a$query_one("#dup")$text, "y")
})

test_that("read-only fields explain themselves", {
  a <- app(label("x"))
  expect_error(a$screen <- vertical(), "`screen` is read-only")
  t <- tabs(tab("A"))
  expect_error(t$active_tab <- NULL, "`active_tab` is read-only")
})

test_that("removing the focused widget repairs focus and cancels its timers", {
  ticks <- 0L
  b1 <- button("one", id = "one")
  b2 <- button("two", id = "two")
  a <- app(vertical(b1, b2))
  pilot <- test_app(a, 20, 8)
  b1$set_interval(1, function(self, app) ticks <<- ticks + 1L)
  expect_identical(a$focused$id, "one")
  b1$remove()
  pilot$advance(2)
  expect_identical(a$focused$id, "two")
  expect_identical(ticks, 0L)
  expect_length(a$query("#one"), 0)
})

test_that("parent and child pointers stay consistent under random edits", {
  set.seed(11)
  root <- vertical()
  pool <- lapply(1:15, function(i) if (i %% 3 == 0) vertical() else label(paste(i)))
  containers <- c(list(root), Filter(function(w) inherits(w, "Vertical"), pool))
  for (step in 1:200) {
    w <- sample(pool, 1)[[1]]
    target <- sample(containers, 1)[[1]]
    op <- sample(c("mount", "remove", "before"), 1)
    try(switch(op,
      mount = target$mount(w),
      remove = w$remove(),
      before = if (length(target$children)) target$mount(w, before = 1)
    ), silent = TRUE)
    for (node in c(list(root), pool)) {
      for (child in node$children) expect_identical(child$parent, node)
      if (!is.null(node$parent)) {
        expect_true(any(vapply(node$parent$children, identical, logical(1), node)))
      }
    }
  }
})

test_that("moving a widget keeps its reactive bindings; removing it ends them", {
  s <- signal("x")
  l <- label("")
  l$bind_reactive("text", function() s())
  a <- vertical(l)
  b <- vertical()
  pilot <- test_app(app(a, b))
  on.exit(pilot$stop(), add = TRUE)
  ticks <- 0L
  l$set_interval(1, function(w, app) ticks <<- ticks + 1L)
  b$mount(l)
  s("moved")
  expect_identical(l$text, "moved")
  pilot$advance(1.5)
  expect_gte(ticks, 1L)
  l$remove()
  s("removed")
  expect_identical(l$text, "moved")
})

test_that("widget$on() accepts the selector before or after the handler", {
  seen <- character()
  box <- vertical(button("A", id = "a"), button("B", id = "b"))
  box$on("button.pressed", function(event, app) seen <<- c(seen, "handler-first"), selector = "#a")
  box$on("button.pressed", "#a", function(event, app) seen <<- c(seen, "selector-first"))
  pilot <- test_app(app(box))
  on.exit(pilot$stop(), add = TRUE)
  pilot$click("#b")
  expect_length(seen, 0L)
  pilot$click("#a")
  expect_setequal(seen, c("handler-first", "selector-first"))
})

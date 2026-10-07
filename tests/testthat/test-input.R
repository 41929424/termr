input_pilot <- function(..., width = 20) {
  inp <- input(id = "in", ...)
  pilot <- test_app(app(inp), width, 3)
  list(pilot = pilot, input = inp)
}

# Content row of the input (inside the border and padding).
content_row <- function(pilot) {
  row <- pilot$screen_text()[[2]]
  substring(row, 3L, nchar(row) - 2L)
}

test_that("typing inserts characters at the cursor", {
  x <- input_pilot()
  x$pilot$type("Hllo")
  x$pilot$press("left", "left", "left")
  x$pilot$type("e")
  expect_identical(x$input$value, "Hello")
  expect_identical(x$input$cursor_position, 2L)
})

test_that("backspace, delete, home and end edit the text", {
  x <- input_pilot(value = "abcdef")
  expect_identical(x$input$cursor_position, 6L)
  x$pilot$press("backspace")
  expect_identical(x$input$value, "abcde")
  x$pilot$press("home", "delete")
  expect_identical(x$input$value, "bcde")
  x$pilot$press("end", "left", "ctrl+u")
  expect_identical(x$input$value, "e")
  x$pilot$press("home", "ctrl+k")
  expect_identical(x$input$value, "")
  x$pilot$press("backspace", "delete", "left")
  expect_identical(x$input$cursor_position, 0L)
})

test_that("printable keys are consumed before app bindings", {
  quit <- FALSE
  inp <- input()
  pilot <- test_app(app(inp, bind("q", function(app) quit <<- TRUE)), 20, 3)
  pilot$type("q")
  expect_identical(inp$value, "q")
  expect_false(quit)
  pilot$press("tab")
  expect_true(inp$focused) # the only focusable widget keeps focus
})

test_that("input sends changed and submitted messages", {
  log <- character()
  a <- app(
    input(id = "name"),
    on("input.changed", "#name", function(event, app) log <<- c(log, paste("changed", event$data$value))),
    on("input.submitted", "#name", function(event, app) log <<- c(log, paste("submitted", event$data$value)))
  )
  pilot <- test_app(a, 20, 3)
  pilot$type("ab")
  pilot$press("enter")
  name <- a$query_one("#name")
  name$value <- "xyz"
  pilot$step()
  expect_identical(log, c("changed a", "changed ab", "submitted ab", "changed xyz"))
})

test_that("the cursor is drawn and the text scrolls horizontally", {
  x <- input_pilot(width = 10) # content width 6
  x$pilot$type("abcdefghij")
  expect_identical(content_row(x$pilot), "fghij ")
  cursor_cell <- x$pilot$driver$terminal$screen$get_cell(8, 2)
  expect_true(attrs_decode(cursor_cell$attrs)[["reverse"]])
  x$pilot$press("home")
  expect_identical(content_row(x$pilot), "abcdef")
  x$pilot$press("end")
  for (i in 1:8) x$pilot$press("backspace")
  expect_identical(content_row(x$pilot), "ab    ")
})

test_that("placeholder, password and max_length", {
  x <- input_pilot(placeholder = "Your name")
  expect_identical(content_row(x$pilot), "Your name       ")
  first <- x$pilot$driver$terminal$screen$get_cell(3, 2)
  expect_true(attrs_decode(first$attrs)[["reverse"]])
  expect_identical(x$pilot$driver$terminal$screen$get_cell(4, 2)$fg, "bright_black")
  pw <- input_pilot(password = TRUE)
  pw$pilot$type("secret")
  expect_identical(pw$input$value, "secret")
  expect_match(content_row(pw$pilot), "\u2022\u2022\u2022\u2022\u2022\u2022", fixed = TRUE)
  short <- input_pilot(max_length = 3)
  short$pilot$type("abcdef")
  expect_identical(short$input$value, "abc")
})

test_that("wide characters are edited as single characters", {
  x <- input_pilot()
  x$pilot$type("\u4e2d\u6587x")
  x$pilot$press("left", "backspace")
  expect_identical(x$input$value, "\u4e2dx")
})

test_that("values are sanitised", {
  inp <- input(value = "a\nb\033c")
  expect_identical(inp$value, "a bc")
  expect_error(input(value = list(1)), "string")
})

test_that("disabled inputs ignore typing", {
  inp <- input(disabled = TRUE)
  pilot <- test_app(app(inp), 20, 3)
  inp$on_key(key_event("x"))
  expect_identical(inp$value, "")
})

test_that("form snapshot", {
  a <- app(vertical(
    label("termr demo", style = style(bold = TRUE)),
    input(id = "name", placeholder = "Your name"),
    button("Say hello", id = "submit"),
    label("", id = "result")
  ), on("button.pressed", "#submit", function(event, app) {
    app$query_one("#result")$update(paste("Hello,", app$query_one("#name")$value))
  }))
  pilot <- test_app(a, 30, 9)
  pilot$type("Ada")
  pilot$press("tab", "enter")
  expect_snapshot(pilot$snapshot())
})

test_that("shift and word movement select text", {
  x <- input_pilot(value = "hello brave world", width = 40)
  x$pilot$press("shift+left", "shift+left")
  expect_identical(x$input$selection, "ld")
  x$pilot$press("ctrl+shift+left")
  expect_identical(x$input$selection, "world")
  x$pilot$press("left")
  expect_identical(x$input$cursor_position, 12L)
  expect_identical(x$input$selection, "")
  x$pilot$press("ctrl+left")
  expect_identical(x$input$cursor_position, 6L)
  x$pilot$press("ctrl+right", "ctrl+right")
  expect_identical(x$input$cursor_position, 17L)
  x$pilot$press("shift+home")
  expect_identical(x$input$selection, "hello brave world")
  x$pilot$press("ctrl+a")
  x$pilot$type("new")
  expect_identical(x$input$value, "new")
})

test_that("word deletion and selection deletion", {
  x <- input_pilot(value = "one two three", width = 40)
  x$pilot$press("ctrl+backspace")
  expect_identical(x$input$value, "one two ")
  x$pilot$press("ctrl+w")
  expect_identical(x$input$value, "one ")
  x$pilot$press("home", "ctrl+delete")
  expect_identical(x$input$value, " ")
  x$input$value <- "abcdef"
  x$input$select_range(1, 4)
  x$pilot$press("backspace")
  expect_identical(x$input$value, "aef")
  expect_identical(x$input$cursor_position, 1L)
})

test_that("copy, cut and paste use the app clipboard", {
  inp <- input(value = "copy me", id = "in")
  a <- app(inp)
  pilot <- test_app(a, 30, 3)
  inp$select_range(0, 4)
  pilot$press("ctrl+c")
  expect_false(pilot$exited)
  expect_identical(a$clipboard, "copy")
  pilot$press("end", "ctrl+v")
  expect_identical(inp$value, "copy mecopy")
  inp$select_range(0, 5)
  pilot$press("ctrl+x")
  expect_identical(inp$value, "mecopy")
  expect_identical(a$clipboard, "copy ")
  # Without a selection Ctrl+C keeps its app meaning (quit).
  pilot$press("ctrl+c")
  expect_true(pilot$exited)
})

test_that("validation sets the invalid state and sends messages", {
  got <- character()
  inp <- input(id = "age", validate = function(x) if (!grepl("^[0-9]+$", x)) "Enter a number")
  a <- app(inp, on("*", "#age", function(event, app) {
    if (event$type %in% c("input.valid", "input.invalid")) got <<- c(got, event$type)
  }))
  pilot <- test_app(a, 20, 3)
  expect_false(inp$valid)
  expect_identical(inp$error, "Enter a number")
  expect_true("invalid" %in% inp$pseudo_states())
  expect_identical(pilot$driver$terminal$screen$get_cell(1, 1)$fg, "bright_red")
  pilot$type("4")
  expect_true(inp$valid)
  pilot$type("x")
  expect_identical(got, c("input.valid", "input.invalid"))
  pilot$press("backspace", "enter")
  expect_true(inp$valid)
  expect_error(input(validate = "no"), "function")
})

test_that("the selection is highlighted and the mouse selects", {
  x <- input_pilot(value = "abcdef", width = 20)
  x$input$select_range(1, 3)
  x$pilot$step()
  screen <- x$pilot$driver$terminal$screen
  expect_identical(screen$get_cell(4, 2)$bg, "blue")  # "b"
  expect_identical(screen$get_cell(3, 2)$bg, "")      # "a"
  x$pilot$mouse("down", 3, 2, button = "left")
  x$pilot$mouse("move", 6, 2, button = "left")
  expect_identical(x$input$selection, "abc")
  x$pilot$mouse("down", 8, 2, button = "left", shift = TRUE)
  expect_identical(x$input$selection, "abcde")
})

test_that("password values stay out of print, inspection, event logs and clipboards", {
  pw <- input("hunter2", id = "pw", password = TRUE)
  expect_false(grepl("hunter2", pw$format(), fixed = TRUE))
  expect_false(any(grepl("hunter2", capture.output(print(inspect_widget(pw))), fixed = TRUE)))
  pilot <- test_app(app(pw))
  on.exit(pilot$stop(), add = TRUE)
  pilot$app$log_events()
  pw$focus()
  pw$select_range(0, 7)
  pilot$press("ctrl+c")
  expect_false(grepl("hunter2", pilot$app$clipboard, fixed = TRUE))
  expect_false(any(grepl("hunter2", unlist(pilot$app$event_log()), fixed = TRUE)))
  expect_identical(pw$value, "hunter2")
})

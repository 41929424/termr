main_app <- function() {
  app(vertical(input(id = "name"), button("Open", id = "open"), label("main screen", id = "main_label")))
}

test_that("push_screen shows a new screen with its own focus chain", {
  a <- main_app()
  pilot <- test_app(a, 30, 8)
  pilot$press("tab") # focus the button
  second <- vertical(label("second", id = "second_label"), button("A", id = "a"), button("B", id = "b"))
  a$push_screen(second)
  pilot$step()
  expect_identical(pilot$app$focused$id, "a")
  expect_identical(vapply(a$focus_chain(), function(w) w$id, ""), c("a", "b"))
  expect_match(pilot$screen_text()[[1]], "^second")
  expect_false(any(grepl("main screen", pilot$screen_text())))
  pilot$press("tab", "tab")
  expect_identical(a$focused$id, "a")
  a$pop_screen()
  pilot$step()
  expect_identical(a$focused$id, "open")
  expect_true(any(grepl("main screen", pilot$screen_text())))
  expect_error(a$pop_screen(), "last screen")
})

test_that("screen lifecycle events", {
  log <- character()
  a <- main_app()
  a$on("*", function(event, app) {
    if (event$type %in% c("screen.show", "screen.hide") || (event$type %in% c("mount", "unmount") && inherits(event$sender, "Screen"))) {
      log <<- c(log, paste(event$type, class(event$sender)[[1]]))
    }
  })
  pilot <- test_app(a, 30, 8)
  log <- character()
  a$push_screen(modal(label("hi")))
  pilot$step()
  a$pop_screen()
  pilot$step()
  expect_identical(log, c(
    "mount ModalScreen", "screen.hide Screen", "screen.show ModalScreen",
    "unmount ModalScreen", "screen.hide ModalScreen", "screen.show Screen"
  ))
})

test_that("modal dialogs dim the screen below and capture input", {
  a <- main_app()
  pilot <- test_app(a, 40, 10)
  dlg <- modal(label("Are you sure?"), title = "Question", width = 20)
  a$push_screen(dlg)
  pilot$step()
  text <- pilot$screen_text()
  dialog_rows <- grep("Question", text)
  expect_length(dialog_rows, 1)
  # The 20-column dialog is centred in 40 columns.
  expect_identical(as.integer(regexpr("\u256d Question \u2500", text[[dialog_rows]])), 11L)
  # Underlying text is still shown, dimmed.
  screen <- pilot$driver$terminal$screen
  expect_identical(screen$get_cell(3, 2)$fg, "bright_black")
  # Keys go to the dialog: typing does not reach the input below.
  pilot$type("abc")
  expect_identical(a$query_one("#name")$value, "")
  # Clicking the dimmed screen does nothing.
  pilot$click(3, 2)
  expect_identical(a$screen, dlg)
  pilot$press("escape")
  expect_false(identical(a$screen, dlg))
  expect_identical(a$focused$id, "name")
})

test_that("confirm_dialog calls on_confirm or on_cancel", {
  result <- NULL
  a <- main_app()
  pilot <- test_app(a, 50, 12)
  a$push_screen(confirm_dialog("Delete file?", on_confirm = function(app) result <<- "yes",
                               on_cancel = function(app) result <<- "no"))
  pilot$step()
  expect_identical(a$focused$id, "cancel")
  pilot$press("tab", "enter")
  expect_identical(result, "yes")
  a$push_screen(confirm_dialog("Again?", on_confirm = function(app) result <<- "yes2",
                               on_cancel = function(app) result <<- "no2"))
  pilot$step()
  pilot$press("escape")
  expect_identical(result, "no2")
  a$push_screen(alert_dialog("Done"))
  pilot$step()
  pilot$click("#ok")
  expect_length(a$screens, 1)
})

test_that("push_screen callbacks receive the result", {
  got <- NULL
  a <- main_app()
  pilot <- test_app(a, 40, 10)
  dlg <- modal(button("Pick", id = "pick"))
  dlg$on("button.pressed", function(event, app) dlg$dismiss("picked"))
  a$push_screen(dlg, callback = function(result, app) got <<- result)
  pilot$step()
  pilot$press("enter")
  expect_identical(got, "picked")
})

test_that("switch_screen replaces the top screen and query searches all screens", {
  a <- main_app()
  pilot <- test_app(a, 30, 6)
  a$switch_screen(vertical(label("other", id = "other")))
  pilot$step()
  expect_length(a$screens, 1)
  expect_match(pilot$screen_text()[[1]], "^other")
  a$push_screen(modal(label("x", id = "in_modal")))
  pilot$step()
  expect_identical(a$query_one("#other")$text, "other")
  expect_length(a$query("Label"), 2)
  expect_error(a$query_one("#missing"), "No widget matches")
})

test_that("timers of a popped screen are cancelled", {
  ticks <- 0L
  a <- main_app()
  pilot <- test_app(a, 30, 6)
  s <- vertical(label("timer"))
  a$push_screen(s)
  pilot$step()
  s$set_interval(1, function(self, app) ticks <<- ticks + 1L)
  pilot$advance(2)
  a$pop_screen()
  pilot$advance(3)
  expect_identical(ticks, 2L)
})

test_that("notifications appear, stack and expire", {
  a <- main_app()
  pilot <- test_app(a, 40, 12)
  a$notify("Saved", title = "File", severity = "success", timeout = 2)
  pilot$step()
  text <- pilot$screen_text()
  row <- grep("Saved", text)
  expect_length(row, 1)
  expect_match(text[[row]], "Saved +\u2502 $")
  expect_match(text[[row - 1L]], "\u256d File")
  border_x <- regexpr("\u2502 Saved", text[[row]])
  expect_identical(pilot$driver$terminal$screen$get_cell(border_x, row)$fg, "bright_green")
  expect_length(a$notifications(), 1)
  pilot$advance(2.5)
  expect_length(a$notifications(), 0)
  expect_false(any(grepl("Saved", pilot$screen_text())))
  for (i in 1:7) a$notify(paste("n", i), timeout = Inf)
  expect_length(a$notifications(), 5)
  expect_error(a$notify("x", severity = "fatal"), "severity")
})

test_that("clicking a notification closes it and keys are unaffected", {
  a <- main_app()
  pilot <- test_app(a, 40, 12)
  toast <- a$notify("Hello there", timeout = Inf)
  pilot$step()
  pilot$type("x")
  expect_identical(a$query_one("#name")$value, "x")
  pilot$click(toast)
  expect_length(a$notifications(), 0)
})

test_that("panels draw a title in their border", {
  buf <- render_widget(panel(label("CPU 42%"), title = "Metrics"), 20, 3)
  expect_identical(buf$to_text()[[1]], "\u256d Metrics \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u256e")
  expect_match(buf$to_text()[[2]], "^\u2502 CPU 42%")
  p <- panel(title = "A")
  p$border_title <- "B"
  expect_identical(p$border_title, "B")
})

test_that("dismiss results are evaluated before the screen goes away", {
  got <- NULL
  a <- app(label("main"))
  pilot <- test_app(a, 60, 15)
  dlg <- modal(input(id = "new_name"), button("OK", id = "ok"), title = "Rename")
  dlg$on("button.pressed", function(event, app) dlg$dismiss(app$query_one("#new_name")$value))
  a$push_screen(dlg, callback = function(result, app) got <<- result)
  pilot$type("new.csv")
  pilot$press("tab", "enter")
  expect_identical(got, "new.csv")
})

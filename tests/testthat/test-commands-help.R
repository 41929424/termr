test_that("command() validates and prints", {
  cmd <- command("Save", "save", category = "File", shortcut = "ctrl+s")
  expect_s3_class(cmd, "termr_command")
  expect_identical(command_text(cmd), "File: Save  (ctrl+s)")
  expect_output(print(cmd), "File: Save")
  expect_error(command("x", 1), "action")
  expect_error(command(c("a", "b"), "x"), "label")
})

test_that("add_command accepts command objects and the old label/action form", {
  saved <- 0
  a <- app(label("x"), actions = list(save = function(app) saved <<- saved + 1))
  a$add_command("Open", function(app) NULL)
  a$add_command(command("Save", "save", category = "File", shortcut = "ctrl+s"))
  pilot <- test_app(a, 60, 14)
  pilot$press("ctrl+p")
  text <- paste(pilot$screen_text(), collapse = "\n")
  expect_match(text, "File: Save  \\(ctrl\\+s\\)")
  expect_match(text, "Open")
  pilot$type("save")
  pilot$press("enter")
  expect_identical(saved, 1)
})

test_that("disabled commands are marked and do not run", {
  ran <- FALSE
  enabled <- FALSE
  a <- app(label("x"))
  a$add_command(command("Deploy", function(app) ran <<- TRUE, enabled = function(app) enabled))
  pilot <- test_app(a, 60, 14)
  pilot$press("ctrl+p")
  pilot$type("deploy")
  expect_true(any(grepl("Deploy  \\(unavailable\\)", pilot$screen_text())))
  pilot$press("enter")
  expect_false(ran)
  expect_s3_class(a$screen, "CommandPalette")
  expect_length(a$notifications(), 1)
  pilot$press("escape")
  enabled <- TRUE
  pilot$press("ctrl+p")
  pilot$type("deploy")
  pilot$press("enter")
  expect_true(ran)
})

test_that("palette and help are built from the same commands", {
  a <- app(input(id = "i"), bind("ctrl+s", "save", "Save file"))
  a$add_command(command("Export", function(app) NULL, category = "Data", shortcut = "ctrl+e",
                        description = "Write a CSV"))
  pilot <- test_app(a, 60, 16)
  commands <- collect_commands(a)
  labels <- vapply(commands, function(cmd) cmd$label, "")
  expect_true(all(c("Export", "Save file") %in% labels))
  md <- help_markdown(a)
  expect_match(md, "Save file", fixed = TRUE)
  expect_match(md, "**Export** (ctrl+e) - Write a CSV", fixed = TRUE)
  expect_match(md, "### Data", fixed = TRUE)
})

test_that("F1 shows the help screen with bindings of the focused widget", {
  a <- app(vertical(text_area("x", id = "ed"), button("B", id = "b")))
  a$query_one("#ed")$set(help = "Edit **notes** here.")
  pilot <- test_app(a, 90, 30)
  md <- help_markdown(a)
  pilot$press("f1")
  expect_s3_class(a$screen, "HelpScreen")
  text <- paste(pilot$screen_text(), collapse = "\n")
  expect_match(text, "Help")
  expect_match(text, "TextArea #ed")
  expect_match(text, "Edit notes here.")
  expect_match(text, "cursor left")
  expect_match(md, "Undo", fixed = TRUE)
  expect_match(md, "Command palette", fixed = TRUE)
  # F1 again does not stack screens; Escape closes.
  pilot$press("f1")
  expect_length(a$screens, 2)
  pilot$press("escape")
  expect_length(a$screens, 1)
  a$show_help()
  pilot$step()
  expect_s3_class(a$screen, "HelpScreen")
})

test_that("help works for an app without focusable widgets", {
  a <- app(label("hello"))
  pilot <- test_app(a, 60, 20)
  pilot$press("f1")
  expect_match(paste(pilot$screen_text(), collapse = "\n"), "Application")
})

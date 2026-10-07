sql_tokens <- function(text) {
  fn <- sql_highlighter()
  spans <- fn(strsplit(text, "\n", fixed = TRUE)[[1L]])
  lapply(seq_along(spans), function(i) {
    line <- strsplit(text, "\n", fixed = TRUE)[[1L]][[i]]
    row <- spans[[i]]
    if (!nrow(row)) return(character())
    vapply(seq_len(nrow(row)), function(j) {
      paste0(substr(line, row$start[[j]], row$end[[j]]), "=", row$token[[j]])
    }, "")
  })
}

test_that("SQL highlighter handles common tokens and mixed casing", {
  tokens <- unlist(sql_tokens("SELECT name FROM users WHERE active = TRUE AND n >= 3.14"))
  expect_true("SELECT=keyword" %in% tokens)
  expect_true("name=identifier" %in% tokens)
  expect_true("TRUE=constant" %in% tokens)
  expect_true("3.14=number" %in% tokens)
  expect_true("==operator" %in% tokens)
  lower <- unlist(sql_tokens("select * from t"))
  mixed <- unlist(sql_tokens("SeLeCt * FrOm t"))
  expect_true("select=keyword" %in% lower)
  expect_true("SeLeCt=keyword" %in% mixed)
})

test_that("SQL highlighter handles strings, comments, parameters and identifiers", {
  tokens <- unlist(sql_tokens("SELECT 'it''s fine', \"col\", `other`, [last] -- note\n/* block */ :name @p ? $2"))
  expect_true("'it''s fine'=string" %in% tokens)
  expect_true("\"col\"=quoted_identifier" %in% tokens)
  expect_true("`other`=quoted_identifier" %in% tokens)
  expect_true("[last]=quoted_identifier" %in% tokens)
  expect_true("-- note=comment" %in% tokens)
  expect_true("/* block */=comment" %in% tokens)
  expect_true(":name=parameter" %in% tokens)
  expect_true("@p=parameter" %in% tokens)
  expect_true("?=parameter" %in% tokens)
  expect_true("$2=parameter" %in% tokens)
})

test_that("SQL highlighter is tolerant of incomplete SQL and carries block comments", {
  spans <- sql_highlighter()(c("SELECT 'unfinished", "/* comment", "still comment */ FROM t"))
  expect_true("string" %in% spans[[1]]$token)
  expect_true("comment" %in% spans[[2]]$token)
  expect_true("comment" %in% spans[[3]]$token)
  expect_true("keyword" %in% spans[[3]]$token)
  expect_no_error(sql_highlighter()(c("SELECT (", "WHERE x = .5e-2;")))
})

test_that("SQL highlighter supports Unicode and viewport-first multiline calls", {
  fn <- sql_highlighter(dialect = "sqlite")
  lines <- c("SELECT имя FROM таблица", "/* open", "close */ SELECT 1e10")
  full <- fn(lines)
  expect_true(any(full[[1]]$token == "identifier"))
  row <- fn(lines, state = list(line = 3L, version = 1L))
  expect_true(any(row$token == "keyword"))
  expect_true(any(row$token == "comment"))
  expect_identical(attr(fn, "dialect"), "sqlite")
})

test_that("contextual SQL highlighting resumes from the first edited line", {
  fn <- sql_highlighter()
  first <- c("SELECT 1", "/* comment", "still comment */ FROM t")
  expect_true("comment" %in% fn(first, state = list(line = 3L, version = 1L, changed_from = 1L))$token)
  edited <- c("SELECT 1", "ordinary text", "still comment */ FROM t")
  spans <- fn(edited, state = list(line = 3L, version = 2L, changed_from = 2L))
  expect_false("comment" %in% spans$token)
  expect_true("keyword" %in% spans$token)
})

test_that("SQL editor inherits TextArea behavior and offers a safe no-connection state", {
  ed <- sql_editor("SELECT * FROM mtcars", id = "query")
  expect_s3_class(ed, "TextArea")
  expect_true(ed$line_numbers)
  expect_identical(ed$tab_behavior, "indent")
  expect_no_error(render_widget(ed, 60, 8))
  result <- ed$execute()
  expect_false(result$ok)
  expect_match(result$message, "database connection")
  expect_true(any(vapply(ed$bindings(), function(b) "ctrl+enter" %in% b$keys, logical(1))) == FALSE)
})

test_that("DBI wrapper and SQL events work with SQLite when installed", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "items", data.frame(id = 1:3, name = c("a", "b", "c")))
  external <- db_connection(con, name = "test")
  expect_true(external$is_valid())
  expect_identical(nrow(external$query("SELECT * FROM items LIMIT 2")), 2L)
  expect_identical(external$execute("CREATE TABLE scratch (id INTEGER)"), 0L)
  expect_identical(external$execute("INSERT INTO scratch VALUES (1)"), 1L)
  expect_error(external$query("SELECT * FROM missing"), "no such table")
  expect_false(external$disconnect())
  expect_true(DBI::dbIsValid(con))

  events <- list()
  editor <- sql_editor("SELECT * FROM items LIMIT 2", id = "query", connection = external)
  application <- app(
    editor,
    on("sql.query_started", "#query", function(event, app) events[[length(events) + 1L]] <<- event$type),
    on("sql.query_completed", "#query", function(event, app) events[[length(events) + 1L]] <<- event$data)
  )
  pilot <- test_app(application, 50, 10)
  pilot$press("ctrl+enter")
  expect_identical(events[[1]], "sql.query_started")
  expect_equal(events[[2]]$rows, 2L)
  expect_s3_class(events[[2]]$result, "data.frame")
  editor$set_text("SELECT * FROM missing")
  editor$execute()
  pilot$step()
  expect_identical(events[[3]], "sql.query_failed")
  expect_match(events[[4]]$message, "no such table")

  owned_con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  owned <- db_connection(owned_con, owned = TRUE)
  owned_editor <- sql_editor(connection = owned)
  owned_app <- app(owned_editor, bind("q", function(app) app$exit()))
  run(owned_app, driver = HeadlessDriver$new(30, 5))
  expect_false(owned$is_valid())
})

test_that("database support reports optional and invalid connection states", {
  if (!requireNamespace("DBI", quietly = TRUE)) {
    expect_error(db_connection(structure(list(), class = "fake_connection")),
                 "Database support requires the DBI package")
    skip("DBI is not installed")
  }
  invalid <- getFromNamespace("TermrDBConnection", "termr")$new(con = "not a connection")
  expect_false(invalid$is_valid())
  expect_error(invalid$query("SELECT 1"), "connection is not valid")
})

test_that("owned SQL connections close when the app shuts down", {
  closed <- FALSE
  mock <- structure(list(owned = TRUE, disconnect = function() closed <<- TRUE),
                    class = "TermrDBConnection")
  editor <- sql_editor(connection = mock)
  pilot <- test_app(app(editor), width = 30, height = 5)
  pilot$stop()
  expect_true(closed)
})

test_that("sql-workspace is registered as an example", {
  expect_true("sql-workspace" %in% run_example())
  expect_s3_class(load_example("sql-workspace"), "App")
})

test_that("a widget removed before app start cannot leave a live timer", {
  fired <- FALSE
  widget <- label("temporary")
  application <- app(widget)
  widget$set_timeout(1, function(self, app) fired <<- TRUE)
  widget$remove()

  pilot <- test_app(application, 20, 3)
  on.exit(pilot$stop(), add = TRUE)
  pilot$advance(2)
  expect_false(fired)
})

test_that("queued messages for a removed widget are discarded", {
  hits <- 0L
  Probe <- widget(
    "LifecycleProbe",
    render = function(self) "probe",
    on = list("probe.ping" = function(self, event) hits <<- hits + 1L)
  )
  probe <- Probe()
  pilot <- test_app(app(probe), 20, 3)
  on.exit(pilot$stop(), add = TRUE)

  probe$post_message("probe.ping")
  probe$remove()
  pilot$step()
  expect_identical(hits, 0L)
})

test_that("removing the focused widget moves focus to a live widget", {
  first <- input(id = "first")
  second <- input(id = "second")
  pilot <- test_app(app(vertical(first, second)), 20, 5)
  on.exit(pilot$stop(), add = TRUE)
  expect_identical(pilot$app$focused, first)

  first$remove()
  pilot$step()
  expect_identical(pilot$app$focused, second)
})

test_that("removing widgets and screens cancels their subprocesses", {
  skip_on_cran()
  rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")

  owner <- label("owner")
  application <- app(owner)
  detached <- tryCatch(owner$run_process(rscript, c("-e", "Sys.sleep(60)"), env = child_tmp()), error = identity)
  if (inherits(detached, "error") && grepl("Access is denied", conditionMessage(detached), fixed = TRUE) &&
      !identical(tolower(Sys.getenv("CI")), "true")) {
    skip("The sandbox blocks processx child-process pipes.")
  }
  if (inherits(detached, "error")) stop(detached)
  on.exit(detached$cancel(), add = TRUE)
  expect_true(detached$is_running())
  owner$remove()
  expect_identical(detached$state, "cancelled")
  expect_false(detached$.__enclos_env__$private$process$is_alive())

  pilot <- test_app(application, 20, 3)
  on.exit(pilot$stop(), add = TRUE)
  screen_owner <- label("screen worker")
  screen <- modal(screen_owner)
  application$push_screen(screen)
  pilot$step()
  screen_worker <- screen_owner$run_process(rscript, c("-e", "Sys.sleep(60)"), env = child_tmp())
  on.exit(screen_worker$cancel(), add = TRUE)
  expect_true(screen_worker$is_running())
  application$pop_screen()
  pilot$step()
  expect_identical(screen_worker$state, "cancelled")
  expect_false(screen_worker$.__enclos_env__$private$process$is_alive())
})

test_that("a shutdown hook error cannot prevent terminal restoration", {
  BrokenShutdown <- R6::R6Class(
    "BrokenShutdown",
    inherit = Label,
    public = list(on_app_shutdown = function() stop("shutdown hook failed"))
  )
  driver <- HeadlessDriver$new(20, 3)
  driver$press("q")
  application <- app(BrokenShutdown$new("x"), bind("q", function(app) app$exit()))

  expect_warning(run(application, driver = driver), "shutdown hook failed")
  expect_false(application$running)
  expect_false(driver$started)
  expect_false(driver$terminal$alt_screen)
  expect_true(driver$terminal$cursor_visible)
})

test_that("Ctrl+C exits through the normal app shutdown path", {
  driver <- HeadlessDriver$new(20, 3)
  driver$press("ctrl+c")
  application <- app(label("x"))

  run(application, driver = driver)
  expect_false(application$running)
  expect_false(driver$started)
  expect_false(driver$terminal$alt_screen)
  expect_true(driver$terminal$cursor_visible)
})

test_that("static rendering does not start app lifecycle state", {
  current <- current_app()
  mounted <- 0L
  Probe <- widget(
    "StaticRenderProbe",
    render = function(self) "static",
    on = list(mount = function(self, event) mounted <<- mounted + 1L)
  )
  probe <- Probe()

  expect_identical(render_lines(probe, 12, 2), c("static", ""))
  expect_identical(mounted, 0L)
  expect_identical(current_app(), current)
})

test_that("a stopped app can be collected", {
  current <- current_app()
  collected <- FALSE
  application <- app(label("x"))
  reg.finalizer(application, function(e) collected <<- TRUE, onexit = TRUE)
  pilot <- test_app(application, 10, 1)
  pilot$stop()
  rm(pilot, application)

  for (i in seq_len(5L)) gc()
  expect_true(collected)
  expect_identical(current_app(), current)
})

test_that("the event-loop guard ignores queued lifecycle events but stops real loops", {
  big <- app(vertical(lapply(1:60, function(i) label(paste("row", i)))))
  big$.__enclos_env__$private$max_events_per_tick <- 20L
  expect_no_warning(pilot <- test_app(big))
  pilot$stop()

  looping <- app(
    label("x", id = "x"),
    on("ping", function(event, app) app$post_message("ping"))
  )
  looping$.__enclos_env__$private$max_events_per_tick <- 20L
  pilot <- test_app(looping)
  on.exit(pilot$stop(), add = TRUE)
  looping$post_message("ping")
  expect_warning(pilot$step(), "too many events")
})

test_that("an app can run twice; stopping it cancels its timers", {
  a <- app(label("hello", id = "x"))
  ticks <- 0
  a$set_interval(1, function(app) ticks <<- ticks + 1)
  a$set_timeout(2.5, function(app) app$exit("first"))
  expect_identical(run(a, driver = HeadlessDriver$new(20, 3)), "first")
  expect_gte(ticks, 2)
  before <- ticks
  seen <- FALSE
  d <- HeadlessDriver$new(20, 3)
  a$set_timeout(1.5, function(app) {
    seen <<- any(grepl("hello", d$screen_text()))
    app$exit("second")
  })
  expect_identical(run(a, driver = d), "second")
  expect_true(seen)
  expect_identical(ticks, before)
})

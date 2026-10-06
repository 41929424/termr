test_that("signals read, write and ignore identical writes", {
  x <- signal(1)
  expect_identical(x(), 1)
  x(2)
  expect_identical(x(), 2)
  runs <- 0L
  w <- watch(function() {
    x()
    runs <<- runs + 1L
  })
  expect_identical(runs, 1L)
  x(2)
  expect_identical(runs, 1L)
  x(3)
  expect_identical(runs, 2L)
  dispose(w)
  x(4)
  expect_identical(runs, 2L)
})

test_that("computed values are lazy and cached", {
  x <- signal(2)
  evals <- 0L
  sq <- computed(function() {
    evals <<- evals + 1L
    x()^2
  })
  expect_identical(evals, 0L)
  expect_identical(sq(), 4)
  expect_identical(sq(), 4)
  expect_identical(evals, 1L)
  x(3)
  expect_identical(evals, 1L)
  expect_identical(sq(), 9)
  expect_identical(evals, 2L)
})

test_that("watchers run in creation order and see consistent values (diamond)", {
  a <- signal(1)
  b <- computed(function() a() + 1)
  ten <- computed(function() a() * 10)
  log <- character()
  w1 <- watch(function() log <<- c(log, paste("w1", b() + ten())))
  w2 <- watch(function() log <<- c(log, paste("w2", a())))
  expect_identical(log, c("w1 12", "w2 1"))
  a(2)
  expect_identical(log, c("w1 12", "w2 1", "w1 23", "w2 2"))
})

test_that("a watcher does not run when a computed returns the same value", {
  x <- signal(1)
  parity <- computed(function() x() %% 2)
  runs <- 0L
  watch(function() {
    parity()
    runs <<- runs + 1L
  })
  x(3)
  expect_identical(runs, 1L)
  x(4)
  expect_identical(runs, 2L)
})

test_that("batch groups writes into one watcher run", {
  a <- signal(1)
  b <- signal(1)
  seen <- list()
  watch(function() seen[[length(seen) + 1L]] <<- c(a(), b()))
  batch({
    a(2)
    b(3)
  })
  expect_length(seen, 2L)
  expect_identical(seen[[2]], c(2, 3))
  # Nested batches flush at the outermost end.
  batch({
    batch(a(5))
    expect_length(seen, 2L)
    b(6)
  })
  expect_length(seen, 3L)
  # An error inside a batch still flushes and propagates.
  expect_error(batch({
    a(7)
    stop("boom")
  }), "boom")
  expect_identical(seen[[length(seen)]][[1]], 7)
  expect_identical(batch(42), 42)
})

test_that("dynamic dependencies are re-tracked", {
  flag <- signal(TRUE)
  a <- signal("a")
  b <- signal("b")
  runs <- 0L
  watch(function() {
    runs <<- runs + 1L
    if (flag()) a() else b()
  })
  b("b2")
  expect_identical(runs, 1L)
  flag(FALSE)
  expect_identical(runs, 2L)
  a("a2")
  expect_identical(runs, 2L)
  b("b3")
  expect_identical(runs, 3L)
})

test_that("peek and untracked create no dependency", {
  a <- signal(1)
  b <- signal(1)
  runs <- 0L
  watch(function() {
    runs <<- runs + 1L
    a()
    peek(b)
    untracked(b())
  })
  b(2)
  expect_identical(runs, 1L)
  a(2)
  expect_identical(runs, 2L)
})

test_that("cycles are detected", {
  a <- computed(function() b())
  b <- computed(function() a())
  expect_error(a(), "Cycle detected")
  old <- rx$max_runs
  rx$max_runs <- 200L
  on.exit(rx$max_runs <- old)
  p <- signal(0)
  q <- signal(0)
  watch(function() q(p() + 1))
  expect_error(watch(function() p(q() + 1)), "Reactive cycle")
  expect_identical(length(rx$pending), 0L)
})

test_that("watchers that write signals settle", {
  a <- signal(1)
  doubled <- signal(0)
  watch(function() doubled(a() * 2))
  expect_identical(doubled(), 2)
  a(5)
  expect_identical(doubled(), 10)
})

test_that("errors in a watcher's first run propagate and leave no trace", {
  x <- signal(1)
  expect_error(watch(function() {
    x()
    stop("bad watcher")
  }), "bad watcher")
  expect_no_error(x(2))
})

test_that("update_signal and printing", {
  x <- signal(1)
  update_signal(x, function(v) v + 1)
  expect_identical(x(), 2)
  expect_output(print(x), "<signal>")
  expect_output(print(computed(function() 1)), "<computed")
  expect_error(update_signal(computed(function() 1), identity), "signal")
})

test_that("labels and buttons bound to signals re-render", {
  count <- signal(0)
  double <- computed(function() count() * 2)
  lbl <- label(function() paste("Double:", double()), id = "lbl")
  btn <- button("Add", id = "add", on_press = function() count(count() + 1))
  a <- app(vertical(lbl, btn))
  pilot <- test_app(a, 30, 4)
  expect_match(pilot$screen_text()[[1]], "Double: 0")
  pilot$press("tab")
  pilot$press("enter")
  expect_match(pilot$screen_text()[[1]], "Double: 2")
  pilot$press("space", "space")
  expect_match(pilot$screen_text()[[1]], "Double: 6")
  expect_identical(count(), 3)
})

test_that("handlers write several signals in one batch", {
  a <- signal(1)
  b <- signal(1)
  renders <- 0L
  lbl <- label(function() {
    renders <<- renders + 1L
    paste(a(), b())
  })
  btn <- button("Go", id = "go", on_press = function() {
    a(2)
    b(2)
  })
  a_app <- app(vertical(lbl, btn))
  pilot <- test_app(a_app, 20, 4)
  before <- renders
  pilot$press("tab", "enter")
  expect_identical(renders - before, 1L)
  expect_match(pilot$screen_text()[[1]], "2 2")
})

test_that("bindings end when the widget is removed", {
  x <- signal("one")
  runs <- 0L
  lbl <- label(function() {
    runs <<- runs + 1L
    x()
  })
  host <- vertical(lbl)
  a <- app(host)
  pilot <- test_app(a, 20, 3)
  x("two")
  expect_identical(runs, 2L)
  lbl$remove()
  x("three")
  expect_identical(runs, 2L)
})

test_that("other widgets accept reactive functions", {
  v <- signal(0.25)
  pb <- progress_bar(function() v())
  m <- metric("CPU", function() v() * 100)
  s <- sparkline(function() c(1, v()))
  kv <- key_value(function() list(value = v()))
  expect_identical(pb$value, 0.25)
  expect_identical(m$value, 25)
  v(0.5)
  expect_identical(pb$value, 0.5)
  expect_identical(m$value, 50)
  expect_identical(s$data, c(1, 0.5))
  expect_identical(kv$data$value, 0.5)
})

test_that("a thousand signals and watchers stay fast", {
  sigs <- lapply(1:1000, function(i) signal(i))
  total <- computed(function() sum(vapply(sigs, function(s) s(), 0)))
  runs <- 0L
  w <- watch(function() {
    total()
    runs <<- runs + 1L
  })
  elapsed <- system.time(for (i in 1:20) sigs[[i]](-i))[["elapsed"]]
  expect_identical(runs, 21L)
  expect_lt(elapsed, 3)
  dispose(w)
})

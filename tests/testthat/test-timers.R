test_that("timeouts fire once and intervals repeat", {
  now <- 0
  tm <- TimerManager$new(function() now)
  fired <- character()
  tm$add(1, function() fired <<- c(fired, "once"))
  tm$add(0.5, function() fired <<- c(fired, "tick"), repeating = TRUE)
  expect_identical(tm$time_until_next(), 0.5)
  run <- function(t) t$callback()
  now <- 0.5
  tm$fire_due(run)
  now <- 1
  tm$fire_due(run)
  now <- 1.5
  tm$fire_due(run)
  # Timers due at the same time fire in creation order.
  expect_identical(fired, c("tick", "once", "tick", "tick"))
  expect_length(tm$timers, 1)
})

test_that("cancelled timers do not fire and late intervals do not burst", {
  now <- 0
  tm <- TimerManager$new(function() now)
  count <- 0L
  t1 <- tm$add(1, function() count <<- count + 100L)
  tm$add(1, function() count <<- count + 1L, repeating = TRUE)
  t1$cancel()
  now <- 10
  tm$fire_due(function(t) t$callback())
  expect_identical(count, 1L)
  expect_identical(tm$time_until_next(), 1)
})

test_that("timer arguments are validated", {
  tm <- TimerManager$new(function() 0)
  expect_error(tm$add(-1, function() NULL), "non-negative")
  expect_error(tm$add(0, function() NULL, repeating = TRUE), "positive")
  expect_error(tm$add(1, "f"), "function")
})

test_that("the event queue is FIFO", {
  q <- EventQueue$new()
  expect_true(q$is_empty())
  for (i in 1:100) q$push(i)
  out <- integer()
  for (i in 1:70) out <- c(out, q$pop())
  q$push(101L)
  while (!is.null(x <- q$pop())) out <- c(out, x)
  expect_identical(out, c(1:100, 101L))
  expect_identical(q$size(), 0L)
})

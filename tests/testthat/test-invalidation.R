layouts <- function(a) a$frame_stats[["layouts"]]

test_that("a colour change repaints without a new layout pass", {
  lbl <- label("hello", id = "lbl", style = style(foreground = "red"))
  a <- app(vertical(lbl, label("other")))
  pilot <- test_app(a, 20, 4)
  before <- layouts(a)
  lbl$style <- style(foreground = "blue")
  pilot$step()
  expect_identical(layouts(a), before)
  expect_identical(pilot$driver$terminal$screen$get_cell(1, 1)$fg, "blue")
  # A size property does require layout.
  lbl$style <- style(foreground = "blue", width = 12)
  pilot$step()
  expect_identical(layouts(a), before + 1L)
  expect_identical(lbl$region$width, 12L)
})

test_that("changing label text re-lays out, changing nothing does not", {
  lbl <- label("a", id = "lbl")
  a <- app(horizontal(lbl, label("b")))
  pilot <- test_app(a, 20, 2)
  before <- layouts(a)
  lbl$update("a much longer text")
  pilot$step()
  expect_gt(layouts(a), before)
  expect_identical(lbl$region$width, 18L)
  mark <- layouts(a)
  lbl$update("a much longer text")
  pilot$step()
  expect_identical(layouts(a), mark)
})

test_that("cursor moves and scrolling in tables, lists and inputs do not re-lay out", {
  tbl <- data_table(data.frame(n = 1:100), id = "t")
  opt <- option_list(paste("item", 1:50), id = "o")
  inp <- input("hello world", id = "i")
  ta <- text_area(paste("line", 1:80, collapse = "\n"), id = "ta")
  a <- app(vertical(tbl, opt, inp, ta))
  pilot <- test_app(a, 40, 24)
  before <- layouts(a)
  pilot$focus(tbl)
  pilot$press("down", "down", "pagedown")
  pilot$focus(opt)
  pilot$press("down", "down", "pagedown")
  pilot$focus(inp)
  pilot$press("left", "left", "shift+left", "home")
  pilot$focus(ta)
  pilot$press("down", "right", "shift+down", "pagedown")
  expect_identical(layouts(a), before)
  # ... but editing text does.
  pilot$type("x")
  expect_gt(layouts(a), before)
})

test_that("progress bars and spinners animate without layout", {
  pb <- progress_bar(0.1, id = "p")
  sp <- spinner("working", id = "s")
  a <- app(vertical(pb, sp))
  pilot <- test_app(a, 30, 3)
  before <- layouts(a)
  for (v in c(0.3, 0.6, 0.9)) {
    pb$value <- v
    pilot$step()
  }
  pilot$advance(1)
  expect_identical(layouts(a), before)
  expect_match(pilot$screen_text()[[1]], "90%")
})

test_that("window size, mounting and focus changes still re-lay out", {
  a <- app(vertical(label("x", id = "x"), button("b", id = "b")))
  pilot <- test_app(a, 20, 5)
  n <- layouts(a)
  pilot$resize(30, 6)
  expect_gt(layouts(a), n)
  n <- layouts(a)
  a$query_one("#x")$parent$mount(label("new", id = "new"))
  pilot$step()
  expect_gt(layouts(a), n)
  n <- layouts(a)
  a$push_screen(modal(label("dialog")))
  pilot$step()
  expect_gt(layouts(a), n)
})

test_that("skipping layout never changes what is on the screen", {
  run <- function(skip, seed) {
    old <- options(termr.skip_layout = skip)
    on.exit(options(old))
    set.seed(seed)
    tbl <- data_table(data.frame(a = 1:60, b = letters[(0:59 %% 26) + 1]), id = "t")
    lbl <- label("status", id = "lbl")
    inp <- input("abc", id = "i")
    ta <- text_area("one\ntwo\nthree", id = "ta")
    pb <- progress_bar(0, id = "p")
    a <- app(vertical(lbl, tbl, inp, ta, pb))
    pilot <- test_app(a, 40, 20)
    frames <- list()
    ops <- list(
      function() pilot$press(sample(c("down", "up", "pagedown", "tab", "left", "right", "end", "home"), 1)),
      function() pilot$type(sample(c("a", "b", "z"), 1)),
      function() lbl$style <- style(foreground = sample(c("red", "green", "blue"), 1)),
      function() lbl$update(paste(rep("w", sample(1:30, 1)), collapse = "")),
      function() pb$value <- runif(1),
      function() tbl$sort("b", decreasing = sample(c(TRUE, FALSE), 1)),
      function() pilot$resize(sample(25:50, 1), sample(12:24, 1)),
      function() pilot$press("ctrl+z")
    )
    for (i in seq_len(stress_workload(80L, 30L))) {
      ops[[sample(length(ops), 1)]]()
      pilot$step()
      frames[[i]] <- pilot$screen_text()
    }
    frames
  }
  for (seed in stress_workload(1:3, 1L)) {
    expect_identical(run(TRUE, seed), run(FALSE, seed), info = seed)
  }
})

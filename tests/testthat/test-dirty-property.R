rich_ui <- function() {
  tree <- tree_view(tree_node("root", tree_node("a", "a1", "a2"), "b", expanded = TRUE),
                    id = "tree", style = style(height = 6))
  vertical(
    horizontal(
      vertical(
        label("\u4e2d\u6587\u6587\u5b57 \U0001F600 e\u0301 \U0001F468\u200d\U0001F469\u200d\U0001F467 wrapped text goes here", id = "wide",
              style = style(wrap = "word", width = 14)),
        spinner("busy", id = "spin"),
        progress_bar(0, id = "pb"),
        style = style(width = 16, height = "auto")
      ),
      tree,
      text_area("hello\nworld \u4e2d\u6587\nthird line", id = "ta", style = style(height = 6, width = 18))
    ),
    tabs(
      tab("One", option_list(paste("opt", 1:20), id = "opts", style = style(height = 4))),
      tab("Two", data_table(data.frame(n = 1:40, s = letters[(0:39 %% 26) + 1]), id = "tbl", cursor = "cell")),
      id = "tabs"
    ),
    input("\u4e2d\u6587", id = "in", style = style(border = "none")),
    label("footer", id = "footer", style = style(background = "$surface"))
  )
}

random_ops <- function(a, pilot, counter) {
  list(
    function() pilot$press(sample(c("tab", "shift+tab", "down", "up", "left", "right", "pagedown", "home", "end", "enter"), 1)),
    function() pilot$press(sample(c("shift+left", "shift+down", "ctrl+z", "ctrl+f", "escape", "backspace"), 1)),
    function() pilot$type(sample(c("a", "\u4e2d", "\U0001F600", " "), 1)),
    function() pilot$hover(sample(1:60, 1), sample(1:20, 1)),
    function() pilot$click(sample(1:60, 1), sample(1:20, 1)),
    function() pilot$scroll(sample(1:60, 1), direction = sample(c("up", "down"), 1), y = sample(1:20, 1)),
    function() a$query_one("#pb")$set(value = runif(1)),
    function() a$query_one("#wide")$update(paste(sample(c("\u4e2d\u6587", "ab", "\U0001F600", "x"), 8, TRUE), collapse = " ")),
    function() a$query_one("#footer")$set(style = style(foreground = sample(c("red", "green"), 1), background = "$surface")),
    function() a$notify(paste("note", sample(100, 1)), timeout = 1),
    function() pilot$advance(sample(c(0.1, 0.3, 0.7), 1)),
    function() a$push_screen(modal(label("dialog \u4e2d"), button("ok", id = "okb"))),
    function() if (length(a$screens) > 1) a$pop_screen(),
    function() a$query_one("#tree")$root$children[[1]]$toggle()
  )
}

test_that("rectangular incremental repaint equals a full repaint (randomised, rich UI)", {
  for (seed in c(11, 12, 13)) {
    set.seed(seed)
    a <- app(rich_ui())
    pilot <- test_app(a, 62, 22)
    ops <- random_ops(a, pilot)
    log <- character()
    for (i in 1:100) {
      k <- sample(length(ops), 1)
      log <- c(log, k)
      op <- ops[[k]]
      ok <- tryCatch({ op(); TRUE }, error = function(e) conditionMessage(e))
      pilot$step()
      same <- a$frame$equals(full_frame(a))
      if (!same || !isTRUE(ok)) {
        fail(sprintf("seed %d step %d (ops %s): %s", seed, i, paste(log, collapse = ","), if (isTRUE(ok)) "frame differs" else ok))
        break
      }
    }
    expect_gt(a$frame_stats[["incremental"]], 5L)
  }
})

test_that("a rectangle edge never cuts a wide grapheme", {
  a <- app(vertical(
    label(strrep("\u4e2d", 10), id = "w"),
    label("x", id = "x"),
    label(strrep("\U0001F600", 8), id = "e")
  ))
  pilot <- test_app(a, 21, 4)
  for (i in 1:20) {
    a$query_one("#x")$update(strrep("y", i))
    a$query_one("#w")$update(paste0(strrep("a", i %% 3), strrep("\u4e2d", 10 - i %% 3)))
    pilot$step()
    expect_true(a$frame$equals(full_frame(a)), info = i)
  }
})

test_that("lazy scroll-view layout and rectangular repaint match a full render (randomised)", {
  for (seed in 21:23) {
    set.seed(seed)
    a <- app(vertical(
      label("header", id = "hdr"),
      scroll_view(vertical(lapply(1:40, function(i) {
        if (i %% 5 == 0) input(paste("in", i), id = paste0("w", i)) else button(paste("B", i), id = paste0("w", i))
      })), id = "sv", style = style(height = 10)),
      label("footer", id = "ftr")
    ))
    pilot <- test_app(a, 30, 16)
    ops <- list(
      function() pilot$press(sample(c("tab", "shift+tab", "down", "pagedown", "up", "pageup", "end", "home"), 1)),
      function() pilot$scroll(5, direction = sample(c("up", "down"), 1), y = 5),
      function() a$query_one("#sv")$scroll_by(0, sample(-5:15, 1)),
      function() a$query_one("#hdr")$update(strrep("h", sample(1:20, 1))),
      function() pilot$resize(sample(15:40, 1), sample(8:20, 1)),
      function() pilot$click(sample(1:30, 1), sample(1:16, 1)),
      function() a$query_one("#w10")$set(visible = sample(c(TRUE, FALSE), 1))
    )
    log <- integer()
    for (i in 1:80) {
      k <- sample(length(ops), 1)
      log <- c(log, k)
      ops[[k]]()
      pilot$step()
      if (!a$frame$equals(full_frame(a))) {
        fail(sprintf("seed %d step %d (ops %s)", seed, i, paste(log, collapse = ",")))
        break
      }
    }
    expect_true(TRUE)
  }
})

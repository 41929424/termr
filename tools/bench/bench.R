# termr benchmark suite.
#
#   Rscript tools/bench/bench.R                 # all scenarios
#   Rscript tools/bench/bench.R datatable       # scenarios whose name matches
#   Rscript tools/bench/bench.R --samples 9     # more samples (default 7)
#   Rscript tools/bench/bench.R --csv out.csv   # also write the table
#
# Every scenario builds an app on a headless driver whose output is
# discarded, then runs one *tick* of the real event loop (input -> events ->
# layout -> paint -> diff -> ANSI) per iteration. Iterations are timed in
# batches and the median over several samples is reported, so the coarse
# Windows timer (10-15 ms) does not matter. Besides wall time the table shows
# what was actually repainted (from `app$last_paint`): cells, dirty
# rectangles, ANSI bytes and whether the repaint was full. Wall time is
# noisy; the repaint columns are exact.
#
# Compare revisions with tools/bench/compare.R. Base R only.

pkg <- if (file.exists("DESCRIPTION")) read.dcf("DESCRIPTION", "Package")[[1]] else "termr"
suppressMessages({
  if (requireNamespace("pkgload", quietly = TRUE) && file.exists("DESCRIPTION")) {
    pkgload::load_all(".", quiet = TRUE)
  } else {
    library(pkg, character.only = TRUE)
  }
})
ns <- asNamespace(pkg)

args <- commandArgs(trailingOnly = TRUE)
opt <- function(flag, default) {
  i <- match(flag, args)
  if (is.na(i)) default else args[[i + 1L]]
}
samples <- as.integer(opt("--samples", 7L))
csv <- opt("--csv", NULL)
positional <- args[!args %in% c("--samples", "--csv", csv, as.character(samples))]
filter <- if (length(positional)) positional[[1]] else NULL

`%||%` <- function(a, b) if (is.null(a)) b else a
now <- function() proc.time()[["elapsed"]]

# Run a scenario ------------------------------------------------------------

new_session <- function(ui, width, height) {
  a <- app(ui)
  driver <- ns$HeadlessDriver$new(width, height)
  driver$terminal <- list2env(list(feed = function(text) NULL, width = width, height = height,
                                   resize = function(w, h) NULL))
  p <- a$.__enclos_env__$private
  p$start(driver)
  p$tick(wait = FALSE)
  cache <- new.env()
  q <- function(selector) {
    # Looking a widget up walks the whole tree; the benchmark times the framework, not the lookup.
    cache[[selector]] %||% (cache[[selector]] <- a$query_one(selector))
  }
  list(app = a, driver = driver, p = p, width = width, height = height, q = q)
}

press <- function(s, ...) {
  s$driver$press(...)
  s$p$tick(wait = FALSE)
}

resize <- function(s, w, h) {
  s$driver$terminal$width <- w
  s$driver$terminal$height <- h
  s$driver$feed(ns$ResizeEvent$new(w, h))
  s$p$tick(wait = FALSE)
}

bench <- function(name, setup, step = function(s, i) s$p$tick(wait = FALSE), iters = 10L, size = c(120, 40)) {
  list(name = name, setup = setup, step = step, iters = iters, size = size)
}

run_scenario <- function(sc) {
  s <- sc$setup(sc$size[[1]], sc$size[[2]])
  # warm up (JIT, caches)
  for (i in 1:3) sc$step(s, i)
  per_iter <- numeric(samples)
  paints <- list()
  k <- 0L
  for (r in seq_len(samples)) {
    t0 <- now()
    for (i in seq_len(sc$iters)) {
      k <- k + 1L
      before <- sum(s$app$frame_stats[c("full", "incremental")])
      sc$step(s, k)
      after <- sum(s$app$frame_stats[c("full", "incremental")])
      lp <- s$app$last_paint  # NULL on revisions without repaint instrumentation
      if (after > before && !is.null(lp)) paints[[length(paints) + 1L]] <- lp
    }
    per_iter[[r]] <- (now() - t0) * 1000 / sc$iters
  }
  s$p$shutdown()
  pick <- function(field) if (length(paints)) stats::median(vapply(paints, function(x) as.numeric(x[[field]]), 0)) else 0
  data.frame(
    scenario = sc$name,
    screen = if (length(paints)) paints[[1]]$screen_cells else sc$size[[1]] * sc$size[[2]],
    median_ms = round(stats::median(per_iter), 2),
    p90_ms = round(stats::quantile(per_iter, 0.9, names = FALSE), 2),
    repaint_cells = round(pick("repainted_cells")),
    rects = round(pick("rects")),
    ansi_bytes = round(pick("ansi_bytes")),
    full = if (length(paints)) sprintf("%d/%d", sum(vapply(paints, function(x) isTRUE(x$full), TRUE)), length(paints)) else "-",
    stringsAsFactors = FALSE
  )
}

# UI builders -------------------------------------------------------------------

labels_ui <- function(n) vertical(lapply(seq_len(n), function(i) label(paste("Label", i), id = paste0("l", i))))

nested_ui <- function(depth, breadth) {
  count <- 0L
  build <- function(d) {
    if (d == 0L) {
      count <<- count + 1L
      return(label("leaf", id = paste0("leaf", count)))
    }
    kids <- lapply(seq_len(breadth), function(i) build(d - 1L))
    if (d %% 2L == 0L) do.call(vertical, kids) else do.call(horizontal, kids)
  }
  vertical(label("tick", id = "tick"), build(depth))
}

table_data <- function(n) data.frame(id = seq_len(n), group = rep(letters, length.out = n),
                                     value = seq_len(n) / 7, flag = rep(c(TRUE, FALSE), length.out = n))

big_tree <- function() {
  kids <- lapply(1:100, function(i) do.call(tree_node, c(list(paste("dir", i)), lapply(1:100, function(j) paste("file", i, j)))))
  tree_view(do.call(tree_node, c(list("root", expanded = TRUE), kids)), id = "tree")
}

single <- function(ui_fn) function(w, h) new_session(ui_fn(), w, h)

scenarios <- list(
  # rendering
  bench("static-80x24", single(function() labels_ui(30)), function(s, i) { s$app$refresh(); s$p$tick(wait = FALSE) }, size = c(80, 24)),
  bench("static-120x40", single(function() labels_ui(40)), function(s, i) { s$app$refresh(); s$p$tick(wait = FALSE) }),
  bench("static-200x60", single(function() labels_ui(60)), function(s, i) { s$app$refresh(); s$p$tick(wait = FALSE) }, size = c(200, 60)),
  # widget tree
  bench("tree341-label-update", single(function() nested_ui(4, 4)),
        function(s, i) { s$q("#tick")$update(paste("tick", i)); s$p$tick(wait = FALSE) }),
  bench("tree341-style-update", single(function() nested_ui(4, 4)),
        function(s, i) { s$q("#leaf1")$set(style = style(foreground = if (i %% 2) "red" else "green")); s$p$tick(wait = FALSE) }),
  bench("tree1000-label-update", single(function() nested_ui(5, 4)),
        function(s, i) { s$q("#tick")$update(paste("tick", i)); s$p$tick(wait = FALSE) }, iters = 5L),
  bench("focus-move-form", single(function() vertical(lapply(1:20, function(i) input(paste("field", i), id = paste0("in", i))))),
        function(s, i) press(s, "tab"), size = c(80, 60)),
  # layout
  bench("layout-nested-resize", single(function() nested_ui(4, 4)),
        function(s, i) resize(s, 120 - i %% 2, 40), iters = 5L),
  bench("grid-resize", single(function() grid_layout(lapply(1:60, function(i) label(paste("cell", i))), columns = c("1fr", "2fr", "auto"), gap = 1)),
        function(s, i) resize(s, 120 - i %% 3, 40)),
  bench("grid-spans-resize", single(function() grid_layout(lapply(1:40, function(i) label(paste("cell", i), style = style(column_span = 1 + i %% 2))),
                                                          columns = 4, gap = 1)),
        function(s, i) resize(s, 120 - i %% 3, 40)),
  # data table
  bench("datatable-10k-cursor", single(function() data_table(table_data(1e4), id = "t")),
        function(s, i) { s$q("#t")$move_cursor(row = i); s$p$tick(wait = FALSE) }),
  bench("datatable-200k-cursor", single(function() data_table(table_data(2e5), id = "t")),
        function(s, i) { s$q("#t")$move_cursor(row = i); s$p$tick(wait = FALSE) }),
  bench("datatable-1M-cursor", single(function() data_table(table_data(1e6), id = "t")),
        function(s, i) { s$q("#t")$move_cursor(row = i); s$p$tick(wait = FALSE) }),
  bench("datatable-1M-vscroll", single(function() data_table(table_data(1e6), id = "t")),
        function(s, i) press(s, "pagedown")),
  bench("datatable-1M-hscroll", single(function() data_table(table_data(1e6), id = "t", cursor = "cell")),
        function(s, i) press(s, if (i %% 8 < 4) "right" else "left"), size = c(30, 40)),
  bench("datatable-200k-sort", single(function() data_table(table_data(2e5), id = "t")),
        function(s, i) { s$q("#t")$sort(c("group", "value")[1 + i %% 2]); s$p$tick(wait = FALSE) }, iters = 3L),
  # tree and scrolling
  bench("tree-10k-scroll", single(big_tree), function(s, i) press(s, "down"), iters = 5L),
  bench("tree-expand-collapse", single(big_tree),
        function(s, i) press(s, c("down", "enter")[1 + i %% 2]), iters = 5L),
  bench("scrollview-2000-labels", single(function() scroll_view(labels_ui(2000), id = "sv")),
        function(s, i) { s$q("#sv")$scroll_by(0, 1); s$p$tick(wait = FALSE) }, iters = 5L),
  bench("scrollview-page", single(function() scroll_view(labels_ui(2000), id = "sv")),
        function(s, i) { s$q("#sv")$scroll_by(0, 38); s$p$tick(wait = FALSE) }, iters = 5L),
  # text and unicode
  bench("label-ascii", single(function() vertical(label("x", id = "t"), label(strrep("hello world ", 8), style = style(wrap = "word")))),
        function(s, i) { s$q("#t")$update(strrep("a", i %% 50)); s$p$tick(wait = FALSE) }),
  bench("label-cjk", single(function() vertical(label("x", id = "t"), label(strrep("中文", 40), style = style(wrap = "char")))),
        function(s, i) { s$q("#t")$update(strrep("中", i %% 30)); s$p$tick(wait = FALSE) }),
  bench("label-emoji", single(function() vertical(label("x", id = "t"), label(strrep("\U0001F600 ", 40), style = style(wrap = "word")))),
        function(s, i) { s$q("#t")$update(strrep("\U0001F600", i %% 20)); s$p$tick(wait = FALSE) }),
  bench("label-zwj", single(function() vertical(label("x", id = "t"), label(strrep("\U0001F468‍\U0001F469‍\U0001F467 ", 20), style = style(wrap = "word")))),
        function(s, i) { s$q("#t")$update(strrep("\U0001F468‍\U0001F469‍\U0001F467", i %% 10)); s$p$tick(wait = FALSE) }),
  bench("textarea-1MB-type", single(function() text_area(paste(rep("The quick brown fox jumps over the lazy dog 0123456789", 18000), collapse = "\n"), id = "ta")),
        function(s, i) press(s, "x"), iters = 5L),
  bench("textarea-1MB-scroll", single(function() text_area(paste(rep("The quick brown fox jumps over the lazy dog 0123456789", 18000), collapse = "\n"), id = "ta")),
        function(s, i) press(s, "pagedown"), iters = 5L),
  # overlays
  bench("modal-open-close", single(function() vertical(label("main"), data_table(table_data(1000)))),
        function(s, i) { if (i %% 2) s$app$push_screen(modal(label("dialog"), button("ok"))) else s$app$pop_screen(); s$p$tick(wait = FALSE) }),
  bench("notifications", single(function() vertical(label("main"), data_table(table_data(1000)))),
        function(s, i) { s$app$notify(paste("note", i), timeout = Inf); s$p$tick(wait = FALSE) }),
  bench("command-palette", single(function() vertical(label("main"), input())),
        function(s, i) { if (i %% 2) press(s, "ctrl+p") else press(s, "escape") }),
  # reactive graph
  bench("signals-1000", function(w, h) {
    sigs <- lapply(1:1000, function(i) signal(i))
    total <- computed(function() sum(vapply(sigs, function(x) x(), 0)))
    new_session(vertical(label(function() paste("total", total()), id = "t")), w, h) -> s
    s$sigs <- sigs
    s
  }, function(s, i) { s$sigs[[1 + i %% 1000]](-i); s$p$tick(wait = FALSE) })
)

if (!is.null(filter)) scenarios <- Filter(function(s) grepl(filter, s$name, fixed = TRUE), scenarios)

results <- do.call(rbind, lapply(scenarios, function(sc) {
  cat(sprintf("running %s ...\n", sc$name))
  tryCatch(run_scenario(sc), error = function(e) {
    data.frame(scenario = sc$name, screen = NA, median_ms = NA, p90_ms = NA, repaint_cells = NA, rects = NA,
               ansi_bytes = NA, full = paste("error:", substr(conditionMessage(e), 1, 40)), stringsAsFactors = FALSE)
  })
}))
cat("\n")
old_width <- options(width = 200)
print(results, row.names = FALSE)
options(old_width)
cat(sprintf("\n%d samples x iterations per scenario; R %s, %s\n", samples, getRversion(), R.version$platform))
if (!is.null(csv)) utils::write.csv(results, csv, row.names = FALSE)

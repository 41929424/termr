# Standalone reactive graph: signal(), computed(), watch().
#
#   count  <- signal(0)
#   double <- computed(function() count() * 2)
#   watch(function() cat("double is", double(), "\n"))
#   count(5)                       # prints "double is 10"
#
# A small, deterministic graph - not Shiny's engine:
#  * a signal is a function: `x()` reads (and records the dependency for the
#    running computed / watch), `x(value)` writes;
#  * a computed is lazy and cached: it re-evaluates when read after one of its
#    dependencies changed;
#  * a watch (effect) runs immediately and again, in creation order, after
#    the signals it read changed. It re-runs only if a dependency really
#    changed (a computed that returns the same value does not trigger it);
#  * batch() groups writes: watchers run once, at the end;
#  * a computed that depends on itself raises an error; watchers that keep
#    re-triggering each other stop with an error after a bounded number of
#    runs;
#  * dispose() disconnects a watch or computed.
# Widgets can be bound to the graph: label(function() ...) re-renders when
# the signals it reads change.

rx <- new.env(parent = emptyenv())
rx$observer <- NULL      # the node whose function is running (tracking)
rx$depth <- 0L           # batch nesting
rx$pending <- list()     # effects waiting to run, by id
rx$flushing <- FALSE
rx$next_id <- 0L
rx$max_runs <- 10000L

new_node <- function(kind, fn = NULL, value = NULL) {
  rx$next_id <- rx$next_id + 1L
  node <- new.env(parent = emptyenv())
  node$kind <- kind
  node$id <- rx$next_id
  node$fn <- fn
  node$value <- value
  node$version <- 0L
  node$deps <- list()
  node$subs <- list()      # subscribers, keyed by id
  node$dirty <- TRUE
  node$evaluating <- FALSE
  node$disposed <- FALSE
  node$seen <- list()      # dependency versions seen at the last run
  node
}

# Record that the running observer depends on `node`.
track <- function(node) {
  obs <- rx$observer
  if (is.null(obs)) return(invisible())
  key <- as.character(obs$id)
  node$subs[[key]] <- obs
  obs$deps[[as.character(node$id)]] <- node
  invisible()
}

unlink_deps <- function(node) {
  key <- as.character(node$id)
  for (dep in node$deps) dep$subs[[key]] <- NULL
  node$deps <- list()
  node$seen <- list()
}

# A dependency changed: mark computeds below it dirty and queue watchers.
invalidate_subs <- function(node) {
  for (sub in node$subs) {
    if (sub$disposed) next
    if (sub$kind == "computed") {
      if (!sub$dirty) {
        sub$dirty <- TRUE
        invalidate_subs(sub)
      }
    } else if (sub$kind == "effect") {
      rx$pending[[as.character(sub$id)]] <- sub
    }
  }
}

# Evaluate a computed if it is stale. Returns its value.
refresh_computed <- function(node) {
  if (node$evaluating) {
    stop("Cycle detected: a computed() depends on itself.", call. = FALSE)
  }
  if (!node$dirty || node$disposed) return(node$value)
  unlink_deps(node)
  node$evaluating <- TRUE
  old <- rx$observer
  rx$observer <- node
  on.exit({
    rx$observer <- old
    node$evaluating <- FALSE
  }, add = TRUE)
  value <- node$fn()
  node$dirty <- FALSE
  if (node$version == 0L || !identical(value, node$value)) {
    node$value <- value
    node$version <- node$version + 1L
  }
  for (dep in node$deps) node$seen[[as.character(dep$id)]] <- dep$version
  node$value
}

# Has any dependency of an effect really changed since it last ran?
effect_is_stale <- function(node) {
  if (node$version == 0L) return(TRUE)
  for (dep in node$deps) {
    if (dep$kind == "computed") refresh_computed(dep)
    if (!identical(node$seen[[as.character(dep$id)]], dep$version)) return(TRUE)
  }
  FALSE
}

run_effect <- function(node) {
  unlink_deps(node)
  old <- rx$observer
  rx$observer <- node
  on.exit(rx$observer <- old, add = TRUE)
  node$fn()
  node$version <- node$version + 1L
  for (dep in node$deps) node$seen[[as.character(dep$id)]] <- dep$version
  invisible()
}

flush_effects <- function() {
  if (rx$flushing || rx$depth > 0L) return(invisible())
  rx$flushing <- TRUE
  on.exit(rx$flushing <- FALSE, add = TRUE)
  runs <- 0L
  while (length(rx$pending)) {
    ids <- as.integer(names(rx$pending))
    key <- as.character(min(ids))
    node <- rx$pending[[key]]
    rx$pending[[key]] <- NULL
    if (node$disposed || !effect_is_stale(node)) next
    runs <- runs + 1L
    if (runs > rx$max_runs) {
      rx$pending <- list()
      stop("Reactive cycle: watchers keep triggering each other (stopped after ",
           rx$max_runs, " runs).", call. = FALSE)
    }
    run_effect(node)
  }
  invisible()
}

#' Reactive values, computed values and watchers
#'
#' A small reactive graph for application state. `signal()` holds a value,
#' `computed()` derives a value from signals (lazily, cached), and `watch()`
#' runs code whenever the signals it reads change. Widgets can be created
#' from functions that read signals - `label(function() ...)` - and are
#' re-rendered automatically.
#'
#' * Read a signal with `x()`, write it with `x(value)`. Writing the same
#'   value (`identical()`) does nothing. Collections are replaced as a whole:
#'   `items(c(items(), "new"))` - changes made *inside* an object are not
#'   noticed.
#' * `computed(fn)` re-evaluates only when read after a dependency changed.
#'   A computed that depends on itself raises an error.
#' * `watch(fn)` runs `fn` now and again after its dependencies changed, in
#'   creation order. If it writes signals, the watchers depending on them run
#'   afterwards; a loop is stopped with an error. Use `dispose()` to stop it.
#' * `batch(expr)` defers watchers until `expr` is done, so several writes
#'   cause one run. Event handlers of an app run in a batch automatically.
#' * `peek(x)` reads a signal or computed without creating a dependency;
#'   `untracked(expr)` does the same for an expression.
#' * Watchers and bound widgets keep their signals' subscriber lists alive;
#'   call `dispose()` (or remove the widget) when they are no longer needed.
#'
#' This graph is independent of the widget-level [reactive()] fields. It is
#' **experimental**.
#'
#' @param value Initial value of a signal.
#' @param fn A function of no arguments.
#' @param x A signal or computed.
#' @param expr An expression.
#' @return `signal()` and `computed()` return functions (classes
#'   `termr_signal` / `termr_computed`); `watch()` returns a handle with a
#'   `$dispose()` method.
#' @name signals
#' @examples
#' count <- signal(1)
#' double <- computed(function() count() * 2)
#' seen <- c()
#' w <- watch(function() seen <<- c(seen, double()))
#' count(2)
#' batch({ count(3); count(4) })
#' seen
#' dispose(w)
NULL

#' @rdname signals
#' @export
signal <- function(value = NULL) {
  node <- new_node("signal", value = value)
  node$dirty <- FALSE
  f <- function(value) {
    if (missing(value)) {
      track(node)
      return(node$value)
    }
    if (identical(value, node$value)) return(invisible(node$value))
    node$value <- value
    node$version <- node$version + 1L
    invalidate_subs(node)
    flush_effects()
    invisible(value)
  }
  structure(f, node = node, class = c("termr_signal", "function"))
}

#' @rdname signals
#' @export
computed <- function(fn) {
  check_function(fn, "fn")
  node <- new_node("computed", fn = fn)
  f <- function() {
    track(node)
    refresh_computed(node)
  }
  structure(f, node = node, class = c("termr_computed", "function"))
}

#' @rdname signals
#' @export
watch <- function(fn) {
  check_function(fn, "fn")
  node <- new_node("effect", fn = fn)
  # The first run happens now; reads inside it are tracked.
  rx$depth <- rx$depth + 1L
  tryCatch(run_effect(node), error = function(e) {
    rx$depth <- rx$depth - 1L
    unlink_deps(node)
    node$disposed <- TRUE
    stop(e)
  })
  rx$depth <- rx$depth - 1L
  flush_effects()
  structure(list(dispose = function() dispose_node(node), node = node), class = "termr_watch")
}

dispose_node <- function(node) {
  node$disposed <- TRUE
  unlink_deps(node)
  rx$pending[[as.character(node$id)]] <- NULL
  for (sub in node$subs) sub$deps[[as.character(node$id)]] <- NULL
  node$subs <- list()
  invisible()
}

#' @rdname signals
#' @export
dispose <- function(x) {
  if (inherits(x, "termr_watch")) {
    x$dispose()
  } else if (inherits(x, c("termr_computed", "termr_signal"))) {
    dispose_node(attr(x, "node"))
  } else if (is_widget(x)) {
    for (w in x$walk()) widget_private(w)$dispose_bindings()
  } else {
    stop("`x` must be a watch(), computed(), signal() or widget.", call. = FALSE)
  }
  invisible(x)
}

#' @rdname signals
#' @export
batch <- function(expr) {
  rx$depth <- rx$depth + 1L
  value <- tryCatch(expr, error = function(e) {
    rx$depth <- rx$depth - 1L
    try(flush_effects(), silent = TRUE)
    stop(e)
  })
  rx$depth <- rx$depth - 1L
  flush_effects()
  invisible(value)
}

#' @rdname signals
#' @export
peek <- function(x) {
  if (!inherits(x, c("termr_signal", "termr_computed"))) stop("`x` must be a signal() or computed().", call. = FALSE)
  untracked(x())
}

#' @rdname signals
#' @export
untracked <- function(expr) {
  old <- rx$observer
  rx$observer <- NULL
  on.exit(rx$observer <- old)
  expr
}

#' @rdname signals
#' @param fn Function applied to the current value.
#' @export
update_signal <- function(x, fn) {
  if (!inherits(x, "termr_signal")) stop("`x` must be a signal().", call. = FALSE)
  check_function(fn, "fn")
  x(fn(peek(x)))
}

#' @export
print.termr_signal <- function(x, ...) {
  cat("<signal> ", paste(utils::capture.output(utils::str(peek(x), give.head = FALSE)), collapse = " "), "\n", sep = "")
  invisible(x)
}

#' @export
print.termr_computed <- function(x, ...) {
  node <- attr(x, "node")
  cat("<computed", if (node$dirty) " (stale)", ">\n", sep = "")
  invisible(x)
}

#' @export
print.termr_watch <- function(x, ...) {
  cat("<watch", if (x$node$disposed) " (disposed)", ">\n", sep = "")
  invisible(x)
}

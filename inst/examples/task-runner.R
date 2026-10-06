# Task runner: run background jobs, watch progress and logs, cancel them.
# Shows run_worker(): every task runs in its own R process, so the interface
# never blocks. Synthetic tasks only.
#
#   Rscript -e 'termr::run_example("task-runner")'
#
# Keys: Up/Down choose a task   Enter or r run   c cancel   x clear log
#       F1 help   Ctrl+P commands   q quit

library(termr)

tasks <- list(
  list(name = "Sleep (5 s)", fn = function() {
    for (i in 1:5) {
      Sys.sleep(1)
      termr_progress(i / 5, sprintf("slept %d s", i))
    }
    "slept 5 seconds"
  }),
  list(name = "Compute (fit 40 models)", fn = function() {
    set.seed(1)
    r2 <- numeric(40)
    for (i in 1:40) {
      d <- data.frame(x = rnorm(2000), z = rnorm(2000))
      d$y <- d$x * 2 + rnorm(2000)
      r2[[i]] <- summary(stats::lm(y ~ x + z, d))$r.squared
      cat(sprintf("model %02d: R2 = %.3f\n", i, r2[[i]]))
      termr_progress(i / 40, sprintf("model %d of 40", i))
    }
    sprintf("mean R2 = %.3f", mean(r2))
  }),
  list(name = "Generated progress", fn = function() {
    for (i in 1:20) {
      Sys.sleep(0.15)
      termr_progress(i / 20, paste("batch", i))
    }
    message("all batches written")
    "20 batches"
  }),
  list(name = "Failing job", fn = function() {
    Sys.sleep(1)
    cat("about to fail\n")
    stop("disk quota exceeded (simulated)")
  })
)
# The functions run in other R processes: they must not carry this script's environment.
for (i in seq_along(tasks)) environment(tasks[[i]]$fn) <- globalenv()
names(tasks) <- vapply(tasks, `[[`, "", "name")

state <- new.env()
state$worker <- NULL
state$selected <- tasks[[1]]$name

ui <- vertical(
  label("Task runner", style = style(bold = TRUE, foreground = "$accent")),
  horizontal(
    panel(option_list(names(tasks), id = "tasks", style = style(height = "auto")), title = "Tasks", style = style(width = 32, height = "auto")),
    panel(
      label("Idle", id = "status"),
      progress_bar(0, id = "progress"),
      label("", id = "message", style = style(foreground = "$muted")),
      horizontal(
        button("Run", id = "run", variant = "primary"),
        button("Cancel", id = "cancel", disabled = TRUE),
        style = style(height = "auto")
      ),
      title = "Current task", style = style(height = "auto", width = "1fr")
    ),
    style = style(height = "auto")
  ),
  panel(log_view(max_lines = 500, timestamps = TRUE, id = "log"), title = "Log", style = style(height = "1fr")),
  label("Enter run   c cancel   x clear log   q quit", style = style(foreground = "$muted")),
  style = style(padding = c(0, 1))
)

set_running <- function(app, running) {
  app$query_one("#run")$set(disabled = running)
  app$query_one("#cancel")$set(disabled = !running)
  app$query_one("#tasks")$set(disabled = running)
}

start_task <- function(app) {
  if (!is.null(state$worker) && state$worker$is_running()) return(invisible())
  name <- state$selected
  log <- app$query_one("#log")
  log$write("Started: ", name)
  app$query_one("#status")$update(span(paste("Running:", name), style(foreground = "$accent")))
  app$query_one("#progress")$set(value = 0)
  set_running(app, TRUE)
  state$worker <- app$run_worker(
    tasks[[name]]$fn,
    name = name,
    timeout = 120,
    on_progress = function(value, message, app) {
      app$query_one("#progress")$set(value = value)
      app$query_one("#message")$update(message)
    },
    on_stdout = function(line, app) log$write(line, level = "debug"),
    on_stderr = function(line, app) log$write(line, level = "warning"),
    on_complete = function(result, app) {
      log$write("Finished: ", result, level = "success")
      app$query_one("#status")$update(span(paste("Done:", name), style(foreground = "$success")))
      app$notify(result, title = name, severity = "success")
      set_running(app, FALSE)
    },
    on_error = function(message, app) {
      log$write("Failed: ", message, level = "error")
      app$query_one("#status")$update(span(paste("Failed:", name), style(foreground = "$error")))
      app$notify(message, title = name, severity = "error")
      set_running(app, FALSE)
    }
  )
}

cancel_task <- function(app) {
  w <- state$worker
  if (is.null(w) || !w$is_running()) return(invisible())
  w$cancel()
  app$query_one("#log")$write("Cancelled: ", state$selected, level = "warning")
  app$query_one("#status")$update(span(paste("Cancelled:", state$selected), style(foreground = "$warning")))
  set_running(app, FALSE)
}

runner <- app(
  ui,
  title = "Task runner",
  bind("q", "quit", "Quit"),
  bind("r", function(app) start_task(app), "Run task"),
  bind("c", function(app) cancel_task(app), "Cancel task"),
  bind("x", function(app) app$query_one("#log")$clear(), "Clear log"),
  on("option_list.highlighted", "#tasks", function(event, app) state$selected <- event$data$value),
  on("option_list.selected", "#tasks", function(event, app) {
    state$selected <- event$data$value
    start_task(app)
  }),
  on("button.pressed", "#run", function(event, app) start_task(app)),
  on("button.pressed", "#cancel", function(event, app) cancel_task(app))
)
runner$add_command(command("Run selected task", function(app) start_task(app), category = "Tasks", shortcut = "r"))
runner$add_command(command("Cancel running task", function(app) cancel_task(app), category = "Tasks", shortcut = "c",
                           enabled = function(app) !is.null(state$worker) && state$worker$is_running()))
runner$call_later(function(app) app$query_one("#log")$write("Choose a task and press Enter.", level = "info"))

run(runner)

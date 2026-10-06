# Terminal dashboard: a polished live dashboard with synthetic data -
# metrics with trends, sparklines, gauges, a service table and an event feed.
#
#   Rscript -e 'termr::run_example("terminal-dashboard")'
#
# Keys: p pause/resume   t cycle theme   F1 help   Ctrl+P commands   q quit
# (Honours NO_COLOR and options(termr.reduce_motion = TRUE).)

library(termr)

set.seed(7)
state <- new.env()
state$paused <- FALSE
state$tick <- 0L
state$rps <- 480
state$latency <- 120
state$errors <- 0.4
state$cpu <- 42
services <- data.frame(
  service = c("api", "auth", "search", "billing", "worker", "cache"),
  status = "ok", load = c(52, 31, 67, 18, 44, 23), p95 = c(120, 80, 210, 95, 340, 12),
  stringsAsFactors = FALSE
)

walk <- function(x, step, lo = 0, hi = Inf) min(hi, max(lo, x + stats::rnorm(1, 0, step)))
themes <- c("default", "dark", "light", "high-contrast")

ui <- vertical(
  horizontal(
    label("Service dashboard", style = style(bold = TRUE, foreground = "$accent", width = "1fr")),
    spinner("live", id = "live", style = style(width = "auto")),
    style = style(height = "auto")
  ),
  horizontal(
    metric("Requests/s", "-", id = "rps"),
    metric("Latency p95", "-", id = "lat"),
    metric("Error rate", "-", id = "err"),
    metric("CPU", "-", id = "cpu"),
    style = style(height = "auto")
  ),
  grid_layout(
    panel(sparkline(id = "rps-spark", min = 0), title = "Requests/s", style = style(height = "auto")),
    panel(sparkline(id = "lat-spark", min = 0), title = "Latency (ms)", style = style(height = "auto")),
    panel(progress_bar(0, id = "cpu-bar"), progress_bar(0, id = "mem-bar"), title = "CPU / memory", style = style(height = "auto")),
    panel(sparkline(id = "err-spark", min = 0), title = "Errors (%)", style = style(height = "auto")),
    columns = 2, gap = c(0, 1), style = style(height = "auto")
  ),
  horizontal(
    panel(data_table(services, id = "services", cursor = "none", zebra = TRUE), title = "Services", style = style(width = "1fr")),
    panel(log_view(max_lines = 100, timestamps = TRUE, id = "events"), title = "Events", style = style(width = "1fr")),
    style = style(height = "1fr")
  ),
  label("p pause   t theme   F1 help   q quit", id = "footer", style = style(foreground = "$muted")),
  style = style(padding = c(0, 1))
)

refresh <- function(app) {
  if (state$paused) return(invisible())
  state$tick <- state$tick + 1L
  state$rps <- walk(state$rps, 30, 50, 2000)
  state$latency <- walk(state$latency, 12, 20, 900)
  state$errors <- walk(state$errors, 0.2, 0, 15)
  state$cpu <- walk(state$cpu, 5, 3, 99)
  app$query_one("#rps")$update(sprintf("%.0f", state$rps), delta = stats::rnorm(1, 0, 20))
  app$query_one("#lat")$update(sprintf("%.0f ms", state$latency), delta = stats::rnorm(1, 0, 6))
  app$query_one("#err")$update(sprintf("%.2f%%", state$errors), delta = stats::rnorm(1, 0, 0.1))
  app$query_one("#cpu")$update(sprintf("%.0f%%", state$cpu), delta = stats::rnorm(1, 0, 2))
  app$query_one("#rps-spark")$push(state$rps)
  app$query_one("#lat-spark")$push(state$latency)
  app$query_one("#err-spark")$push(state$errors)
  app$query_one("#cpu-bar")$set(value = state$cpu / 100)
  app$query_one("#mem-bar")$set(value = walk(0.55, 0.05, 0.2, 0.95))
  services$load <<- pmin(100, pmax(1, services$load + round(stats::rnorm(nrow(services), 0, 6))))
  services$p95 <<- pmax(5, services$p95 + round(stats::rnorm(nrow(services), 0, 15)))
  services$status <<- ifelse(services$load > 85 | services$p95 > 400, "degraded", "ok")
  app$query_one("#services")$set_data(services)
  log <- app$query_one("#events")
  if (state$errors > 3) {
    log$write(sprintf("Error rate %.1f%% above threshold", state$errors), level = "error")
  } else if (state$latency > 300) {
    log$write(sprintf("Latency p95 %.0f ms", state$latency), level = "warning")
  } else if (state$tick %% 10 == 0) {
    log$write("All services healthy", level = "success")
  }
}

dash <- app(
  ui, title = "Dashboard", theme = "dark",
  bind("q", "quit", "Quit"),
  bind("p", function(app) {
    state$paused <- !state$paused
    app$query_one("#footer")$update(if (state$paused) "PAUSED - press p to resume" else "p pause   t theme   F1 help   q quit")
  }, "Pause / resume"),
  bind("t", function(app) {
    i <- match(app$theme$name, themes, nomatch = 1L)
    app$theme <- themes[[i %% length(themes) + 1L]]
  }, "Next theme")
)
dash$add_command(command("Inject an error burst", function(app) {
  state$errors <- 9
  app$query_one("#events")$write("Simulated error burst", level = "error")
}, category = "Demo"))
dash$set_interval(0.5, refresh)
dash$call_later(function(app) app$query_one("#events")$write("Dashboard started", level = "info"))

run(dash)

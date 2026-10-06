# System monitor (synthetic data): metrics, sparklines, progress bars and a
# log, all updated by a timer.
#
#   Rscript -e 'termr::run_example("system-monitor")'

library(termr)

history <- list(cpu = numeric(), mem = numeric(), net = numeric())
state <- new.env()
state$cpu <- 35
state$mem <- 52
state$net <- 120

gauge <- function(name, id) {
  panel(
    progress_bar(0, id = paste0(id, "-bar")),
    sparkline(id = paste0(id, "-spark"), min = 0, max = 100),
    title = name
  )
}

ui <- vertical(
  label("termr system monitor (synthetic data)", style = style(bold = TRUE, foreground = "$accent")),
  horizontal(
    metric("CPU", "-", id = "cpu", format = function(x) if (is.numeric(x)) sprintf("%.0f%%", x) else x),
    metric("Memory", "-", id = "mem", format = function(x) if (is.numeric(x)) sprintf("%.0f%%", x) else x),
    metric("Network", "-", id = "net", format = function(x) if (is.numeric(x)) sprintf("%.0f kB/s", x) else x),
    metric("Uptime", "0s", id = "uptime"),
    style = style(height = "auto")
  ),
  grid_layout(gauge("CPU", "cpu"), gauge("Memory", "mem"), columns = 2, gap = c(0, 1), style = style(height = "auto")),
  panel(log_view(max_lines = 200, timestamps = TRUE, id = "log", style = style(height = "1fr")),
        title = "Events", style = style(height = "1fr")),
  label("q quit   Ctrl+P commands", style = style(foreground = "$muted")),
  style = style(padding = c(0, 1))
)

started <- Sys.time()
walk <- function(x, step, lo = 0, hi = 100) min(hi, max(lo, x + stats::rnorm(1, 0, step)))

tick <- function(app) {
  old <- c(state$cpu, state$mem, state$net)
  state$cpu <- walk(state$cpu, 8)
  state$mem <- walk(state$mem, 2)
  state$net <- walk(state$net, 40, 0, 1000)
  app$query_one("#cpu")$update(state$cpu, delta = state$cpu - old[[1]])
  app$query_one("#mem")$update(state$mem, delta = state$mem - old[[2]])
  app$query_one("#net")$update(state$net, delta = state$net - old[[3]])
  app$query_one("#uptime")$update(sprintf("%.0fs", as.numeric(difftime(Sys.time(), started, units = "secs"))))
  app$query_one("#cpu-bar")$set(value = state$cpu / 100)
  app$query_one("#mem-bar")$set(value = state$mem / 100)
  app$query_one("#cpu-spark")$push(state$cpu)
  app$query_one("#mem-spark")$push(state$mem)
  if (state$cpu > 80) app$query_one("#log")$write(sprintf("CPU high: %.0f%%", state$cpu), level = "warning")
  if (stats::runif(1) < 0.1) app$query_one("#log")$write("Heartbeat ok", level = "debug")
}

monitor <- app(ui, bind("q", "quit", "Quit"))
monitor$add_command("Clear log", function(app) app$query_one("#log")$clear())
monitor$add_command("Toggle dark theme", function(app) {
  app$theme <- if (app$theme$name == "dark") "default" else "dark"
})
monitor$set_interval(0.5, tick)
monitor$call_later(function(app) app$query_one("#log")$write("Monitor started", level = "success"))

run(monitor)

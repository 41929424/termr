# Model training monitor: a (synthetic) training run in a background R
# process reports progress; the interface shows epochs, loss, accuracy, a
# loss sparkline and a log, and stays responsive throughout.
#
#   Rscript -e 'termr::run_example("model-monitor")'

library(termr)

train <- function(epochs) {
  loss <- 2.5
  acc <- 0.2
  for (epoch in seq_len(epochs)) {
    Sys.sleep(0.3)
    loss <- loss * runif(1, 0.82, 0.97)
    acc <- min(0.99, acc + (1 - acc) * runif(1, 0.05, 0.2))
    termr_progress(epoch / epochs, sprintf("%d %.4f %.4f", epoch, loss, acc))
  }
  list(loss = loss, accuracy = acc)
}
# The worker runs in a fresh R session: keep the function self-contained
# (its environment is sent along, so do not capture the app or widgets).
environment(train) <- globalenv()

epochs <- 30

ui <- vertical(
  label("Model training monitor", style = style(bold = TRUE, foreground = "$accent")),
  horizontal(
    metric("Epoch", sprintf("0/%d", epochs), id = "epoch"),
    metric("Loss", NA, id = "loss", format = function(x) if (is.na(x)) "-" else sprintf("%.4f", x)),
    metric("Accuracy", NA, id = "acc", format = function(x) if (is.na(x)) "-" else sprintf("%.1f%%", 100 * x)),
    style = style(height = "auto")
  ),
  panel(progress_bar(0, id = "progress"), sparkline(id = "loss-curve"), title = "Progress / loss"),
  panel(log_view(timestamps = TRUE, id = "log", style = style(height = "1fr")), title = "Log", style = style(height = "1fr")),
  horizontal(
    button("Start", id = "start", variant = "primary"),
    button("Cancel", id = "cancel", variant = "error", disabled = TRUE),
    spinner("idle", type = "line", id = "spinner"),
    style = style(height = "auto", valign = "middle")
  ),
  style = style(padding = c(0, 1))
)

monitor <- app(ui, bind("q", "quit", "Quit"))
job <- NULL

monitor$on("button.pressed", "#start", function(event, app) {
  log <- app$query_one("#log")
  log$write("Training started", level = "info")
  app$query_one("#start")$set(disabled = TRUE)
  app$query_one("#cancel")$set(disabled = FALSE)
  app$query_one("#spinner")$set(label = "training")
  previous_loss <- NA
  job <<- app$run_worker(
    train, args = list(epochs = epochs),
    on_progress = function(value, message, app) {
      parts <- as.numeric(strsplit(message, " ")[[1]])
      app$query_one("#epoch")$update(sprintf("%d/%d", parts[[1]], epochs))
      app$query_one("#loss")$update(parts[[2]], delta = if (is.na(previous_loss)) NULL else parts[[2]] - previous_loss)
      app$query_one("#acc")$update(parts[[3]])
      app$query_one("#progress")$set(value = value)
      app$query_one("#loss-curve")$push(parts[[2]])
      previous_loss <<- parts[[2]]
      if (parts[[1]] %% 5 == 0) log$write(sprintf("epoch %d: loss %.4f", parts[[1]], parts[[2]]))
    },
    on_complete = function(result, app) {
      log$write(sprintf("Done: accuracy %.1f%%", 100 * result$accuracy), level = "success")
      app$notify("Training finished", severity = "success")
    },
    on_error = function(message, app) log$write(message, level = "error")
  )
})

monitor$on("button.pressed", "#cancel", function(event, app) {
  if (!is.null(job)) job$cancel()
})

monitor$on("*", function(event, app) {
  if (event$type %in% c("worker.completed", "worker.failed", "worker.cancelled")) {
    app$query_one("#start")$set(disabled = FALSE)
    app$query_one("#cancel")$set(disabled = TRUE)
    app$query_one("#spinner")$set(label = "idle")
    if (event$type == "worker.cancelled") app$query_one("#log")$write("Cancelled", level = "warning")
  }
})

run(monitor)

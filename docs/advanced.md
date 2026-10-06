# Screens, workers and developer tools

## Screens and dialogs

```r
app$push_screen(vertical(label("Settings"), ...))   # a new screen
app$pop_screen()
app$switch_screen(other)

app$push_screen(confirm_dialog("Delete file?", on_confirm = function(app) delete()))
app$push_screen(alert_dialog("Saved"))
dlg <- modal(input(id = "new_name"), button("OK", id = "ok"), title = "Rename")
dlg$on("button.pressed", function(event, app) dlg$dismiss(app$query_one("#new_name")$value))
app$push_screen(dlg, callback = function(result, app) rename(result))
```

The top screen gets the keys, the mouse and the focus. Modal screens are
drawn over the dimmed screen below; Escape closes them and the previous
focus comes back.

## Notifications

```r
app$notify("Saved", title = "File", severity = "success", timeout = 3)
```

Severities: `information`, `success`, `warning`, `error`. Clicking a
notification closes it.

## Background workers

```r
app$run_worker(
  function(path) {
    data <- read.csv(path)
    termr_progress(0.5, "fitting")
    lm(y ~ x, data)
  },
  args = list(path = "data.csv"),
  on_progress = function(value, message, app) bar$set(value = value),
  on_complete = function(fit, app) app$notify("Model ready"),
  on_error = function(message, app) app$notify(message, severity = "error")
)
```

The function runs in a fresh R process (`Rscript --vanilla`): pass data
in `args`, list packages in `packages`. Workers send `worker.*` events,
can be cancelled (`worker$cancel()`), and stop with their owner widget
(`widget$run_worker()`) and with the app. Use `inline = TRUE` in tests.

## Mouse

Mouse input is on by default (`app(mouse = FALSE)` turns it off): clicks
press buttons, focus widgets and place the input cursor; the wheel scrolls;
`:hover` styles follow the pointer.

## Developer tools

* `inspect_widget(app, "#id")`: type, region, states, state, computed
  style, matching stylesheet rules, bindings.
* `app$log_events()` / `app$event_log()`: record dispatched events.
* `app(debug = TRUE)`: F12 shows an overlay with the focused and hovered
  widget, the last event and repaint statistics.
* `app$frame_stats`: full and incremental repaints.

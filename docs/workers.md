# Background work

Long computations must not block the interface. termr runs them in separate
processes and reports back through events and callbacks. This API is
**experimental**.

## R functions: `app$run_worker()`

```r
app$run_worker(
  function(n) {
    for (i in seq_len(n)) {
      cat("step", i, "\n")                       # stdout -> on_stdout
      message("working")                        # stderr -> on_stderr
      termr_progress(i / n, paste("step", i))   # separate progress channel
      Sys.sleep(0.2)
    }
    "done"
  },
  args = list(n = 20),
  timeout = 60,
  on_progress = function(value, message, app) ...,
  on_stdout   = function(line, app) ...,
  on_stderr   = function(line, app) ...,
  on_complete = function(result, app) ...,
  on_error    = function(message, app) ...
)
```

The function runs in a fresh `Rscript --vanilla` session: pass data through
`args`, packages through `packages`. The result travels in an RDS file and
progress in an append-only file, so the job's own output is never parsed as
protocol. Output lines have control characters removed; at most 200 lines are
delivered per tick; the last 1,000 are kept in `worker$stdout` / `$stderr`.

## Programs: `app$run_process()`

```r
app$run_process("Rscript", c("-e", "cat('hello\n')"),
                on_stdout = function(line, app) ..., on_exit = function(status, app) ...)
```

Command and arguments are separate; there is no shell, so nothing is
interpolated (use `run_process("sh", c("-c", script))` if you really want one).
A non-zero exit status is a failure.

## Lifecycle

A worker (`Worker`) has `state` (`"running"`, `"completed"`, `"failed"`,
`"cancelled"`), `result`, `error`, `timed_out` and `cancel()`. Cancelling
interrupts first (Unix) and kills the process tree after a short grace period.
Workers end when cancelled, when their owner widget is removed
(`widget$run_worker()`), after `timeout`, and when the app exits. Temporary
files have unique names and are removed however the worker ends.

Events (bubbling from the owner widget or the screen, `event$data$worker`):
`worker.started`, `worker.progress`, `worker.stdout`, `worker.stderr`,
`worker.completed`, `worker.failed` (`timed_out`, `status`), `worker.cancelled`.

## `process_view()`

A ready-made widget: status line plus scrolling log for one program;
Ctrl+X cancels, Ctrl+R restarts, `process.finished` is sent at the end.

## Testing

`inline = TRUE` runs a function in the current process (deterministic);
`pilot$run_worker(fn)`, `pilot$wait_for_workers()` and `pilot$wait_for()` handle
real processes. There is no worker pool: start as many workers as you need.

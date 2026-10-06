# Driver for Windows consoles (conhost and Windows Terminal).
#
# Base R cannot read individual key presses from a Windows console, so a
# PowerShell helper (inst/helpers/termr-input.ps1) shares R's console,
# reads key events with Console.ReadKey and reports them, together with
# console size changes, as lines on a pipe that R polls with a timeout.
# The helper also lets Ctrl+C arrive as a key and enables ANSI processing
# in legacy consoles; it restores both when it exits.

WindowsDriver <- R6::R6Class(
  "WindowsDriver",
  inherit = TerminalDriver,
  public = list(
    startup_timeout = 15,

    initialize = function(color_mode = detect_color_mode()) {
      self$color_mode <- color_mode
      self$capabilities <- terminal_capabilities(overrides = list(colors = color_mode, truecolor = identical(color_mode, "truecolor")))
    },

    start = function() {
      if (self$started) return(invisible(self))
      script <- system.file("helpers", "termr-input.ps1", package = "termr")
      if (!nzchar(script)) stop("The termr input helper script is missing.", call. = FALSE)
      private$stop_file <- tempfile("termr-stop-")
      private$helper <- process$new(
        "powershell.exe",
        c("-NoLogo", "-NoProfile", "-ExecutionPolicy", "Bypass", "-File", script,
          "-StopFile", private$stop_file, "-ParentPid", Sys.getpid(),
          "-Mouse", if (isTRUE(self$mouse)) 1L else 0L),
        stdin = "", stdout = "|", stderr = "|", cleanup = TRUE
      )
      self$started <- TRUE
      private$wait_until_ready()
      self$write(self$setup_sequence())
      invisible(self)
    },

    stop = function() {
      if (!self$started) return(invisible(self))
      self$started <- FALSE
      # Restore the screen while the console still processes ANSI sequences,
      # then let the helper restore the console modes and exit.
      try(self$write(self$teardown_sequence()), silent = TRUE)
      helper <- private$helper
      private$helper <- NULL
      if (!is.null(helper)) {
        try(file.create(private$stop_file), silent = TRUE)
        try(helper$wait(2000), silent = TRUE)
        try(if (helper$is_alive()) helper$kill(), silent = TRUE)
      }
      try(unlink(private$stop_file), silent = TRUE)
      invisible(self)
    },

    write = function(text) write_stdout(text),

    size = function() private$last_size,

    read_events = function(timeout = 0) {
      helper <- private$helper
      wait_ms <- as.integer(round(min(timeout, 1) * 1000))
      status <- helper$poll_io(wait_ms)
      events <- list()
      if (status[["output"]] == "ready") {
        for (line in helper$read_output_lines()) {
          ev <- private$parse_line(line)
          if (inherits(ev, "Event")) ev <- list(ev)
          events <- c(events, ev)
        }
      }
      if (!helper$is_alive() && status[["output"]] != "ready") {
        stop("The termr input helper stopped unexpectedly. ", private$helper_error(), call. = FALSE)
      }
      events
    }
  ),
  private = list(
    helper = NULL,
    stop_file = NULL,
    last_size = c(width = 80L, height = 24L),
    high_surrogate = NULL,
    buttons = 0L,

    wait_until_ready = function() {
      deadline <- now_seconds() + self$startup_timeout
      while (now_seconds() < deadline) {
        status <- private$helper$poll_io(200L)
        if (status[["output"]] == "ready") {
          for (line in private$helper$read_output_lines()) {
            private$parse_line(line)
            if (startsWith(line, "S\t")) return(invisible())
          }
        }
        if (!private$helper$is_alive()) break
      }
      stop("Could not start the termr input helper. ", private$helper_error(),
           "\ntermr needs an interactive console (cmd, PowerShell or Windows Terminal).",
           call. = FALSE)
    },

    helper_error = function() {
      err <- tryCatch(private$helper$read_error(), error = function(e) "")
      if (nzchar(err)) paste("Details:", trimws(err)) else ""
    },

    parse_line = function(line) {
      fields <- strsplit(line, "\t", fixed = TRUE)[[1]]
      switch(
        fields[[1]],
        S = {
          size <- c(width = as.integer(fields[[2]]), height = as.integer(fields[[3]]))
          changed <- !identical(size, private$last_size)
          private$last_size <- size
          if (changed && self$started) ResizeEvent$new(size[["width"]], size[["height"]])
        },
        K = {
          code <- as.integer(fields[[3]])
          # Characters outside the BMP arrive as UTF-16 surrogate pairs.
          if (code >= 0xD800L && code <= 0xDBFFL) {
            private$high_surrogate <- code
            return(NULL)
          }
          if (code >= 0xDC00L && code <= 0xDFFFL && !is.null(private$high_surrogate)) {
            code <- 0x10000L + (private$high_surrogate - 0xD800L) * 1024L + (code - 0xDC00L)
            private$high_surrogate <- NULL
          }
          windows_key_event(as.integer(fields[[2]]), code, as.integer(fields[[4]]))
        },
        M = {
          v <- as.integer(fields[2:6])
          events <- windows_mouse_events(v[[1]], v[[2]], v[[3]], v[[4]], v[[5]], private$buttons)
          if (bitwAnd(v[[4]], 12L) == 0L) private$buttons <- bitwAnd(v[[3]], 0xFFFFL)
          events
        },
        E = stop("termr input helper: ", paste(fields[-1], collapse = " "), call. = FALSE),
        NULL
      )
    }
  )
)

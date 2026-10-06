# Command palette.
#
# Ctrl+P opens a modal search box listing the app's commands: commands added
# with app$add_command(), described key bindings of the focused widget, its
# ancestors and the app, and the app's actions. Typing filters the list
# (prefix matches first, then substrings, then subsequences); Enter runs the
# highlighted command after closing the palette, in the context of the
# widget that had focus.

# Rank `labels` against `query`: indices of matches, best first.
filter_commands <- function(labels, query) {
  if (!nzchar(query)) return(seq_along(labels))
  q <- tolower(query)
  l <- tolower(labels)
  prefix <- startsWith(l, q)
  substring_match <- !prefix & grepl(q, l, fixed = TRUE)
  pattern <- paste(vapply(strsplit(q, "")[[1]], function(ch) {
    if (grepl("[[:alnum:]]", ch)) ch else paste0("[", ch, "]")
  }, ""), collapse = ".*")
  subsequence <- !prefix & !substring_match & grepl(pattern, l, perl = TRUE)
  c(which(prefix), which(substring_match), which(subsequence))
}

CommandPalette <- R6::R6Class(
  "CommandPalette",
  inherit = ModalScreen,
  public = list(
    commands = list(),

    initialize = function(commands) {
      self$commands <- commands
      search <- Input$new(placeholder = "Search commands...", id = "palette-search",
                          style = style(border = "none", padding = c(0, 1)))
      results <- OptionList$new(private$labels(), id = "palette-list",
                                style = style(height = min(max(1L, length(commands)), 12L), border = "none"))
      results$focusable <- FALSE
      body <- panel(search, rule(), results, title = "Commands", style = style(width = 60, background = "default"))
      super$initialize(body, dim = TRUE)
    },

    # Palettes sit near the top of the screen.
    default_style = function() style(width = "1fr", height = "1fr", align = "center", valign = "top", padding = c(2, 0, 0, 0)),

    default_bindings = function() {
      c(super$default_bindings(), list(
        bind("down", "next_command"), bind("up", "previous_command"),
        bind("pagedown", "next_page"), bind("pageup", "previous_page")
      ))
    },

    action_next_command = function() private$list()$action_list_down(),
    action_previous_command = function() private$list()$action_list_up(),
    action_next_page = function() private$list()$action_list_page_down(),
    action_previous_page = function() private$list()$action_list_page_up(),

    on_input_changed = function(event) {
      event$stop()
      private$.matches <- filter_commands(private$labels(), event$data$value)
      private$list()$set_choices(private$labels()[private$.matches] %||% character())
    },

    on_input_submitted = function(event) {
      event$stop()
      private$run_highlighted()
    },

    on_option_list_selected = function(event) {
      event$stop()
      private$run_highlighted()
    }
  ),
  private = list(
    .matches = NULL,
    list = function() self$query_one("#palette-list"),
    labels = function() vapply(self$commands, function(cmd) {
      paste0(command_text(cmd), if (identical(cmd$available, FALSE)) "  (unavailable)" else "")
    }, character(1)),
    run_highlighted = function() {
      matches <- private$.matches %||% seq_along(self$commands)
      i <- private$list()$cursor
      if (i < 1L || i > length(matches)) return(invisible())
      command <- self$commands[[matches[[i]]]]
      if (identical(command$available, FALSE)) {
        self$app$notify(sprintf("\"%s\" is not available right now.", command$label), severity = "warning")
        return(invisible())
      }
      self$dismiss(command)
    }
  )
)

open_command_palette <- function(app) {
  if (inherits(app$screen, "CommandPalette")) return(invisible())
  commands <- lapply(collect_commands(app), function(cmd) {
    cmd$available <- command_enabled(cmd, app)
    cmd
  })
  app$push_screen(CommandPalette$new(commands), callback = function(command, app) {
    if (is.null(command)) return()
    widget <- command$widget
    if (!is.null(widget) && !identical(widget$app, app)) widget <- NULL
    app$call_later(function(app) app$run_action(command$action, widget))
  })
  invisible()
}

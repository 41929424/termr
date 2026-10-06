#' Commands
#'
#' A command is something the user can do: it has a label, an action, an
#' optional category and keyboard shortcut, and may be enabled or disabled
#' depending on the app state. The command palette (Ctrl+P) and the help
#' screen (F1) are built from the same list: commands added with
#' `app$add_command()`, the described key bindings of the focused widget, its
#' ancestors and the app, and the app's named actions. Existing [bind()]
#' bindings keep working and appear in both.
#'
#' @param label Text shown in the palette and the help screen.
#' @param action An action name or a `function(app)`.
#' @param category Optional group name, e.g. `"File"`.
#' @param shortcut Optional key shown next to the command (documentation
#'   only; use [bind()] to create the binding).
#' @param enabled `NULL` (always) or a `function(app)` returning `TRUE` /
#'   `FALSE`. Disabled commands are dimmed in the palette and cannot run.
#' @param description Optional longer text for the help screen.
#' @return An object of class `termr_command`.
#' @export
#' @examples
#' command("Save", action = "save", category = "File", shortcut = "ctrl+s")
command <- function(label, action, category = NULL, shortcut = NULL, enabled = NULL, description = NULL) {
  check_scalar_character(label, "label")
  if (!is_scalar_character(action) && !is.function(action)) {
    stop("`action` must be an action name or a function.", call. = FALSE)
  }
  check_scalar_character(category, "category", allow_null = TRUE)
  check_scalar_character(shortcut, "shortcut", allow_null = TRUE)
  check_scalar_character(description, "description", allow_null = TRUE)
  check_function(enabled, "enabled", allow_null = TRUE)
  structure(
    list(label = label, action = action, category = category, shortcut = shortcut,
         enabled = enabled, description = description, widget = NULL, source = "command"),
    class = "termr_command"
  )
}

#' @export
format.termr_command <- function(x, ...) {
  paste0("<command ", if (!is.null(x$category)) paste0(x$category, ": "), x$label,
         if (!is.null(x$shortcut)) paste0(" (", x$shortcut, ")"), ">")
}

#' @export
print.termr_command <- function(x, ...) {
  cat(format(x), "\n", sep = "")
  invisible(x)
}

is_command <- function(x) inherits(x, "termr_command")

command_enabled <- function(cmd, app) {
  if (is.null(cmd$enabled)) return(TRUE)
  isTRUE(tryCatch(cmd$enabled(app), error = function(e) FALSE))
}

# The text of a command in the palette: "Category: label  (shortcut)".
command_text <- function(cmd) {
  paste0(if (!is.null(cmd$category)) paste0(cmd$category, ": "), cmd$label,
         if (!is.null(cmd$shortcut)) paste0("  (", cmd$shortcut, ")"))
}

# Every command available in the current context: app commands, then the
# described bindings of the focused widget, its ancestors and the app, then
# the app's named actions. Each entry is a termr_command; `source` says where
# it came from ("command", "widget", "screen", "app", "action").
collect_commands <- function(app) {
  out <- list()
  seen <- character()
  add <- function(cmd) {
    text <- command_text(cmd)
    if (text %in% seen) return()
    seen <<- c(seen, text)
    out[[length(out) + 1L]] <<- cmd
  }
  from_binding <- function(b, node, source) {
    cmd <- command(b$description, b$action, shortcut = b$keys[[1]])
    cmd$widget <- node
    cmd$source <- source
    cmd
  }
  for (cmd in app$.__enclos_env__$private$.commands) add(cmd)
  focused <- app$focused
  chain <- if (is.null(focused)) list(app$screen) else c(list(focused), focused$ancestors())
  for (node in chain) {
    source <- if (inherits(node, "Screen")) "screen" else "widget"
    for (b in rev(node$bindings())) {
      if (!is.null(b$description)) add(from_binding(b, node, source))
    }
  }
  for (b in rev(app$bindings())) {
    if (!is.null(b$description) && !identical(b$action, "command_palette")) {
      add(from_binding(b, NULL, "app"))
    }
  }
  for (name in names(app$.__enclos_env__$private$.actions)) {
    cmd <- command(gsub("_", " ", name), name)
    cmd$source <- "action"
    add(cmd)
  }
  out
}

# Markdown for the help screen, built from the same information.
help_markdown <- function(app) {
  esc <- function(x) gsub("|", "/", gsub("[*_`]", "", x), fixed = TRUE)
  pretty <- function(b) {
    if (!is.null(b$description)) return(b$description)
    if (is.function(b$action)) return("(custom action)")
    gsub("_", " ", b$action)
  }
  table <- function(bindings) {
    if (!length(bindings)) return(character())
    rows <- vapply(bindings, function(b) sprintf("| %s | %s |", esc(paste(b$keys, collapse = ", ")), esc(pretty(b))), "")
    c("| Key | Action |", "|---|---|", unique(rows), "")
  }
  lines <- c("# Help", "")
  focused <- app$focused
  chain <- if (is.null(focused)) list(app$screen) else c(list(focused), focused$ancestors())
  for (node in chain) {
    bindings <- node$bindings()
    help <- node$help
    if (!length(bindings) && is.null(help)) next
    name <- class(node)[[1]]
    id <- if (!is.null(node$id)) paste0(" #", node$id) else ""
    lines <- c(lines, sprintf("## %s%s", name, id), "")
    if (!is.null(help)) lines <- c(lines, help, "")
    lines <- c(lines, table(bindings))
  }
  lines <- c(lines, "## Application", "", table(app$bindings()))
  cmds <- Filter(function(cmd) cmd$source == "command", collect_commands(app))
  if (length(cmds)) {
    cats <- vapply(cmds, function(cmd) cmd$category %||% "General", "")
    lines <- c(lines, "## Commands", "")
    for (cat in unique(cats)) {
      lines <- c(lines, sprintf("### %s", cat), "")
      for (cmd in cmds[cats == cat]) {
        lines <- c(lines, sprintf("- **%s**%s%s%s", esc(cmd$label),
                                  if (!is.null(cmd$shortcut)) paste0(" (", esc(cmd$shortcut), ")") else "",
                                  if (!is.null(cmd$description)) paste0(" - ", esc(cmd$description)) else "",
                                  if (!command_enabled(cmd, app)) " _(unavailable)_" else ""))
      }
      lines <- c(lines, "")
    }
  }
  lines <- c(lines, "Press Escape to close this help.")
  paste(lines, collapse = "\n")
}

open_help <- function(app) {
  if (inherits(app$screen, "HelpScreen")) return(invisible())
  doc <- markdown_view(help_markdown(app), id = "help-doc", style = style(height = "1fr", width = "1fr"))
  screen <- HelpScreen$new(doc)
  app$push_screen(screen)
  invisible(screen)
}

HelpScreen <- R6::R6Class(
  "HelpScreen",
  inherit = ModalScreen,
  public = list(
    initialize = function(doc) {
      body <- panel(doc, title = "Help (Esc to close)", style = style(width = "80%", height = "80%", max_width = 100,
                                                                      background = "default"))
      super$initialize(body, dim = TRUE)
    }
  )
)

#' @title Input widget
#' @description A single-line text input. See [input()].
#' @rdname Input-class
#' @export
Input <- R6::R6Class(
  "Input",
  inherit = Widget,
  public = list(
    #' @field paint_states Cursor and selection changes only repaint.
    paint_states = c("cursor", "anchor"),
    #' @field focusable Inputs can be focused.
    focusable = TRUE,
    #' @field placeholder Text shown while the input is empty.
    placeholder = "",
    #' @field password Show bullets instead of the characters?
    password = FALSE,
    #' @field max_length Maximum number of characters, or `NULL`.
    max_length = NULL,
    #' @field validate `NULL` or `function(value)` returning `NULL` (valid)
    #'   or an error message.
    validate = NULL,

    #' @description Create an input.
    #' @param value Initial text.
    #' @param placeholder Placeholder text.
    #' @param id,classes,style,disabled See [Widget].
    #' @param password Mask the characters?
    #' @param max_length Maximum number of characters.
    #' @param validate Validation function.
    initialize = function(value = "", placeholder = "", id = NULL, classes = NULL, style = NULL,
                          disabled = FALSE, password = FALSE, max_length = NULL, validate = NULL) {
      super$initialize(id = id, classes = classes, style = style, disabled = disabled)
      check_scalar_character(placeholder, "placeholder")
      check_flag(password)
      check_function(validate, "validate", allow_null = TRUE)
      if (!is.null(max_length)) max_length <- check_count(max_length, "max_length")
      self$placeholder <- placeholder
      self$password <- password
      self$max_length <- max_length
      self$validate <- validate
      value <- clean_input_value(value, max_length)
      private$.state$value <- value
      private$.state$cursor <- length(input_chars(value))
      private$.state$anchor <- NA_integer_
      private$.state$error <- private$check(value)
    },

    #' @description The built-in style.
    default_style = function() {
      style(
        width = "1fr", height = "auto", border = "round", padding = c(0, 1),
        border_color = "$muted",
        focus = style(border_color = "$accent"),
        disabled = style(foreground = "$muted"),
        states = list(invalid = style(border_color = "$error"))
      )
    },

    #' @description Active states: focus, hover, disabled and invalid.
    pseudo_states = function() c(super$pseudo_states(), if (!is.null(private$.state$error)) "invalid"),

    #' @description Editing key bindings.
    default_bindings = function() {
      list(
        bind("left", "cursor_left"), bind("right", "cursor_right"),
        bind("home", "cursor_home"), bind("end,ctrl+e", "cursor_end"),
        bind("ctrl+left,alt+b", "word_left"), bind("ctrl+right,alt+f", "word_right"),
        bind("shift+left", "select_left"), bind("shift+right", "select_right"),
        bind("shift+home", "select_home"), bind("shift+end", "select_end"),
        bind("ctrl+shift+left", "select_word_left"), bind("ctrl+shift+right", "select_word_right"),
        bind("ctrl+a", "select_all"),
        bind("backspace", "delete_left"), bind("delete,ctrl+d", "delete_right"),
        bind("ctrl+backspace,alt+backspace,ctrl+w", "delete_word_left"),
        bind("ctrl+delete,alt+d", "delete_word_right"),
        bind("ctrl+u", "delete_to_start"), bind("ctrl+k", "delete_to_end"),
        bind("ctrl+v", "paste"),
        bind("enter", "submit", "Submit")
      )
    },

    #' @description Printable keys replace the selection or are inserted at
    #'   the cursor. Ctrl+C / Ctrl+X copy / cut when text is selected (otherwise
    #'   Ctrl+C keeps its app meaning).
    #' @param event A `KeyEvent`.
    on_key = function(event) {
      if (!self$is_enabled()) return(invisible())
      if (event$is_printable()) {
        self$insert(event$char)
        event$stop()
      } else if (event$key %in% c("ctrl+c", "ctrl+x") && length(private$selected_indices())) {
        self$copy()
        if (event$key == "ctrl+x") private$delete_selection()
        event$stop()
      }
    },

    #' @description Clicking places the cursor; shift+click and dragging
    #'   select.
    #' @param event A `MouseEvent`.
    on_mouse_down = function(event) {
      if (event$button != "left" || is.null(self$region)) return(invisible())
      pos <- private$position_at(event$screen_x)
      if (event$shift) {
        if (is.na(private$.state$anchor)) self$set_state("anchor", private$.state$cursor)
        self$set_state("cursor", pos)
      } else {
        self$set_state("anchor", pos)
        self$set_state("cursor", pos)
      }
      event$stop()
    },

    #' @description Dragging with the left button extends the selection.
    #' @param event A `MouseEvent`.
    on_mouse_move = function(event) {
      if (event$button == "left" && !is.null(self$region) && !is.na(private$.state$anchor)) {
        self$set_state("cursor", private$position_at(event$screen_x))
        event$stop()
      }
    },

    #' @description Pasted text is inserted as one edit. An Input is a
    #'   single line, so line breaks in the paste become spaces.
    #' @param event A `PasteEvent`.
    on_paste = function(event) {
      if (!self$is_enabled()) return(invisible())
      self$insert(gsub("[[:space:]]*\n[[:space:]]*", " ", event$text))
      event$stop()
    },

    #' @description Insert text at the cursor (replacing the selection).
    #' @param text A string.
    insert = function(text) {
      text <- clean_input_value(text)
      if (!nzchar(text)) return(invisible(self))
      private$delete_selection(notify = FALSE)
      chars <- input_chars(private$.state$value)
      cursor <- private$.state$cursor
      before <- paste(chars[seq_len(cursor)], collapse = "")
      after <- paste(chars[seq_len(length(chars) - cursor) + cursor], collapse = "")
      # Inserted text can merge with its neighbours into one grapheme
      # (e.g. a ZWJ followed by an emoji), so positions are recomputed from
      # the joined string rather than counted.
      if (!is.null(self$max_length) &&
          length(input_chars(paste0(before, text, after))) > self$max_length) {
        room <- max(0L, self$max_length - length(chars))
        text <- paste(input_chars(text)[seq_len(room)], collapse = "")
        if (!nzchar(text)) return(invisible(self))
      }
      head <- input_chars(paste0(before, text))
      private$edit(input_chars(paste0(before, text, after)), length(head))
    },

    #' @description Remove all text.
    clear = function() {
      self$value <- ""
      invisible(self)
    },

    #' @description Select a range of characters.
    #' @param start,end Positions (0 = before the first character).
    select_range = function(start, end) {
      n <- length(input_chars(private$.state$value))
      self$set_state("anchor", as.integer(min(max(0L, start), n)))
      self$set_state("cursor", as.integer(min(max(0L, end), n)))
      invisible(self)
    },

    #' @description Copy the selection to the app clipboard (and, when
    #'   `system = TRUE` and supported, to the system clipboard via OSC 52).
    #' @param system Also write to the system clipboard?
    copy = function(system = TRUE) {
      sel <- private$selected_indices()
      app <- self$app
      # Password fields never copy their value to a clipboard.
      if (length(sel) && !is.null(app) && !isTRUE(self$password)) {
        chars <- input_chars(private$.state$value)
        app$clipboard_write(paste(chars[sel], collapse = ""), system = system)
      }
      invisible(self)
    },

    #' @description Move one character left.
    action_cursor_left = function() private$move(private$.state$cursor - 1L, extend = FALSE, collapse_to = "start"),
    #' @description Move one character right.
    action_cursor_right = function() private$move(private$.state$cursor + 1L, extend = FALSE, collapse_to = "end"),
    #' @description Move to the start.
    action_cursor_home = function() private$move(0L, extend = FALSE),
    #' @description Move to the end.
    action_cursor_end = function() private$move(private$length(), extend = FALSE),
    #' @description Move to the previous word.
    action_word_left = function() private$move(private$word_left(), extend = FALSE),
    #' @description Move to the next word.
    action_word_right = function() private$move(private$word_right(), extend = FALSE),
    #' @description Extend the selection left.
    action_select_left = function() private$move(private$.state$cursor - 1L, extend = TRUE),
    #' @description Extend the selection right.
    action_select_right = function() private$move(private$.state$cursor + 1L, extend = TRUE),
    #' @description Select to the start.
    action_select_home = function() private$move(0L, extend = TRUE),
    #' @description Select to the end.
    action_select_end = function() private$move(private$length(), extend = TRUE),
    #' @description Select to the previous word.
    action_select_word_left = function() private$move(private$word_left(), extend = TRUE),
    #' @description Select to the next word.
    action_select_word_right = function() private$move(private$word_right(), extend = TRUE),
    #' @description Select everything.
    action_select_all = function() self$select_range(0L, private$length()),

    #' @description Delete the selection or the character before the cursor.
    action_delete_left = function() {
      if (private$delete_selection()) return(invisible(self))
      cursor <- private$.state$cursor
      if (cursor == 0L) return(invisible(self))
      chars <- input_chars(private$.state$value)
      private$edit(chars[-cursor], cursor - 1L)
    },

    #' @description Delete the selection or the character at the cursor.
    action_delete_right = function() {
      if (private$delete_selection()) return(invisible(self))
      cursor <- private$.state$cursor
      chars <- input_chars(private$.state$value)
      if (cursor >= length(chars)) return(invisible(self))
      private$edit(chars[-(cursor + 1L)], cursor)
    },

    #' @description Delete the word before the cursor.
    action_delete_word_left = function() {
      if (private$delete_selection()) return(invisible(self))
      private$delete_between(private$word_left(), private$.state$cursor)
    },

    #' @description Delete the word after the cursor.
    action_delete_word_right = function() {
      if (private$delete_selection()) return(invisible(self))
      private$delete_between(private$.state$cursor, private$word_right())
    },

    #' @description Delete everything before the cursor.
    action_delete_to_start = function() private$delete_between(0L, private$.state$cursor),

    #' @description Delete everything after the cursor.
    action_delete_to_end = function() private$delete_between(private$.state$cursor, private$length()),

    #' @description Insert the app clipboard.
    action_paste = function() {
      app <- self$app
      if (!is.null(app) && nzchar(app$clipboard)) self$insert(app$clipboard)
    },

    #' @description Send an `"input.submitted"` message (Enter).
    action_submit = function() {
      self$post_message("input.submitted", list(value = private$.state$value, valid = self$valid))
    },

    #' @description Natural content width.
    content_width = function() {
      max(str_width(private$.state$value), str_width(self$placeholder)) + 1L
    },

    #' @description The visible part of the text, scrolled so the cursor
    #'   is visible, with the cursor drawn in reverse video and the
    #'   selection highlighted when focused.
    #' @param width Content width.
    render_lines = function(width = NA_integer_) {
      width <- if (is.na(width)) .Machine$integer.max else max(1L, width)
      chars <- input_chars(private$.state$value)
      if (self$password) chars <- rep("\u2022", length(chars))
      show_cursor <- self$focused && self$is_enabled()
      if (length(chars) == 0L) return(private$placeholder_lines(show_cursor))
      widths <- pmax(grapheme_width(chars), 1L)
      cursor <- private$.state$cursor
      cursor_width <- if (cursor < length(chars)) widths[[cursor + 1L]] else 1L
      scroll <- min(private$.scroll, cursor)
      used <- function(from) sum(widths[seq_len(cursor - from) + from]) + cursor_width
      while (scroll < cursor && used(scroll) > width) scroll <- scroll + 1L
      # Scroll back when the text from an earlier position fits entirely.
      tail_width <- function(from) sum(widths[seq.int(from + 1L, length.out = length(chars) - from)]) +
        (cursor == length(chars))
      while (scroll > 0L && tail_width(scroll - 1L) <= width) scroll <- scroll - 1L
      private$.scroll <- scroll

      visible <- seq.int(scroll + 1L, length.out = length(chars) - scroll)
      visible <- visible[cumsum(widths[visible]) <= width]
      if (!show_cursor) return(text_lines(paste(chars[visible], collapse = "")))
      selected <- private$selected_indices()
      kind <- ifelse(visible == cursor + 1L, "cursor", ifelse(visible %in% selected, "selected", "plain"))
      pieces <- chars[visible]
      if (cursor == length(chars)) {
        pieces <- c(pieces, " ")
        kind <- c(kind, "cursor")
      }
      styles <- list(
        plain = NULL,
        selected = style(background = "$primary", foreground = "$on_primary"),
        cursor = style(reverse = TRUE)
      )
      groups <- cumsum(c(TRUE, kind[-1L] != kind[-length(kind)]))
      segs <- lapply(split(seq_along(pieces), groups), function(i) {
        span(paste(pieces[i], collapse = ""), styles[[kind[[i[[1]]]]]])
      })
      text_lines(do.call(c, unname(segs)))
    }
  ),
  active = list(
    #' @field value The text (reactive). Assigning moves the cursor to the
    #'   end and sends `"input.changed"`.
    value = function(value) {
      if (missing(value)) return(private$.state$value)
      value <- clean_input_value(value, self$max_length)
      private$edit(input_chars(value), length(input_chars(value)))
    },
    #' @field cursor_position Cursor position in characters (0 = start).
    cursor_position = function(value) {
      if (missing(value)) return(private$.state$cursor)
      private$move(value, extend = FALSE)
    },
    #' @field selection The selected text (`""` if none).
    selection = function(value) {
      if (!missing(value)) read_only("selection")
      sel <- private$selected_indices()
      paste(input_chars(private$.state$value)[sel], collapse = "")
    },
    #' @field valid Does the value pass `validate`?
    valid = function(value) if (missing(value)) is.null(private$.state$error) else read_only("valid"),
    #' @field error The validation message, or `NULL`.
    error = function(value) if (missing(value)) private$.state$error else read_only("error")
  ),
  private = list(
    .scroll = 0L,

    length = function() length(input_chars(private$.state$value)),

    check = function(value) run_validator(self$validate, value),

    # Indices (1-based) of the selected characters.
    selected_indices = function() {
      anchor <- private$.state$anchor
      cursor <- private$.state$cursor
      if (is.na(anchor) || anchor == cursor) return(integer())
      seq.int(min(anchor, cursor) + 1L, max(anchor, cursor))
    },

    move = function(position, extend, collapse_to = NULL) {
      n <- private$length()
      sel <- private$selected_indices()
      if (!extend && length(sel) && !is.null(collapse_to)) {
        # Left/Right with a selection go to its start / end.
        position <- if (collapse_to == "start") min(sel) - 1L else max(sel)
      }
      position <- as.integer(min(max(0L, position), n))
      if (extend) {
        if (is.na(private$.state$anchor)) self$set_state("anchor", private$.state$cursor)
      } else {
        self$set_state("anchor", NA_integer_)
      }
      self$set_state("cursor", position)
      invisible(self)
    },

    word_left = function() {
      chars <- input_chars(private$.state$value)
      i <- private$.state$cursor
      while (i > 0L && grepl("^[[:space:][:punct:]]$", chars[[i]])) i <- i - 1L
      while (i > 0L && !grepl("^[[:space:][:punct:]]$", chars[[i]])) i <- i - 1L
      i
    },

    word_right = function() {
      chars <- input_chars(private$.state$value)
      n <- length(chars)
      i <- private$.state$cursor
      while (i < n && grepl("^[[:space:][:punct:]]$", chars[[i + 1L]])) i <- i + 1L
      while (i < n && !grepl("^[[:space:][:punct:]]$", chars[[i + 1L]])) i <- i + 1L
      i
    },

    delete_between = function(from, to) {
      if (to <= from) return(invisible(self))
      chars <- input_chars(private$.state$value)
      private$edit(chars[-seq.int(from + 1L, to)], from)
    },

    delete_selection = function(notify = TRUE) {
      sel <- private$selected_indices()
      if (!length(sel)) return(FALSE)
      chars <- input_chars(private$.state$value)
      start <- min(sel) - 1L
      if (notify) {
        private$edit(chars[-sel], start)
      } else {
        private$.state$value <- paste(chars[-sel], collapse = "")
        private$.state$cursor <- start
        private$.state$anchor <- NA_integer_
      }
      TRUE
    },

    position_at = function(screen_x) {
      inner <- content_rect(self$region, self$computed_style())
      column <- screen_x - inner$x
      chars <- input_chars(private$.state$value)
      scroll <- min(private$.scroll, length(chars))
      rest <- seq.int(scroll + 1L, length.out = length(chars) - scroll)
      widths <- pmax(grapheme_width(chars[rest]), 1L)
      as.integer(scroll + sum(cumsum(widths) <= column))
    },

    edit = function(chars, cursor) {
      value <- paste(chars, collapse = "")
      old <- private$.state$value
      self$set_state("anchor", NA_integer_)
      self$set_state("value", value)
      self$set_state("cursor", as.integer(cursor))
      if (!identical(old, value)) {
        was_valid <- is.null(private$.state$error)
        self$set_state("error", private$check(value))
        self$post_message("input.changed", list(value = value, valid = self$valid))
        if (!identical(was_valid, self$valid)) {
          self$post_message(if (self$valid) "input.valid" else "input.invalid",
                            list(value = value, error = private$.state$error))
        }
      }
      invisible(self)
    },

    placeholder_lines = function(show_cursor) {
      hint <- style(foreground = "$muted")
      if (!show_cursor) return(text_lines(span(self$placeholder, hint)))
      cells <- text_cells(self$placeholder)
      first <- if (length(cells$chars)) cells$chars[[1]] else " "
      rest <- paste(cells$chars[-1L], collapse = "")
      text_lines(c(span(first, style(reverse = TRUE)), span(rest, hint)))
    },

    describe = function() {
      # print(), the event log and the debug overlay must not reveal a password.
      if (isTRUE(self$password)) return(if (nzchar(private$.state$value)) "<hidden>" else "\"\"")
      encodeString(private$.state$value, quote = "\"")
    }
  )
)

#' Single-line text input
#'
#' A focusable text field.
#'
#' Keys: typing inserts at the cursor (replacing a selection);
#' Left/Right/Home/End move (Ctrl+E: end); Ctrl+Left/Right move by word;
#' Shift (+Ctrl) with those keys selects; Ctrl+A selects all;
#' Backspace/Delete delete a character or the selection;
#' Ctrl+Backspace / Alt+Backspace / Ctrl+W and Ctrl+Delete / Alt+D delete a
#' word; Ctrl+U / Ctrl+K delete to the start / end; Ctrl+C, Ctrl+X and
#' Ctrl+V copy, cut and paste with the app clipboard (`app$clipboard`;
#' Ctrl+C without a selection keeps its app meaning, quit by default).
#' The mouse places the cursor; shift+click and dragging select.
#'
#' Messages (`event$data`): `"input.changed"` (`value`, `valid`) on every
#' change, `"input.submitted"` (`value`, `valid`) on Enter, and
#' `"input.valid"` / `"input.invalid"` (`value`, `error`) when the result of
#' `validate` changes. Invalid inputs have the `:invalid` state (red
#' border by default).
#'
#' @param value Initial text.
#' @param placeholder Text shown while the input is empty.
#' @param id Optional identifier (for `#id` selectors).
#' @param classes Optional classes (for `.class` selectors).
#' @param style A [style()].
#' @param disabled Disabled inputs cannot be focused or edited.
#' @param password Show bullets instead of the typed characters.
#' @param max_length Maximum number of characters.
#' @param validate Optional `function(value)` returning `NULL` (or `TRUE`)
#'   when the value is valid, or an error message.
#' @return An `Input` widget. Read or set the text with `$value`; see also
#'   `$valid`, `$error`, `$selection`, `$select_range()`.
#' @export
#' @examples
#' name <- input(placeholder = "Your name", id = "name",
#'               validate = function(x) if (!nzchar(x)) "Required")
#' name$valid
#' name$value <- "Ada"
#' name$valid
input <- function(value = "", placeholder = "", id = NULL, classes = NULL, style = NULL,
                  disabled = FALSE, password = FALSE, max_length = NULL, validate = NULL) {
  Input$new(value, placeholder = placeholder, id = id, classes = classes, style = style,
            disabled = disabled, password = password, max_length = max_length, validate = validate)
}

# Characters of an input value (grapheme clusters, including invisible
# ones so that sequences typed piece by piece can merge).
input_chars <- function(value) split_graphemes(sanitize_text(value))

clean_input_value <- function(value, max_length = NULL) {
  if (is.null(value) || length(value) == 0L) return("")
  if (!is.character(value) && !is.numeric(value)) {
    stop("An input value must be a string.", call. = FALSE)
  }
  value <- sanitize_text(gsub("[\r\n]+", " ", paste(format_value(value), collapse = " ")))
  if (!is.null(max_length)) {
    chars <- input_chars(value)
    if (length(chars) > max_length) value <- paste(chars[seq_len(max_length)], collapse = "")
  }
  value
}

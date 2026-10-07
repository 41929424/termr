#' @title TextArea widget
#' @description A multi-line text editor. See [text_area()].
#' @rdname TextArea-class
#' @export
TextArea <- R6::R6Class(
  "TextArea",
  inherit = Widget,
  public = list(
    #' @field focusable Text areas can be focused.
    focusable = TRUE,
    #' @field placeholder Text shown while the area is empty.
    placeholder = "",
    #' @field line_numbers Show a line number gutter?
    line_numbers = FALSE,
    #' @field wrap Wrap long lines (`TRUE`) or scroll horizontally?
    wrap = TRUE,
    #' @field read_only Block editing (cursor, selection, copy and search
    #'   still work)?
    read_only = FALSE,
    #' @field language Optional language name passed to the configured highlighter.
    language = NULL,
    #' @field tab_size Spaces inserted by Tab (and used for tabs in loaded
    #'   text).
    tab_size = 4L,
    #' @field tab_behavior `"focus"`: Tab moves focus (default, safe in
    #'   forms); `"indent"`: Tab inserts spaces and Shift+Tab removes them.
    tab_behavior = "focus",
    #' @field auto_indent Copy the indentation of the current line on Enter?
    auto_indent = FALSE,
    #' @field highlighter Optional function `(lines, state)` returning a list
    #'   of per-line data frames with 1-based inclusive `start`, `end`, `token`.
    highlighter = NULL,
    #' @field validate `NULL` or `function(value)` returning `NULL` (valid)
    #'   or an error message.
    validate = NULL,

    #' @description Create a text area.
    #' @param value Initial text.
    #' @param placeholder Placeholder text.
    #' @param id,classes,style,disabled See [Widget].
    #' @param line_numbers,wrap,read_only,language,tab_size,tab_behavior,auto_indent
    #'   See the fields.
    #' @param validate Validation function.
    #' @param max_history Maximum number of undo steps.
    initialize = function(value = "", placeholder = "", id = NULL, classes = NULL, style = NULL,
                          disabled = FALSE, line_numbers = FALSE, wrap = TRUE, read_only = FALSE,
                          language = NULL, tab_size = 4L, tab_behavior = "focus",
                          auto_indent = FALSE, validate = NULL, max_history = 200L, highlighter = NULL) {
      super$initialize(id = id, classes = classes, style = style, disabled = disabled)
      check_scalar_character(placeholder, "placeholder")
      check_flag(line_numbers)
      check_flag(wrap)
      check_flag(read_only)
      check_flag(auto_indent)
      check_function(validate, "validate", allow_null = TRUE)
      check_function(highlighter, "highlighter", allow_null = TRUE)
      self$tab_behavior <- check_choice(tab_behavior, c("focus", "indent"), "tab_behavior")
      self$tab_size <- max(1L, check_count(tab_size, "tab_size"))
      self$placeholder <- placeholder
      self$line_numbers <- line_numbers
      self$wrap <- wrap
      self$read_only <- read_only
      self$language <- language
      self$auto_indent <- auto_indent
      self$highlighter <- highlighter
      self$validate <- validate
      private$.buf <- TextBuffer$new(value, self$tab_size)
      private$.highlight_from <- 1L
      private$.undo <- UndoStack$new(max_entries = check_count(max_history, "max_history"))
      private$.wrap_cache <- new.env(parent = emptyenv())
      private$.highlight_cache <- new.env(parent = emptyenv())
      initial <- list(
        cursor_row = 1L, cursor_col = 0L, anchor_row = NA_integer_, anchor_col = NA_integer_,
        top = 1L, sub = 0L, left = 0L
      )
      for (name in names(initial)) private$.state[[name]] <- initial[[name]]
      private$.state$error <- run_validator(validate, private$.buf$text())
    },

    #' @description The built-in style.
    default_style = function() {
      style(
        width = "1fr", height = "1fr", border = "round", padding = c(0, 0),
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
      c(list(
        bind("left", "cursor_left"), bind("right", "cursor_right"),
        bind("up", "cursor_up"), bind("down", "cursor_down"),
        bind("home", "line_start"), bind("end,ctrl+e", "line_end"),
        bind("ctrl+home", "document_start"), bind("ctrl+end", "document_end"),
        bind("ctrl+left,alt+b", "word_left"), bind("ctrl+right,alt+f", "word_right"),
        bind("pageup", "page_up"), bind("pagedown", "page_down"),
        bind("shift+left", "select_left"), bind("shift+right", "select_right"),
        bind("shift+up", "select_up"), bind("shift+down", "select_down"),
        bind("shift+home", "select_line_start"), bind("shift+end", "select_line_end"),
        bind("ctrl+shift+home", "select_document_start"), bind("ctrl+shift+end", "select_document_end"),
        bind("ctrl+shift+left", "select_word_left"), bind("ctrl+shift+right", "select_word_right"),
        bind("shift+pageup", "select_page_up"), bind("shift+pagedown", "select_page_down"),
        bind("ctrl+a", "select_all"),
        bind("enter", "newline"),
        bind("backspace", "delete_left"), bind("delete,ctrl+d", "delete_right"),
        bind("ctrl+backspace,alt+backspace,ctrl+w", "delete_word_left"),
        bind("ctrl+delete,alt+d", "delete_word_right"),
        bind("ctrl+u", "delete_to_line_start"), bind("ctrl+k", "delete_to_line_end"),
        bind("ctrl+v", "paste"),
        bind("ctrl+z", "undo", "Undo"), bind("ctrl+y,ctrl+shift+z", "redo", "Redo"),
        bind("ctrl+f", "open_find", "Find"), bind("ctrl+h", "open_replace", "Replace"),
        bind("ctrl+g", "open_goto", "Go to line"),
        bind("f3", "find_next"), bind("shift+f3", "find_previous")
      ), if (self$tab_behavior == "indent") list(bind("tab", "indent"), bind("shift+tab", "dedent")))
    },

    # Keys ------------------------------------------------------------------

    #' @description Printable keys replace the selection or are inserted at
    #'   the cursor; Ctrl+C / Ctrl+X copy / cut a selection. While the find
    #'   bar is open, keys edit the search text.
    #' @param event A `KeyEvent`.
    on_key = function(event) {
      if (!self$is_enabled()) return(invisible())
      if (private$.goto_open) return(private$goto_key(event))
      if (private$.find_open) return(private$find_key(event))
      if (event$is_printable()) {
        self$insert(event$char)
        event$stop()
      } else if (event$key %in% c("ctrl+c", "ctrl+x") && !is.null(private$selection_range())) {
        self$copy()
        if (event$key == "ctrl+x" && !self$read_only) private$delete_selection()
        event$stop()
      }
    },

    #' @description Pasted text is inserted as one undo step; line breaks
    #'   are kept.
    #' @param event A `PasteEvent`.
    on_paste = function(event) {
      if (!self$is_enabled()) return(invisible())
      if (private$.find_open && !(private$.replace_open && private$.replace_target == "replacement")) {
        private$find_set(paste0(private$.find_text, gsub("\n", " ", event$text, fixed = TRUE)))
      } else if (private$.goto_open) {
        private$.goto_text <- paste0(private$.goto_text, gsub("[^0-9]", "", event$text))
        self$invalidate_paint()
      } else if (private$.replace_open && identical(private$.replace_target, "replacement")) {
        private$.replace_text <- paste0(private$.replace_text, event$text)
        self$invalidate_paint()
      } else {
        self$insert(event$text, kind = "paste")
      }
      event$stop()
    },

    #' @description Click to place the cursor, shift+click and drag to
    #'   select.
    #' @param event A `MouseEvent`.
    on_mouse_down = function(event) {
      if (event$button != "left" || is.null(self$region)) return(invisible())
      pos <- private$position_at(event$screen_x, event$screen_y)
      if (is.null(pos)) return(invisible())
      private$.undo$break_group()
      private$set_cursor(pos, extend = event$shift || FALSE)
      private$.dragging <- TRUE
      event$stop()
    },

    #' @description Dragging with the left button extends the selection.
    #' @param event A `MouseEvent`.
    on_mouse_move = function(event) {
      if (event$button == "left" && isTRUE(private$.dragging) && !is.null(self$region)) {
        pos <- private$position_at(event$screen_x, event$screen_y, clamp = TRUE)
        if (!is.null(pos)) private$set_cursor(pos, extend = TRUE)
        event$stop()
      }
    },

    #' @description Ends a drag selection.
    #' @param event A `MouseEvent`.
    on_mouse_up = function(event) {
      private$.dragging <- FALSE
    },

    #' @description The mouse wheel scrolls three rows.
    #' @param event A `MouseEvent`.
    on_mouse_scroll = function(event) {
      g <- private$geometry()
      if (is.null(g)) return(invisible())
      delta <- switch(event$direction, up = -3L, down = 3L, 0L)
      if (delta != 0L) {
        private$scroll_visual(g, delta)
        private$.follow <- FALSE
        self$invalidate_paint()
        event$stop()
      }
      dx <- switch(event$direction, left = -4L, right = 4L, 0L)
      if (dx != 0L && !self$wrap) {
        private$.state$left <- max(0L, min(private$.state$left + dx, g$max_left))
        self$invalidate_paint()
        event$stop()
      }
    },

    # Content ---------------------------------------------------------------

    #' @description Replace the whole text. Clears the undo history and
    #'   moves the cursor to the start.
    #' @param text A string (or character vector of lines).
    set_text = function(text) {
      private$.buf$set_text(text)
      private$.highlight_from <- 1L
      private$.undo$clear()
      private$.state$cursor_row <- 1L
      private$.state$cursor_col <- 0L
      private$.state$anchor_row <- NA_integer_
      private$.state$anchor_col <- NA_integer_
      private$.state$top <- 1L
      private$.state$sub <- 0L
      private$.state$left <- 0L
      private$.want_x <- NA_integer_
      private$changed()
      invisible(self)
    },

    #' @description Insert text at the cursor, replacing the selection.
    #' @param text A string; line breaks are kept.
    #' @param kind Internal: `"type"` merges into undo groups, `"paste"`
    #'   does not.
    insert = function(text, kind = NULL) {
      if (self$read_only || !self$is_enabled()) return(invisible(self))
      text <- normalize_text(text, self$tab_size)
      if (!nzchar(text)) return(invisible(self))
      sel <- private$selection_range()
      start <- sel$start %||% private$cursor()
      end <- sel$end %||% private$cursor()
      if (is.null(kind)) {
        kind <- if (is.null(sel) && !grepl("\n", text, fixed = TRUE) && grapheme_count(text) == 1L) "type" else "paste"
      }
      closes <- kind != "type" || grepl("^[[:space:]]$", text)
      private$apply_edit(start, end, text, kind, closes)
      invisible(self)
    },

    #' @description Select a range (1-based rows and columns; column 1 is
    #'   before the first character).
    #' @param start_row,start_column,end_row,end_column The range.
    select_range = function(start_row, start_column, end_row, end_column) {
      a <- private$.buf$clamp(text_pos(start_row, start_column - 1L))
      b <- private$.buf$clamp(text_pos(end_row, end_column - 1L))
      private$.state$anchor_row <- a[["row"]]
      private$.state$anchor_col <- a[["col"]]
      private$set_cursor(b, extend = TRUE, keep_anchor = TRUE)
      invisible(self)
    },

    #' @description Move the cursor to a line.
    #' @param row Line number (1-based).
    #' @param column Column (1-based).
    goto_line = function(row, column = 1L) {
      private$set_cursor(private$.buf$clamp(text_pos(row, column - 1L)), extend = FALSE)
      invisible(self)
    },

    #' @description Cursor position. Columns count grapheme clusters and
    #'   terminal display cells, both 1-based.
    cursor_position = function() {
      row <- private$.state$cursor_row
      cells <- private$cells(row)$widths
      column <- private$.state$cursor_col
      list(line = row, grapheme_column = column + 1L,
           display_column = sum(cells[seq_len(min(column, length(cells)))]) + 1L)
    },

    #' @description Copy the selection to the app clipboard (and the system
    #'   clipboard via OSC 52 when `system = TRUE` and supported).
    #' @param system Also write to the system clipboard?
    copy = function(system = TRUE) {
      sel <- private$selection_range()
      app <- self$app
      if (!is.null(sel) && !is.null(app)) app$clipboard_write(private$.buf$slice(sel$start, sel$end), system = system)
      invisible(self)
    },

    #' @description Copy the selection and delete it.
    #' @param system Also write to the system clipboard?
    cut = function(system = TRUE) {
      self$copy(system)
      if (!self$read_only) private$delete_selection()
      invisible(self)
    },

    #' @description Undo the last edit group.
    undo = function() {
      edit <- private$.undo$undo(private$.buf)
      if (is.null(edit)) return(invisible(FALSE))
      private$.highlight_from <- edit$first_row
      private$restore_cursor(edit$before)
      invisible(TRUE)
    },

    #' @description Redo the last undone edit.
    redo = function() {
      edit <- private$.undo$redo(private$.buf)
      if (is.null(edit)) return(invisible(FALSE))
      private$.highlight_from <- edit$first_row
      private$restore_cursor(edit$after)
      invisible(TRUE)
    },

    # Search ----------------------------------------------------------------

    #' @description Search for text and select the first match at or after
    #'   the cursor.
    #' @param query Text or regular expression (PCRE).
    #' @param case_sensitive Match case?
    #' @param regex Is `query` a regular expression? Invalid expressions
    #'   raise an error.
    #' @return `TRUE` if there is a match (invisibly).
    find = function(query, case_sensitive = FALSE, regex = FALSE) {
      private$.find <- search_query(query, case_sensitive, regex)
      private$.find_text <- query
      private$.match_cache <- NULL
      invisible(private$goto_match(1L, include_current = TRUE))
    },

    #' @description Select the next match (wrapping around).
    find_next = function() invisible(private$goto_match(1L)),

    #' @description Select the previous match (wrapping around).
    find_previous = function() invisible(private$goto_match(-1L)),

    #' @description Replace the current search match, or find `query` and
    #'   replace its next match.
    #' @param replacement Replacement text.
    #' @param query Optional search text; defaults to the active search.
    #' @param case_sensitive,regex Search options used when `query` is given.
    replace = function(replacement, query = NULL, case_sensitive = FALSE, regex = FALSE) {
      check_scalar_character(replacement, "replacement")
      if (!is.null(query)) self$find(query, case_sensitive, regex)
      sel <- private$selection_range()
      if (is.null(sel)) return(invisible(FALSE))
      if (!is.null(private$.find) && private$.find$regex) {
        current <- private$.buf$slice(sel$start, sel$end)
        replacement <- sub(private$.find$pattern, replacement, current, perl = TRUE,
                           ignore.case = !private$.find$case_sensitive)
      }
      private$apply_edit(sel$start, sel$end, replacement, "paste", TRUE)
      invisible(TRUE)
    },

    #' @description Replace all matches in one document edit. If the edit is
    #'   larger than the bounded undo budget, it is applied without retaining
    #'   an undo record.
    #' @param query Search text or regular expression.
    #' @param replacement Replacement text.
    #' @param case_sensitive Match case?
    #' @param regex Interpret `query` as a regular expression?
    replace_all = function(query, replacement, case_sensitive = FALSE, regex = FALSE) {
      check_scalar_character(replacement, "replacement")
      q <- search_query(query, case_sensitive, regex)
      lines <- private$.buf$lines
      matches <- vapply(lines, function(line) nrow(query_line_matches(q, line)), 0L)
      count <- sum(matches)
      if (!count) return(invisible(0L))
      changed <- lines
      for (i in which(matches > 0L)) {
        if (q$regex) {
          changed[[i]] <- gsub(q$pattern, replacement, changed[[i]], perl = TRUE,
                               ignore.case = !q$case_sensitive)
        } else {
          haystack <- if (q$case_sensitive) changed[[i]] else tolower(changed[[i]])
          needle <- if (q$case_sensitive) q$pattern else tolower(q$pattern)
          m <- gregexpr(needle, haystack, fixed = TRUE)[[1L]]
          lens <- attr(m, "match.length")
          for (k in rev(seq_along(m))) {
            before <- if (m[[k]] <= 1L) "" else substr(changed[[i]], 1L, m[[k]] - 1L)
            after_start <- m[[k]] + lens[[k]]
            after <- if (after_start > nchar(changed[[i]])) "" else substr(changed[[i]], after_start, nchar(changed[[i]]))
            changed[[i]] <- paste0(before, replacement, after)
          }
        }
      }
      end <- private$.buf$end_pos()
      private$apply_edit(text_pos(1L, 0L), end, paste(changed, collapse = "\n"), "paste", TRUE)
      invisible(count)
    },

    #' @description Remove the search highlights and close the find bar.
    clear_find = function() {
      private$.find <- NULL
      private$.find_open <- FALSE
      private$.match_cache <- NULL
      self$invalidate_paint()
      invisible(self)
    },

    # Actions ---------------------------------------------------------------

    #' @description Move one character left.
    action_cursor_left = function() private$move_horizontal(-1L, FALSE),
    #' @description Move one character right.
    action_cursor_right = function() private$move_horizontal(1L, FALSE),
    #' @description Move one row up.
    action_cursor_up = function() private$move_vertical(-1L, FALSE),
    #' @description Move one row down.
    action_cursor_down = function() private$move_vertical(1L, FALSE),
    #' @description Move to the start of the line.
    action_line_start = function() private$move_to(private$line_start(), FALSE),
    #' @description Move to the end of the line.
    action_line_end = function() private$move_to(private$line_end(), FALSE),
    #' @description Move to the start of the text.
    action_document_start = function() private$move_to(text_pos(1L, 0L), FALSE),
    #' @description Move to the end of the text.
    action_document_end = function() private$move_to(private$.buf$end_pos(), FALSE),
    #' @description Move to the previous word.
    action_word_left = function() private$move_to(private$word_left(), FALSE),
    #' @description Move to the next word.
    action_word_right = function() private$move_to(private$word_right(), FALSE),
    #' @description Move one page up.
    action_page_up = function() private$page(-1L, FALSE),
    #' @description Move one page down.
    action_page_down = function() private$page(1L, FALSE),
    #' @description Extend the selection left.
    action_select_left = function() private$move_horizontal(-1L, TRUE),
    #' @description Extend the selection right.
    action_select_right = function() private$move_horizontal(1L, TRUE),
    #' @description Extend the selection up.
    action_select_up = function() private$move_vertical(-1L, TRUE),
    #' @description Extend the selection down.
    action_select_down = function() private$move_vertical(1L, TRUE),
    #' @description Select to the start of the line.
    action_select_line_start = function() private$move_to(private$line_start(), TRUE),
    #' @description Select to the end of the line.
    action_select_line_end = function() private$move_to(private$line_end(), TRUE),
    #' @description Select to the start of the text.
    action_select_document_start = function() private$move_to(text_pos(1L, 0L), TRUE),
    #' @description Select to the end of the text.
    action_select_document_end = function() private$move_to(private$.buf$end_pos(), TRUE),
    #' @description Select to the previous word.
    action_select_word_left = function() private$move_to(private$word_left(), TRUE),
    #' @description Select to the next word.
    action_select_word_right = function() private$move_to(private$word_right(), TRUE),
    #' @description Extend the selection one page up.
    action_select_page_up = function() private$page(-1L, TRUE),
    #' @description Extend the selection one page down.
    action_select_page_down = function() private$page(1L, TRUE),
    #' @description Select everything.
    action_select_all = function() {
      end <- private$.buf$end_pos()
      self$select_range(1L, 1L, end[["row"]], end[["col"]] + 1L)
    },

    #' @description Insert a line break (keeping the indentation when
    #'   `auto_indent` is on).
    action_newline = function() {
      indent <- ""
      if (self$auto_indent) {
        line <- private$.buf$lines[[private$.state$cursor_row]]
        indent <- sub("^( *).*$", "\\1", line)
        indent <- grapheme_prefix(indent, private$.state$cursor_col)
      }
      self$insert(paste0("\n", indent), kind = "paste")
    },

    #' @description Delete the selection or the character before the cursor.
    action_delete_left = function() {
      if (private$delete_selection()) return(invisible(self))
      cur <- private$cursor()
      if (cur[["col"]] > 0L) {
        private$apply_edit(text_pos(cur[["row"]], cur[["col"]] - 1L), cur, "", "delete", FALSE)
      } else if (cur[["row"]] > 1L) {
        prev <- text_pos(cur[["row"]] - 1L, private$.buf$line_length(cur[["row"]] - 1L))
        private$apply_edit(prev, cur, "", "other", TRUE)
      }
    },

    #' @description Delete the selection or the character at the cursor.
    action_delete_right = function() {
      if (private$delete_selection()) return(invisible(self))
      cur <- private$cursor()
      len <- private$.buf$line_length(cur[["row"]])
      if (cur[["col"]] < len) {
        private$apply_edit(cur, text_pos(cur[["row"]], cur[["col"]] + 1L), "", "delete", FALSE)
      } else if (cur[["row"]] < private$.buf$n_lines()) {
        private$apply_edit(cur, text_pos(cur[["row"]] + 1L, 0L), "", "other", TRUE)
      }
    },

    #' @description Delete the word before the cursor.
    action_delete_word_left = function() {
      if (private$delete_selection()) return(invisible(self))
      private$apply_edit(private$word_left(), private$cursor(), "", "other", TRUE)
    },

    #' @description Delete the word after the cursor.
    action_delete_word_right = function() {
      if (private$delete_selection()) return(invisible(self))
      private$apply_edit(private$cursor(), private$word_right(), "", "other", TRUE)
    },

    #' @description Delete from the start of the line to the cursor.
    action_delete_to_line_start = function() {
      private$apply_edit(private$line_start(), private$cursor(), "", "other", TRUE)
    },

    #' @description Delete from the cursor to the end of the line (or the
    #'   line break when the cursor is at the end).
    action_delete_to_line_end = function() {
      cur <- private$cursor()
      end <- private$line_end()
      if (pos_equal(cur, end) && cur[["row"]] < private$.buf$n_lines()) end <- text_pos(cur[["row"]] + 1L, 0L)
      private$apply_edit(cur, end, "", "other", TRUE)
    },

    #' @description Insert the app clipboard.
    action_paste = function() {
      app <- self$app
      if (!is.null(app) && nzchar(app$clipboard)) self$insert(app$clipboard, kind = "paste")
    },

    #' @description Insert spaces up to the next tab stop (or indent the
    #'   selected lines).
    action_indent = function() {
      sel <- private$selection_range()
      if (!is.null(sel) && sel$start[["row"]] != sel$end[["row"]]) return(private$shift_lines(sel, self$tab_size))
      cur <- private$cursor()
      self$insert(strrep(" ", self$tab_size - (cur[["col"]] %% self$tab_size)), kind = "paste")
    },

    #' @description Remove up to one tab stop of leading spaces.
    action_dedent = function() {
      sel <- private$selection_range()
      rows <- if (is.null(sel)) private$.state$cursor_row else sel$start[["row"]]
      private$shift_lines(sel %||% list(start = text_pos(rows, 0L), end = text_pos(rows, 0L)), -self$tab_size)
    },

    #' @description Undo.
    action_undo = function() self$undo(),
    #' @description Redo.
    action_redo = function() self$redo(),

    #' @description Open the find bar (Ctrl+F).
    action_open_find = function() {
      sel <- private$selection_range()
      if (!is.null(sel) && sel$start[["row"]] == sel$end[["row"]]) {
        private$find_set(private$.buf$slice(sel$start, sel$end), jump = FALSE)
      }
      private$.find_open <- TRUE
      private$.goto_open <- FALSE
      private$.replace_open <- FALSE
      self$invalidate_paint()
    },
    #' @description Select the next match (F3).
    action_find_next = function() self$find_next(),
    #' @description Select the previous match (Shift+F3).
    action_find_previous = function() self$find_previous(),

    #' @description Open the find and replace bar (Ctrl+H).
    action_open_replace = function() {
      private$.replace_open <- TRUE
      private$.replace_target <- "query"
      private$.find_open <- TRUE
      self$invalidate_paint()
    },
    #' @description Open the go-to-line bar (Ctrl+G).
    action_open_goto = function() {
      private$.goto_text <- ""
      private$.goto_open <- TRUE
      self$invalidate_paint()
    },

    #' @description Natural width: the longest line (at most 120 columns).
    content_width = function() min(120L, max(c(1L, nchar(utils::head(private$.buf$lines, 1000L), type = "width")))) + 1L,

    #' @description Natural height: the number of lines (at most 1000).
    content_height = function(width) min(private$.buf$n_lines(), 1000L),

    #' @description Draw the visible part of the text.
    #' @param buffer A [ScreenBuffer].
    #' @param area Visible part of the region.
    #' @param st Computed style.
    paint = function(buffer, area, st) {
      draw_background(buffer, area, st)
      draw_border(buffer, self$region, st, area, self$border_title)
      g <- private$geometry()
      if (is.null(g)) return(invisible())
      if (private$.follow) private$reveal(g)
      private$paint_text(buffer, g, st, rect_intersect(g$inner, area))
    }
  ),
  active = list(
    #' @field value The text. Assigning replaces it (see `set_text()`).
    value = function(value) {
      if (missing(value)) return(private$.buf$text())
      self$set_text(value)
    },
    #' @field text The text (read-only alias of `value`).
    text = function(value) if (missing(value)) private$.buf$text() else read_only("text"),
    #' @field lines The lines as a character vector.
    lines = function(value) if (missing(value)) private$.buf$lines else read_only("lines"),
    #' @field n_lines Number of lines.
    n_lines = function(value) if (missing(value)) private$.buf$n_lines() else read_only("n_lines"),
    #' @field cursor_row,cursor_column Cursor position, 1-based (column 1 is
    #'   before the first character).
    cursor_row = function(value) if (missing(value)) private$.state$cursor_row else read_only("cursor_row"),
    cursor_column = function(value) if (missing(value)) private$.state$cursor_col + 1L else read_only("cursor_column"),
    #' @field selection The selected text (`""` if none).
    selection = function(value) {
      if (!missing(value)) read_only("selection")
      sel <- private$selection_range()
      if (is.null(sel)) "" else private$.buf$slice(sel$start, sel$end)
    },
    #' @field can_undo,can_redo Is there something to undo / redo?
    can_undo = function(value) if (missing(value)) private$.undo$can_undo() else read_only("can_undo"),
    can_redo = function(value) if (missing(value)) private$.undo$can_redo() else read_only("can_redo"),
    #' @field history_size Characters held by the undo history.
    history_size = function(value) if (missing(value)) private$.undo$size() else read_only("history_size"),
    #' @field match_count Number of matches of the current search.
    match_count = function(value) if (missing(value)) private$count_matches() else read_only("match_count"),
    #' @field find_open Is the find bar open?
    find_open = function(value) if (missing(value)) private$.find_open else read_only("find_open"),
    #' @field valid Does the value pass `validate`?
    valid = function(value) if (missing(value)) is.null(private$.state$error) else read_only("valid"),
    #' @field error The validation message, or `NULL`.
    error = function(value) if (missing(value)) private$.state$error else read_only("error")
  ),
  private = list(
    .buf = NULL,
    .undo = NULL,
    .wrap_cache = NULL,
    .follow = TRUE,
    .want_x = NA_integer_,
    .dragging = FALSE,
    .find = NULL,
    .find_text = "",
    .find_open = FALSE,
    .replace_open = FALSE,
    .replace_target = "query",
    .replace_text = "",
    .goto_open = FALSE,
    .goto_text = "",
    .find_error = NULL,
    .match_cache = NULL,
    .maxw_cache = NULL,
    .highlight_cache = NULL,
    .highlight_from = 1L,

    # Positions --------------------------------------------------------------

    cursor = function() text_pos(private$.state$cursor_row, private$.state$cursor_col),

    selection_range = function() {
      st <- private$.state
      if (is.na(st$anchor_row)) return(NULL)
      a <- text_pos(st$anchor_row, st$anchor_col)
      b <- private$cursor()
      if (pos_equal(a, b)) return(NULL)
      pos_order(a, b)
    },

    line_start = function() text_pos(private$.state$cursor_row, 0L),
    line_end = function() text_pos(private$.state$cursor_row, private$.buf$line_length(private$.state$cursor_row)),

    cells = function(row) {
      chars <- split_graphemes(private$.buf$lines[[row]])
      list(chars = chars, widths = if (!length(chars)) integer() else pmax(grapheme_width(chars), 1L))
    },

    highlight_spans = function(row) {
      empty <- data.frame(start = integer(), end = integer(), token = character())
      if (is.null(self$highlighter)) return(empty)
      key <- paste(private$.buf$version, row, sep = ":")
      if (exists(key, private$.highlight_cache, inherits = FALSE)) return(get(key, private$.highlight_cache))
      contextual <- isTRUE(attr(self$highlighter, "termr.contextual", exact = TRUE))
      raw <- if (contextual) {
        self$highlighter(private$.buf$lines,
                         state = list(language = self$language, line = row, version = private$.buf$version,
                                      changed_from = private$.highlight_from))
      } else {
        self$highlighter(private$.buf$lines[[row]], state = list(language = self$language, line = row))
      }
      if (is.list(raw) && !is.data.frame(raw)) raw <- raw[[1L]]
      if (is.null(raw)) raw <- empty
      if (!is.data.frame(raw) || !all(c("start", "end", "token") %in% names(raw))) {
        stop("A highlighter must return data frames with `start`, `end`, and `token` columns.", call. = FALSE)
      }
      raw <- raw[, c("start", "end", "token"), drop = FALSE]
      if (nrow(raw) && (!is.numeric(raw$start) || !is.numeric(raw$end) || !is.character(raw$token) ||
          anyNA(raw$start) || anyNA(raw$end) || anyNA(raw$token) || any(raw$start != floor(raw$start)) ||
          any(raw$end != floor(raw$end)) || any(raw$start < 1L | raw$end < raw$start) ||
          any(raw$end > length(split_graphemes(private$.buf$lines[[row]]))) ||
          any(!raw$token %in% c("keyword", "string", "comment", "number", "constant", "operator", "function",
                               "identifier", "punctuation", "parameter", "quoted_identifier")))) {
        stop("Highlighter spans must use valid grapheme ranges and known token names.", call. = FALSE)
      }
      assign(key, raw, private$.highlight_cache)
      raw
    },

    word_class = function(ch) grepl("^[[:space:][:punct:]]$", ch),

    word_left = function() {
      cur <- private$cursor()
      if (cur[["col"]] == 0L) {
        return(if (cur[["row"]] > 1L) text_pos(cur[["row"]] - 1L, private$.buf$line_length(cur[["row"]] - 1L)) else cur)
      }
      chars <- private$cells(cur[["row"]])$chars
      i <- cur[["col"]]
      while (i > 0L && private$word_class(chars[[i]])) i <- i - 1L
      while (i > 0L && !private$word_class(chars[[i]])) i <- i - 1L
      text_pos(cur[["row"]], i)
    },

    word_right = function() {
      cur <- private$cursor()
      len <- private$.buf$line_length(cur[["row"]])
      if (cur[["col"]] >= len) {
        return(if (cur[["row"]] < private$.buf$n_lines()) text_pos(cur[["row"]] + 1L, 0L) else cur)
      }
      chars <- private$cells(cur[["row"]])$chars
      i <- cur[["col"]]
      while (i < len && private$word_class(chars[[i + 1L]])) i <- i + 1L
      while (i < len && !private$word_class(chars[[i + 1L]])) i <- i + 1L
      text_pos(cur[["row"]], i)
    },

    # Cursor movement ---------------------------------------------------------

    # Place the cursor. With `extend` the selection grows from the anchor
    # (set to the old cursor if there was none); otherwise it is dropped.
    set_cursor = function(pos, extend = FALSE, keep_x = FALSE, keep_anchor = FALSE) {
      st <- private$.state
      pos <- private$.buf$clamp(pos)
      had_selection <- !is.null(private$selection_range())
      if (!keep_anchor) {
        if (extend) {
          if (is.na(st$anchor_row)) {
            st$anchor_row <- st$cursor_row
            st$anchor_col <- st$cursor_col
          }
        } else {
          st$anchor_row <- NA_integer_
          st$anchor_col <- NA_integer_
        }
      }
      st$cursor_row <- pos[["row"]]
      st$cursor_col <- pos[["col"]]
      private$.state <- st
      if (!keep_x) private$.want_x <- NA_integer_
      private$.follow <- TRUE
      self$invalidate_paint()
      if (had_selection || !is.null(private$selection_range())) private$selection_changed()
      invisible(TRUE)
    },

    move_to = function(pos, extend) {
      if (!extend) private$.undo$break_group()
      private$set_cursor(pos, extend)
    },

    move_horizontal = function(delta, extend) {
      sel <- private$selection_range()
      cur <- private$cursor()
      if (!extend && !is.null(sel)) return(private$move_to(if (delta < 0L) sel$start else sel$end, FALSE))
      row <- cur[["row"]]
      col <- cur[["col"]] + delta
      if (col < 0L) {
        if (row == 1L) col <- 0L else {
          row <- row - 1L
          col <- private$.buf$line_length(row)
        }
      } else if (col > private$.buf$line_length(row)) {
        if (row < private$.buf$n_lines()) {
          row <- row + 1L
          col <- 0L
        } else {
          col <- private$.buf$line_length(row)
        }
      }
      private$move_to(text_pos(row, col), extend)
    },

    move_vertical = function(delta, extend) {
      g <- private$geometry()
      cur <- private$cursor()
      if (is.null(g)) return(private$move_to(text_pos(cur[["row"]] + delta, cur[["col"]]), extend))
      sel <- private$selection_range()
      private$.undo$break_group()
      x <- private$.want_x
      if (is.na(x)) x <- private$x_of(cur[["row"]], cur[["col"]], g)
      row <- cur[["row"]]
      seg <- private$segment_of(row, cur[["col"]], g)
      n <- private$.buf$n_lines()
      end_reached <- NULL
      for (i in seq_len(abs(delta))) {
        if (delta > 0L) {
          if (seg + 1L < length(private$starts_of(row, g))) seg <- seg + 1L
          else if (row < n) {
            row <- row + 1L
            seg <- 0L
          } else {
            end_reached <- text_pos(row, private$.buf$line_length(row))
            break
          }
        } else {
          if (seg > 0L) seg <- seg - 1L
          else if (row > 1L) {
            row <- row - 1L
            seg <- length(private$starts_of(row, g)) - 1L
          } else {
            end_reached <- text_pos(1L, 0L)
            break
          }
        }
      }
      pos <- end_reached %||% text_pos(row, private$col_at_x(row, seg, x, g))
      private$set_cursor(pos, extend, keep_x = TRUE)
      private$.want_x <- if (is.null(end_reached)) x else NA_integer_
      invisible(TRUE)
    },

    page = function(direction, extend) {
      g <- private$geometry()
      if (is.null(g)) return(invisible())
      private$scroll_visual(g, direction * g$body_h)
      private$move_vertical(direction * max(1L, g$body_h - 1L), extend)
    },

    # Layout ----------------------------------------------------------------

    geometry = function() {
      region <- self$region
      if (is.null(region)) return(NULL)
      st <- self$computed_style()
      inner <- content_rect(region, st)
      if (rect_is_empty(inner)) return(NULL)
      n <- private$.buf$n_lines()
      gutter <- if (self$line_numbers) nchar(as.character(n)) + 1L else 0L
      body_h <- inner$height - as.integer(private$.find_open || private$.goto_open)
      bar_y <- n > body_h || private$.state$top > 1L
      text_w <- max(1L, inner$width - gutter - as.integer(bar_y))
      bar_x <- FALSE
      max_left <- 0L
      if (!self$wrap) {
        widest <- private$widest_line()
        if (widest + 1L > text_w) {
          bar_x <- body_h > 1L
          max_left <- widest + 1L - text_w
        }
      }
      if (bar_x) body_h <- body_h - 1L
      body_h <- max(1L, body_h)
      list(inner = inner, gutter = gutter, text_w = text_w, wrap_w = max(1L, text_w - 1L),
           body_h = body_h, bar_y = bar_y, bar_x = bar_x, max_left = max_left, n = n,
           bar_row = inner$y + inner$height - 1L)
    },

    widest_line = function() {
      v <- private$.buf$version
      if (is.null(private$.maxw_cache) || private$.maxw_cache$version != v) {
        private$.maxw_cache <- list(version = v, width = max(nchar(private$.buf$lines, type = "width")))
      }
      private$.maxw_cache$width
    },

    # Start indices (1-based graphemes) of the visual rows of a line.
    starts_of = function(row, g) {
      if (!self$wrap) return(1L)
      line <- private$.buf$lines[[row]]
      w <- g$wrap_w
      key <- as.character(row)
      entry <- private$.wrap_cache[[key]]
      if (!is.null(entry) && entry$w == w && identical(entry$line, line)) return(entry$starts)
      if (nchar(line, "width") < w) {
        starts <- 1L
      } else {
        cells <- private$cells(row)
        starts <- editor_wrap_starts(cells$chars, cells$widths, w)
      }
      if (length(private$.wrap_cache) > 4000L) rm(list = ls(private$.wrap_cache), envir = private$.wrap_cache)
      assign(key, list(line = line, w = w, starts = starts), envir = private$.wrap_cache)
      starts
    },

    segment_of = function(row, col, g) {
      starts <- private$starts_of(row, g)
      if (length(starts) == 1L) return(0L)
      findInterval(col + 1L, starts) - 1L
    },

    x_of = function(row, col, g) {
      starts <- private$starts_of(row, g)
      seg <- private$segment_of(row, col, g)
      from <- starts[[seg + 1L]]
      if (col < from) return(0L)
      sum(private$cells(row)$widths[from:col])
    },

    col_at_x = function(row, seg, x, g) {
      starts <- private$starts_of(row, g)
      cells <- private$cells(row)
      n <- length(cells$chars)
      from <- starts[[seg + 1L]]
      to <- if (seg + 1L < length(starts)) starts[[seg + 2L]] - 1L else n
      if (to < from) return(from - 1L)
      k <- sum(cumsum(cells$widths[from:to]) <= x)
      k <- min(k, if (seg + 1L < length(starts)) to - from else to - from + 1L)
      from - 1L + k
    },

    # The visual rows from (top, sub): a list of integer vectors
    # c(row, seg, from, to) with `to` = -1 for "end of line".
    visual_rows = function(g, top, sub, count) {
      rows <- vector("list", count)
      k <- 0L
      r <- top
      s <- sub
      n <- g$n
      while (k < count && r <= n) {
        starts <- private$starts_of(r, g)
        while (s < length(starts) && k < count) {
          k <- k + 1L
          rows[[k]] <- c(r, s, starts[[s + 1L]], if (s + 1L < length(starts)) starts[[s + 2L]] - 1L else -1L)
          s <- s + 1L
        }
        r <- r + 1L
        s <- 0L
      }
      rows[seq_len(k)]
    },

    # Move a (row, seg) position back by `k` visual rows.
    step_back = function(row, seg, k, g) {
      while (k > 0L) {
        if (seg > 0L) seg <- seg - 1L
        else if (row > 1L) {
          row <- row - 1L
          seg <- length(private$starts_of(row, g)) - 1L
        } else break
        k <- k - 1L
      }
      c(row, seg)
    },

    # Visual rows between (top, sub) and (row, seg), or NA when >= limit.
    visual_distance = function(top, sub, row, seg, limit, g) {
      if (row - top >= limit) return(NA_integer_)
      if (row == top) return(seg - sub)
      d <- length(private$starts_of(top, g)) - sub
      r <- top + 1L
      while (r < row && d < limit) {
        d <- d + length(private$starts_of(r, g))
        r <- r + 1L
      }
      if (r < row || d + seg >= limit) NA_integer_ else d + seg
    },

    scroll_visual = function(g, delta) {
      st <- private$.state
      top <- st$top
      sub <- st$sub
      n <- g$n
      steps <- abs(delta)
      while (steps > 0L) {
        if (delta > 0L) {
          if (sub + 1L < length(private$starts_of(top, g))) sub <- sub + 1L
          else if (top < n) {
            top <- top + 1L
            sub <- 0L
          } else break
        } else {
          if (sub > 0L) sub <- sub - 1L
          else if (top > 1L) {
            top <- top - 1L
            sub <- length(private$starts_of(top, g)) - 1L
          } else break
        }
        steps <- steps - 1L
      }
      # Never scroll past the point where the last row is at the bottom.
      limit <- private$step_back(n, length(private$starts_of(n, g)) - 1L, g$body_h - 1L, g)
      if (top > limit[[1]] || (top == limit[[1]] && sub > limit[[2]])) {
        top <- limit[[1]]
        sub <- limit[[2]]
      }
      private$.state$top <- top
      private$.state$sub <- sub
      invisible()
    },

    # Scroll so that the cursor is visible.
    reveal = function(g) {
      private$.follow <- FALSE
      st <- private$.state
      row <- st$cursor_row
      seg <- private$segment_of(row, st$cursor_col, g)
      top <- min(st$top, g$n)
      sub <- if (top == st$top) st$sub else 0L
      if (row < top || (row == top && seg < sub)) {
        top <- row
        sub <- seg
      } else if (is.na(private$visual_distance(top, sub, row, seg, g$body_h, g))) {
        back <- private$step_back(row, seg, g$body_h - 1L, g)
        top <- back[[1]]
        sub <- back[[2]]
      }
      private$.state$top <- top
      private$.state$sub <- sub
      if (!self$wrap) {
        x <- private$x_of(row, st$cursor_col, g)
        left <- st$left
        if (x < left) left <- x
        else if (x + 1L > left + g$text_w) left <- x + 1L - g$text_w
        private$.state$left <- max(0L, min(left, g$max_left))
      } else private$.state$left <- 0L
      invisible()
    },

    # Which text position is under screen (x, y)? With `clamp` positions
    # outside the text snap to the nearest edge (used while dragging).
    position_at = function(x, y, clamp = FALSE) {
      g <- private$geometry()
      if (is.null(g)) return(NULL)
      st <- private$.state
      k <- y - g$inner$y + 1L
      if (clamp) k <- min(max(k, 1L), g$body_h)
      if (k < 1L || k > g$body_h) return(NULL)
      rows <- private$visual_rows(g, st$top, st$sub, g$body_h)
      if (!length(rows)) return(private$.buf$end_pos())
      if (k > length(rows)) {
        if (!clamp) return(private$.buf$end_pos())
        k <- length(rows)
      }
      r <- rows[[k]]
      ox <- x - g$inner$x - g$gutter + st$left
      if (ox < 0L) ox <- 0L
      cells <- private$cells(r[[1]])
      n <- length(cells$chars)
      from <- r[[3]]
      to <- if (r[[4]] < 0L) n else r[[4]]
      if (to < from) return(text_pos(r[[1]], from - 1L))
      cum <- cumsum(cells$widths[from:to])
      idx <- sum(cum <= ox)  # whole characters before the pointer
      col <- from - 1L + min(idx, if (r[[4]] < 0L) to - from + 1L else to - from)
      text_pos(r[[1]], col)
    },

    # Editing ---------------------------------------------------------------

    apply_edit = function(start, end, text, kind, closes) {
      if (self$read_only || !self$is_enabled()) return(invisible(FALSE))
      if (pos_equal(start, end) && !nzchar(text)) return(invisible(FALSE))
      before <- private$cursor()
      res <- private$.buf$replace_range(start, end, text)
      res$before <- before
      res$after <- res$end
      res$kind <- kind
      res$closes <- closes
      private$.undo$push(res)
      st <- private$.state
      st$cursor_row <- res$end[["row"]]
      st$cursor_col <- res$end[["col"]]
      st$anchor_row <- NA_integer_
      st$anchor_col <- NA_integer_
      private$.state <- st
      private$.want_x <- NA_integer_
      private$.highlight_from <- res$first_row
      private$changed()
      invisible(TRUE)
    },

    delete_selection = function() {
      sel <- private$selection_range()
      if (is.null(sel)) return(FALSE)
      if (self$read_only) return(TRUE)
      private$apply_edit(sel$start, sel$end, "", "other", TRUE)
      TRUE
    },

    # Indent (positive) or dedent (negative) the rows of a selection.
    shift_lines = function(sel, amount) {
      rows <- seq.int(sel$start[["row"]], sel$end[["row"]] - as.integer(sel$end[["col"]] == 0L && sel$end[["row"]] > sel$start[["row"]]))
      lines <- private$.buf$lines[rows]
      new <- if (amount > 0L) paste0(strrep(" ", amount), lines) else {
        lead <- nchar(sub("^( *).*$", "\\1", lines))
        substring(lines, pmin(lead, -amount) + 1L)
      }
      if (identical(new, lines)) return(invisible(FALSE))
      start <- text_pos(rows[[1]], 0L)
      end <- text_pos(rows[[length(rows)]], private$.buf$line_length(rows[[length(rows)]]))
      private$apply_edit(start, end, paste(new, collapse = "\n"), "paste", TRUE)
    },

    restore_cursor = function(pos) {
      pos <- private$.buf$clamp(pos)
      st <- private$.state
      st$cursor_row <- pos[["row"]]
      st$cursor_col <- pos[["col"]]
      st$anchor_row <- NA_integer_
      st$anchor_col <- NA_integer_
      private$.state <- st
      private$.want_x <- NA_integer_
      private$changed()
    },

    changed = function() {
      private$.follow <- TRUE
      private$.match_cache <- NULL
      private$.highlight_cache <- new.env(parent = emptyenv())
      st <- private$.state
      st$top <- min(st$top, private$.buf$n_lines())
      error <- run_validator(self$validate, private$.buf$text())
      was <- st$error
      st$error <- error
      private$.state <- st
      if (!identical(was, error)) {
        bump_epoch()
        self$post_message(if (is.null(error)) "textarea.valid" else "textarea.invalid", list(error = error))
      }
      self$invalidate()
      self$post_message("textarea.changed", list(version = private$.buf$version, lines = private$.buf$n_lines(),
                                                  valid = is.null(error)))
    },

    selection_changed = function() {
      sel <- private$selection_range()
      self$post_message("textarea.selection_changed", list(
        empty = is.null(sel),
        start_row = if (!is.null(sel)) sel$start[["row"]], start_column = if (!is.null(sel)) sel$start[["col"]] + 1L,
        end_row = if (!is.null(sel)) sel$end[["row"]], end_column = if (!is.null(sel)) sel$end[["col"]] + 1L
      ))
    },

    # Search ----------------------------------------------------------------

    # Move to the next/previous match relative to the selection or cursor.
    goto_match = function(direction, include_current = FALSE) {
      q <- private$.find
      if (is.null(q) || !nzchar(q$pattern)) return(FALSE)
      buf <- private$.buf
      sel <- private$selection_range()
      ref <- if (direction > 0L) (if (include_current && !is.null(sel)) sel$start else (sel$end %||% private$cursor()))
             else (sel$start %||% private$cursor())
      row <- ref[["row"]]
      col <- ref[["col"]]
      found <- NULL
      m <- query_line_matches(q, buf$lines[[row]])
      if (direction > 0L) {
        ok <- which(m$start >= col)
        if (length(ok)) found <- c(row, m$start[[ok[[1]]]], m$end[[ok[[1]]]])
      } else {
        ok <- which(m$end <= col & m$start < col)
        if (length(ok)) found <- c(row, m$start[[max(ok)]], m$end[[max(ok)]])
      }
      if (is.null(found)) {
        n <- buf$n_lines()
        get <- function(i) buf$lines[i]
        from <- if (direction > 0L) row + 1L else row - 1L
        wrapped <- FALSE
        r <- NA_integer_
        if (direction > 0L && from <= n) r <- search_chunks(q, n, get, from, 1L, wrap = FALSE)
        if (direction < 0L && from >= 1L) r <- search_chunks(q, n, get, from, -1L, wrap = FALSE)
        if (is.na(r)) {
          # Wrap around, including the rest of the starting row.
          r <- search_chunks(q, n, get, if (direction > 0L) 1L else n, direction, wrap = FALSE)
          wrapped <- TRUE
        }
        if (is.na(r)) return(FALSE)
        mm <- query_line_matches(q, buf$lines[[r]])
        k <- if (direction > 0L) 1L else nrow(mm)
        found <- c(r, mm$start[[k]], mm$end[[k]])
        if (wrapped && !is.null(self$app)) NULL
      }
      st <- private$.state
      st$anchor_row <- found[[1]]
      st$anchor_col <- found[[2]]
      st$cursor_row <- found[[1]]
      st$cursor_col <- found[[3]]
      private$.state <- st
      private$.want_x <- NA_integer_
      private$.follow <- TRUE
      self$invalidate_paint()
      private$selection_changed()
      TRUE
    },

    count_matches = function() {
      q <- private$.find
      if (is.null(q) || !nzchar(q$pattern)) return(0L)
      v <- private$.buf$version
      if (!is.null(private$.match_cache) && private$.match_cache$version == v) return(private$.match_cache$count)
      hit <- which(query_matches(q, private$.buf$lines))
      count <- if (length(hit) > 20000L) length(hit) else
        sum(vapply(hit, function(i) nrow(query_line_matches(q, private$.buf$lines[[i]])), 0L))
      private$.match_cache <- list(version = v, count = count)
      count
    },

    # Set the find text (find bar) and jump to the first match from the
    # start of the current selection.
    find_set = function(text, jump = TRUE) {
      private$.find_text <- text
      private$.find_error <- NULL
      private$.match_cache <- NULL
      q <- tryCatch(
        search_query(text, private$.find_case %||% FALSE, isTRUE(private$.find_regex)),
        error = function(e) {
          private$.find_error <- conditionMessage(e)
          NULL
        }
      )
      private$.find <- q
      if (jump && !is.null(q)) private$goto_match(1L, include_current = TRUE)
      self$invalidate_paint()
      invisible()
    },

    .find_case = FALSE,
    .find_regex = FALSE,

    goto_key = function(event) {
      key <- event$key
      if (key == "escape") {
        private$.goto_open <- FALSE
      } else if (key == "backspace") {
        chars <- strsplit(private$.goto_text, "", fixed = TRUE)[[1]]
        private$.goto_text <- paste(head(chars, -1L), collapse = "")
      } else if (key == "enter") {
        row <- suppressWarnings(as.integer(private$.goto_text))
        if (!is.na(row) && row > 0L) self$goto_line(row)
        private$.goto_open <- FALSE
      } else if (event$is_printable() && grepl("^[0-9]$", event$char)) {
        private$.goto_text <- paste0(private$.goto_text, event$char)
      } else {
        return(invisible())
      }
      self$invalidate_paint()
      event$stop()
    },

    find_key = function(event) {
      key <- event$key
      if (key == "escape") {
        private$.find_open <- FALSE
        private$.replace_open <- FALSE
        self$invalidate_paint()
      } else if (key == "tab" && private$.replace_open) {
        private$.replace_target <- if (private$.replace_target == "query") "replacement" else "query"
        self$invalidate_paint()
      } else if (key == "alt+r") {
        private$.replace_open <- !private$.replace_open
        private$.replace_target <- "query"
        self$invalidate_paint()
      } else if (key == "ctrl+enter" && private$.replace_open) {
        tryCatch(self$replace_all(private$.find_text, private$.replace_text,
                                  isTRUE(private$.find_case), isTRUE(private$.find_regex)),
                 error = function(e) private$.find_error <- conditionMessage(e))
        private$.find_open <- FALSE
        private$.replace_open <- FALSE
        self$invalidate_paint()
      } else if (key == "enter" && private$.replace_open && private$.replace_target == "replacement") {
        self$replace(private$.replace_text)
        private$goto_match(1L)
      } else if (key == "backspace" && private$.replace_open && private$.replace_target == "replacement") {
        chars <- split_graphemes(private$.replace_text)
        private$.replace_text <- paste(chars[-length(chars)], collapse = "")
        self$invalidate_paint()
      } else if (key %in% c("enter", "down", "f3")) {
        self$find_next()
      } else if (key %in% c("up", "shift+f3")) {
        self$find_previous()
      } else if (key == "backspace") {
        chars <- split_graphemes(private$.find_text)
        private$find_set(paste(chars[-length(chars)], collapse = ""))
      } else if (key == "ctrl+r") {
        private$.find_regex <- !isTRUE(private$.find_regex)
        private$find_set(private$.find_text)
      } else if (key == "alt+c") {
        private$.find_case <- !isTRUE(private$.find_case)
        private$find_set(private$.find_text)
      } else if (event$is_printable() && private$.replace_open && private$.replace_target == "replacement") {
        private$.replace_text <- paste0(private$.replace_text, event$char)
        self$invalidate_paint()
      } else if (event$is_printable()) {
        private$find_set(paste0(private$.find_text, event$char))
      } else {
        return(invisible())
      }
      event$stop()
    },

    # Painting --------------------------------------------------------------

    paint_text = function(buffer, g, st, clip) {
      state <- private$.state
      buf <- private$.buf
      rows <- private$visual_rows(g, state$top, state$sub, g$body_h)
      focused <- self$focused && self$is_enabled()
      sel <- private$selection_range()
      styles <- list(
        gutter = resolve_style(style(foreground = "$muted"), parent = st),
        gutter_current = resolve_style(style(bold = TRUE), parent = st),
        selected = resolve_style(style(background = "$primary", foreground = "$on_primary"), parent = st),
        match = resolve_style(style(background = "$warning", foreground = "$background", underline = TRUE), parent = st),
        cursor = resolve_style(style(reverse = TRUE), parent = st),
        syntax_keyword = resolve_style(style(foreground = "blue", bold = TRUE), parent = st),
        syntax_string = resolve_style(style(foreground = "green"), parent = st),
        syntax_comment = resolve_style(style(foreground = "$muted", italic = TRUE), parent = st),
        syntax_number = resolve_style(style(foreground = "cyan"), parent = st),
        syntax_constant = resolve_style(style(foreground = "magenta", bold = TRUE), parent = st),
        syntax_operator = resolve_style(style(foreground = "yellow"), parent = st),
        syntax_function = resolve_style(style(foreground = "cyan", bold = TRUE), parent = st),
        syntax_identifier = resolve_style(style(foreground = "white"), parent = st),
        syntax_punctuation = resolve_style(style(foreground = "$muted"), parent = st),
        syntax_parameter = resolve_style(style(foreground = "magenta"), parent = st),
        syntax_quoted_identifier = resolve_style(style(foreground = "cyan", underline = TRUE), parent = st),
        placeholder = resolve_style(style(foreground = "$muted", italic = TRUE), parent = st)
      )
      x0 <- g$inner$x + g$gutter
      if (buf$n_lines() == 1L && !nzchar(buf$lines[[1]]) && nzchar(self$placeholder)) {
        buffer$put_text(x0, g$inner$y, str_truncate(self$placeholder, g$text_w), fg = styles$placeholder$foreground,
                        bg = styles$placeholder$background, attrs = styles$placeholder$attrs, clip = clip)
        if (focused) {
          first <- split_graphemes(sanitize_text(self$placeholder))[[1]]
          buffer$put_text(x0, g$inner$y, first, attrs = styles$cursor$attrs, clip = clip)
        }
        return(invisible())
      }
      query <- private$.find
      text_clip <- rect_intersect(rect(x0, g$inner$y, g$text_w, g$inner$height), clip)
      for (k in seq_along(rows)) {
        r <- rows[[k]]
        y <- g$inner$y + k - 1L
        if (g$gutter > 0L) {
          label <- if (r[[2]] == 0L) formatC(r[[1]], width = g$gutter - 1L) else strrep(" ", g$gutter - 1L)
          gst <- if (r[[1]] == state$cursor_row) styles$gutter_current else styles$gutter
          buffer$put_text(g$inner$x, y, paste0(label, " "), fg = gst$foreground, bg = gst$background,
                          attrs = gst$attrs, clip = clip)
        }
        private$paint_row(buffer, x0, y, g, r, state, sel, query, focused, styles, st, text_clip)
      }
      private$paint_bars(buffer, g, st, clip, rows)
      if (private$.goto_open) private$paint_goto_bar(buffer, g, st, clip)
      else if (private$.find_open) private$paint_find_bar(buffer, g, st, clip)
      invisible()
    },

    paint_row = function(buffer, x0, y, g, r, state, sel, query, focused, styles, st, clip) {
      row <- r[[1]]
      cells <- private$cells(row)
      n <- length(cells$chars)
      from <- r[[3]]
      to <- if (r[[4]] < 0L) n else r[[4]]
      last_seg <- r[[4]] < 0L
      kinds <- character()
      if (to >= from) {
        idx <- from:to
        kinds <- rep("plain", length(idx))
        spans <- private$highlight_spans(row)
        if (nrow(spans)) for (i in seq_len(nrow(spans))) {
          kinds[idx >= spans$start[[i]] & idx <= spans$end[[i]]] <- paste0("syntax_", spans$token[[i]])
        }
        if (!is.null(sel) && row >= sel$start[["row"]] && row <= sel$end[["row"]]) {
          s <- if (row == sel$start[["row"]]) sel$start[["col"]] else 0L
          e <- if (row == sel$end[["row"]]) sel$end[["col"]] else n
          kinds[idx > s & idx <= e] <- "selected"
        }
        if (!is.null(query)) {
          m <- query_line_matches(query, private$.buf$lines[[row]])
          for (i in seq_len(nrow(m))) kinds[idx > m$start[[i]] & idx <= m$end[[i]]] <- "match"
        }
        if (focused && row == state$cursor_row && state$cursor_col < n) {
          kinds[idx == state$cursor_col + 1L] <- "cursor"
        }
        pieces <- cells$chars[idx]
        widths <- cells$widths[idx]
      } else {
        pieces <- character()
        widths <- integer()
      }
      # A selected line break is shown as a highlighted blank cell.
      eol_selected <- !is.null(sel) && last_seg && row >= sel$start[["row"]] && row < sel$end[["row"]]
      cursor_eol <- focused && last_seg && row == state$cursor_row && state$cursor_col >= n
      if (eol_selected || cursor_eol) {
        pieces <- c(pieces, " ")
        widths <- c(widths, 1L)
        kinds <- c(kinds, if (cursor_eol) "cursor" else "selected")
      }
      if (!length(pieces)) return(invisible())
      # Horizontal scrolling (no wrap): drop what is left of the window.
      shift <- state$left
      if (shift > 0L) {
        ends <- cumsum(widths)
        keep <- ends > shift
        pieces <- pieces[keep]
        kinds <- kinds[keep]
        widths <- widths[keep]
        if (!length(pieces)) return(invisible())
      }
      cum <- cumsum(widths)
      visible <- cum - widths < g$text_w - 0L
      pieces <- pieces[visible]
      kinds <- kinds[visible]
      widths <- widths[visible]
      groups <- cumsum(c(TRUE, kinds[-1L] != kinds[-length(kinds)]))
      x <- x0
      group_cells <- split(seq_along(pieces), groups)
      for (gi in group_cells) {
        kind <- kinds[[gi[[1]]]]
        text <- paste(pieces[gi], collapse = "")
        w <- sum(widths[gi])
        sty <- if (kind == "plain") NULL else styles[[kind]]
        buffer$put_text(x, y, text, fg = sty$foreground %||% st$foreground, bg = sty$background %||% st$background,
                        attrs = bitwOr(st$attrs, sty$attrs %||% 0L), clip = clip)
        x <- x + w
      }
      invisible()
    },

    paint_bars = function(buffer, g, st, clip, rows) {
      state <- private$.state
      if (g$bar_y) {
        track <- rect(g$inner$x + g$inner$width - 1L, g$inner$y, 1L, g$body_h)
        view <- if (length(rows)) length(unique(vapply(rows, `[[`, 0L, 1L))) else g$body_h
        draw_scrollbar(buffer, track, state$top - 1L, g$n, max(1L, view), "vertical", st, clip)
      }
      if (g$bar_x) {
        track <- rect(g$inner$x + g$gutter, g$inner$y + g$body_h, g$text_w, 1L)
        draw_scrollbar(buffer, track, state$left, g$max_left + g$text_w, g$text_w, "horizontal", st, clip)
      }
      invisible()
    },

    paint_find_bar = function(buffer, g, st, clip) {
      bar <- resolve_style(style(background = "$surface", foreground = "$foreground"), parent = st)
      err <- resolve_style(style(background = "$surface", foreground = "$error", bold = TRUE), parent = bar)
      width <- g$inner$width
      flags <- paste0(if (isTRUE(private$.find_case)) "[Aa] " else "", if (isTRUE(private$.find_regex)) "[.*] " else "")
      status <- if (!is.null(private$.find_error)) {
        sub("^Invalid regular expression ", "bad regex: ", private$.find_error)
      } else if (!nzchar(private$.find_text)) {
        ""
      } else {
        count <- private$count_matches()
        if (count == 0L) "no matches" else paste0(count, if (count == 1L) " match" else " matches")
      }
      left <- if (private$.replace_open) {
        paste0(if (private$.replace_target == "query") " Find: " else " Replace: ",
               if (private$.replace_target == "query") private$.find_text else private$.replace_text, "_")
      } else paste0(" Find: ", private$.find_text, "_")
      right <- paste0(flags, status, " ")
      room <- max(0L, width - str_width(right))
      line <- paste0(str_align(left, room), str_truncate(right, width))
      sty <- if (!is.null(private$.find_error) || (nzchar(private$.find_text) && private$count_matches() == 0L)) err else bar
      buffer$put_text(g$inner$x, g$bar_row, str_align(line, width), fg = sty$foreground, bg = sty$background,
                      attrs = sty$attrs, clip = clip)
    },

    paint_goto_bar = function(buffer, g, st, clip) {
      bar <- resolve_style(style(background = "$surface", foreground = "$foreground"), parent = st)
      text <- paste0(" Go to line (1-", private$.buf$n_lines(), "): ", private$.goto_text, "_")
      buffer$put_text(g$inner$x, g$bar_row, str_align(str_truncate(text, g$inner$width), g$inner$width),
                      fg = bar$foreground, bg = bar$background, attrs = bar$attrs, clip = clip)
    }
  )
)

# Start indices of the visual rows of a wrapped line. Every grapheme belongs
# to exactly one row; rows break after the last space that fits (a space just
# past the width hangs at the end of the row), or inside a word when there is
# none.
editor_wrap_starts <- function(chars, widths, width) {
  n <- length(chars)
  if (n == 0L || sum(widths) <= width) return(1L)
  csum <- cumsum(widths)
  starts <- 1L
  start <- 1L
  repeat {
    base <- if (start > 1L) csum[[start - 1L]] else 0L
    fit <- findInterval(base + width, csum) - start + 1L
    if (fit < 1L) fit <- 1L
    end <- start + fit - 1L
    if (end >= n) break
    if (chars[[end + 1L]] == " ") {
      # The space after the last word hangs at the end of this row.
      end <- end + 1L
      if (end >= n) break
    } else if (chars[[end]] != " ") {
      spaces <- which(chars[start:end] == " ")
      if (length(spaces)) end <- start + max(spaces) - 1L
    }
    start <- end + 1L
    starts <- c(starts, start)
  }
  starts
}

#' Multi-line text editor
#'
#' An editable text area with a cursor, selection, undo/redo, search and
#' scrolling. Text is stored as a vector of lines and edited by logical
#' (row, grapheme) positions, so emoji, accents and CJK text are never
#' split, and only the visible lines are formatted when drawing.
#'
#' Keys: arrows, Home/End, Ctrl+Home/End, Ctrl+Left/Right (words),
#' PageUp/PageDown, Shift+ to select, Ctrl+A select all, Enter, Backspace,
#' Delete, Ctrl+Backspace / Ctrl+Delete (words), Ctrl+U / Ctrl+K (to line
#' start / end), Ctrl+C / Ctrl+X / Ctrl+V (copy, cut, paste with the app
#' clipboard; copy also reaches the system clipboard via OSC 52 when the
#' terminal supports it), Ctrl+Z undo, Ctrl+Y redo, Ctrl+F find (Enter /
#' Down next, Up previous, Ctrl+R regex, Alt+C case, Esc close), F3 /
#' Shift+F3 next / previous match. Tab moves focus unless
#' `tab_behavior = "indent"`. The mouse places the cursor, drags select and
#' the wheel scrolls. A bracketed paste is one undo step.
#'
#' Messages (`event$data`): `"textarea.changed"` (`version`, `lines`,
#' `valid`; read the text with `$value`), `"textarea.selection_changed"`
#' (`empty`, `start_row`, `start_column`, `end_row`, `end_column`) and
#' `"textarea.valid"` / `"textarea.invalid"` (`error`).
#'
#' Editing groups: typing is merged into undo steps per word, paste,
#' delete-selection and line operations are single steps. The history is
#' bounded (`max_history` steps and about 2 million characters).
#' Tabs in text are expanded to spaces. This widget is **experimental**.
#'
#' @param value Initial text.
#' @param placeholder Text shown while the area is empty.
#' @param id Optional identifier.
#' @param classes Optional classes.
#' @param style A [style()].
#' @param disabled Disabled areas cannot be focused or edited.
#' @param line_numbers Show line numbers?
#' @param wrap Wrap long lines? If `FALSE` the view scrolls horizontally.
#' @param read_only Block editing?
#' @param language Optional language label supplied to the highlighter.
#' @param tab_size Spaces per tab stop.
#' @param tab_behavior `"focus"` (Tab moves focus) or `"indent"`.
#' @param auto_indent Keep the indentation on Enter?
#' @param validate Optional `function(value)` returning `NULL`/`TRUE` when
#'   valid or a message.
#' @param max_history Maximum number of undo steps.
#' @param highlighter Optional viewport-first syntax highlighter; see
#'   `r_highlighter()`.
#' @return A `TextArea` widget. Useful members: `$value`, `$set_text()`,
#'   `$insert()`, `$selection`, `$cursor_row`, `$cursor_column`,
#'   `$goto_line()`, `$cursor_position()`, `$undo()`, `$redo()`, `$find()`,
#'   `$replace()`, `$replace_all()`, `$find_next()`, `$find_previous()`,
#'   `$match_count`.
#' @export
#' @examples
#' ed <- text_area("Hello\nworld", id = "editor", line_numbers = TRUE)
#' ed$insert("!")
#' ed$value
#' ed$find("wor")
#' ed$selection
text_area <- function(value = "", placeholder = "", id = NULL, classes = NULL, style = NULL,
                      disabled = FALSE, line_numbers = FALSE, wrap = TRUE, read_only = FALSE,
                      language = NULL, tab_size = 4L, tab_behavior = "focus", auto_indent = FALSE,
                      validate = NULL, max_history = 200L, highlighter = NULL) {
  TextArea$new(value, placeholder = placeholder, id = id, classes = classes, style = style,
               disabled = disabled, line_numbers = line_numbers, wrap = wrap, read_only = read_only,
               language = language, tab_size = tab_size, tab_behavior = tab_behavior,
               auto_indent = auto_indent, validate = validate, max_history = max_history,
               highlighter = highlighter)
}

#' Lightweight R syntax highlighter
#'
#' Returns a viewport-first tokenizer for [text_area()]. It highlights common
#' R keywords, strings, comments, numeric constants, operators, and function
#' names. It uses tolerant regular expressions and accepts incomplete syntax.
#' Each line produces grapheme-indexed spans with `start`, `end`, and `token`.
#' @return A function `(lines, state = NULL)` suitable for `text_area()`.
#' @export
r_highlighter <- function() {
  pattern <- paste0(
    '"(?:\\\\.|[^"\\\\])*"?', "|", "'(?:\\\\.|[^'\\\\])*'?", "|",
    "`[^`]*`|#[^\\n]*|\\b(?:if|else|repeat|while|function|for|in|next|break)\\b|",
    "\\b(?:TRUE|FALSE|NULL|NA|NA_integer_|NA_real_|NA_character_|Inf|NaN)\\b|",
    "\\b[0-9]+(?:\\.[0-9]*)?(?:[eE][+-]?[0-9]+)?[L]?\\b|",
    "[A-Za-z.][A-Za-z0-9._]*(?=\\s*\\()|<<-|<-|->>|->|==|!=|<=|>=|&&|\\|\\||[+*/^~:$@=<>!&|%-]"
  )
  function(lines, state = NULL) {
    lapply(lines, function(line) {
      match <- gregexpr(pattern, line, perl = TRUE)[[1L]]
      if (match[[1L]] < 0L) return(data.frame(start = integer(), end = integer(), token = character()))
      text <- regmatches(line, list(match))[[1L]]
      lens <- attr(match, "match.length")
      token <- vapply(text, function(value) {
        if (startsWith(value, "#")) "comment"
        else if (startsWith(value, "\"") || startsWith(value, "'") || startsWith(value, "`")) "string"
        else if (grepl("^(if|else|repeat|while|function|for|in|next|break)$", value)) "keyword"
        else if (grepl("^(TRUE|FALSE|NULL|NA|NA_integer_|NA_real_|NA_character_|Inf|NaN)$", value)) "constant"
        else if (grepl("^[0-9]", value)) "number"
        else if (grepl("^[A-Za-z.]", value)) "function"
        else "operator"
      }, "")
      start <- as.integer(match)
      end <- start + as.integer(lens) - 1L
      if (!is_ascii(line)) {
        cumulative <- cumsum(nchar(split_graphemes(line), type = "chars"))
        start <- findInterval(start - 1L, cumulative) + 1L
        end <- findInterval(end - 1L, cumulative) + 1L
      }
      data.frame(start = as.integer(start), end = as.integer(end), token = token)
    })
  }
}

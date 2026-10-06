#' @title TabPane widget
#' @description The content of one tab. See [tab()].
#' @export
TabPane <- R6::R6Class(
  "TabPane",
  inherit = Vertical,
  public = list(
    #' @description Create a pane.
    #' @param label Tab label.
    #' @param ... Child widgets.
    #' @param id,classes,style,disabled See [Widget].
    initialize = function(label, ..., id = NULL, classes = NULL, style = NULL, disabled = FALSE) {
      super$initialize(..., id = id, classes = classes, style = style, disabled = disabled)
      private$.state$label <- check_text(label)
    }
  ),
  active = list(
    #' @field label The tab label (reactive).
    label = function(value) {
      if (missing(value)) return(private$.state$label)
      self$set_state("label", check_text(value))
      if (!is.null(self$parent)) self$parent$invalidate()
    }
  ),
  private = list(
    describe = function() encodeString(as.character(as_text(private$.state$label)), quote = "\"")
  )
)

#' @title Tabs widget
#' @description Tabbed content. See [tabs()].
#' @rdname Tabs-class
#' @export
Tabs <- R6::R6Class(
  "Tabs",
  inherit = Widget,
  public = list(
    #' @field focusable The tab bar can take focus (Left/Right switch tabs).
    focusable = TRUE,

    #' @description Create tabs. See [tabs()].
    #' @param ... [tab()] panes.
    #' @param active Index or id of the initially active tab.
    #' @param id,classes,style,disabled See [Widget].
    initialize = function(..., active = 1L, id = NULL, classes = NULL, style = NULL, disabled = FALSE) {
      super$initialize(id = id, classes = classes, style = style, disabled = disabled)
      private$.state$active <- 0L
      for (pane in flatten_widgets(list(...))) self$add_tab(pane)
      if (length(self$children)) self$activate(active)
    },

    #' @description The built-in style.
    default_style = function() style(width = "1fr", height = "1fr", layout = "vertical"),

    #' @description Left/Right switch tabs when the bar is focused;
    #'   Ctrl+PageUp / Ctrl+PageDown switch from anywhere inside.
    default_bindings = function() {
      list(
        bind("left", "previous_tab"), bind("right", "next_tab"),
        bind("ctrl+pageup", "previous_tab"), bind("ctrl+pagedown", "next_tab")
      )
    },

    #' @description Add a tab at the end.
    #' @param pane A [tab()] (or any widget, wrapped in a tab named after
    #'   its id).
    #' @param activate Make it the active tab?
    add_tab = function(pane, activate = FALSE) {
      if (!inherits(pane, "TabPane")) {
        if (!is_widget(pane)) stop("Tabs must be created with tab().", call. = FALSE)
        pane <- TabPane$new(pane$id %||% paste("Tab", length(self$children) + 1L), pane)
      }
      pane$visible <- FALSE
      self$mount(pane)
      if (activate || private$.state$active == 0L) self$activate(length(self$children))
      invisible(pane)
    },

    #' @description Remove a tab.
    #' @param which Index or id of the tab.
    remove_tab = function(which) {
      i <- private$index_of(which)
      active <- private$.state$active
      self$children[[i]]$remove()
      n <- length(self$children)
      if (n == 0L) {
        private$.state$active <- 0L
      } else if (i == active) {
        # The neighbour on the right (or the new last tab) takes over.
        private$.state$active <- 0L
        self$activate(min(i, n))
      } else if (i < active) {
        private$.state$active <- active - 1L
      }
      self$invalidate()
      invisible(self)
    },

    #' @description Show a tab.
    #' @param which Index or id of the tab.
    activate = function(which) {
      i <- private$index_of(which)
      old <- private$.state$active
      if (i == old) return(invisible(self))
      panes <- self$children
      app <- self$app
      focus_inside <- !is.null(app) && !is.null(app$focused) && old > 0L && old <= length(panes) &&
        any(vapply(app$focused$ancestors(), identical, logical(1), panes[[old]]))
      for (k in seq_along(panes)) panes[[k]]$visible <- k == i
      self$set_state("active", i)
      if (focus_inside) self$focus()
      self$post_message("tabs.changed", list(
        index = i, previous = old, id = panes[[i]]$id,
        label = as.character(as_text(panes[[i]]$label))
      ))
      invisible(self)
    },

    #' @description Show the next tab.
    action_next_tab = function() {
      n <- length(self$children)
      if (n) self$activate(private$.state$active %% n + 1L)
    },

    #' @description Show the previous tab.
    action_previous_tab = function() {
      n <- length(self$children)
      if (n) self$activate((private$.state$active - 2L) %% n + 1L)
    },

    #' @description Clicking a label shows its tab.
    #' @param event A `MouseEvent`.
    on_mouse_down = function(event) {
      if (event$button != "left" || is.null(self$region)) return(invisible())
      bar <- private$bar_rect()
      if (event$screen_y != bar$y) return(invisible())
      spans <- private$label_spans()
      hit <- which(event$screen_x >= spans$start & event$screen_x <= spans$end)
      if (length(hit)) {
        self$activate(hit[[1]])
        event$stop()
      }
    },

    #' @description Panes go below the tab bar.
    #' @param children Visible children.
    #' @param inner Content rectangle.
    #' @param st Computed style.
    arrange_children = function(children, inner, st) {
      body <- rect(inner$x, inner$y + 2L, inner$width, inner$height - 2L)
      lapply(children, function(child) body)
    },

    #' @description Panes are clipped below the tab bar.
    #' @param st Computed style.
    child_clip = function(st) {
      inner <- content_rect(self$region, st)
      rect(inner$x, inner$y + 2L, inner$width, inner$height - 2L)
    },

    #' @description Natural width: the widest of the bar and the panes.
    content_width = function() {
      bar <- sum(private$label_widths()) + 1L
      panes <- vapply(self$children, function(p) natural_width(p), integer(1))
      max(c(bar, panes, 0L))
    },

    #' @description Natural height: bar, rule and the active pane.
    #' @param width Content width.
    content_height = function(width) {
      i <- private$.state$active
      pane <- if (i > 0L) natural_height(self$children[[i]], width) else 0L
      2L + pane
    },

    #' @description Paint the tab bar.
    #' @param buffer A [ScreenBuffer].
    #' @param area Visible part of the region.
    #' @param st Computed style.
    paint = function(buffer, area, st) {
      draw_background(buffer, area, st)
      draw_border(buffer, self$region, st, area, self$border_title)
      bar <- private$bar_rect()
      clip <- rect_intersect(area, rect(bar$x, bar$y, bar$width, 2L))
      spans <- private$label_spans()
      labels <- private$labels()
      focused <- self$focused
      for (k in seq_along(labels)) {
        active <- k == private$.state$active
        tab_st <- resolve_style(
          if (active) (if (focused) tab_styles$active_focus else tab_styles$active) else tab_styles$inactive,
          parent = st
        )
        buffer$put_text(spans$start[[k]], bar$y, paste0(" ", labels[[k]], " "),
                        fg = tab_st$foreground, bg = tab_st$background, attrs = tab_st$attrs, clip = clip)
      }
      rule <- if (unicode_ok()) "\u2500" else "-"
      buffer$put_text(bar$x, bar$y + 1L, strrep(rule, bar$width), fg = "$muted", clip = clip)
      i <- private$.state$active
      if (i > 0L) {
        underline <- if (unicode_ok()) "\u2501" else "="
        buffer$put_text(spans$start[[i]], bar$y + 1L, strrep(underline, spans$end[[i]] - spans$start[[i]] + 1L),
                        fg = resolve_style(tab_styles$active, parent = st)$foreground, clip = clip)
      }
      invisible()
    }
  ),
  active = list(
    #' @field active Index of the active tab (assign to switch).
    active = function(value) {
      if (missing(value)) return(private$.state$active)
      self$activate(value)
    },
    #' @field active_tab The active [TabPane], or `NULL`.
    active_tab = function(value) {
      if (!missing(value)) read_only("active_tab")
      i <- private$.state$active
      if (i > 0L) self$children[[i]] else NULL
    },
    #' @field tab_count Number of tabs.
    tab_count = function(value) if (missing(value)) length(self$children) else read_only("tab_count")
  ),
  private = list(
    index_of = function(which) {
      n <- length(self$children)
      i <- if (is.character(which)) {
        match(which, vapply(self$children, function(p) p$id %||% "", ""))
      } else {
        as.integer(which)
      }
      if (length(i) != 1L || is.na(i) || i < 1L || i > n) {
        stop(sprintf("No tab %s (there are %d).", format(which), n), call. = FALSE)
      }
      i
    },
    labels = function() vapply(self$children, function(p) as.character(as_text(p$label)), character(1)),
    label_widths = function() str_width(private$labels()) + 2L,
    bar_rect = function() {
      inner <- content_rect(self$region, self$computed_style())
      rect(inner$x, inner$y, inner$width, 1L)
    },
    label_spans = function() {
      bar <- private$bar_rect()
      widths <- private$label_widths()
      starts <- bar$x + c(0L, cumsum(widths + 1L))[seq_along(widths)]
      list(start = starts, end = starts + widths - 1L)
    }
  )
)

tab_styles <- list(
  active = style(bold = TRUE, foreground = "$accent"),
  active_focus = style(bold = TRUE, reverse = TRUE, foreground = "$accent"),
  inactive = style(foreground = "white")
)

#' Tabs
#'
#' `tabs()` shows one of several panes, with a tab bar on top. Only the
#' active pane is laid out, painted and part of the focus chain. Switch tabs
#' by clicking a label, with Left/Right while the tab bar has focus, or
#' with Ctrl+PageUp / Ctrl+PageDown from anywhere inside. Switching sends a
#' `"tabs.changed"` message (`event$data`: `index`, `previous`, `id`,
#' `label`).
#'
#' @param ... `tab()` panes.
#' @param active Index or id of the initially active tab.
#' @param id Optional identifier.
#' @param classes Optional classes.
#' @param style A [style()].
#' @return A `Tabs` widget with methods `activate()`, `add_tab()`,
#'   `remove_tab()` and fields `active`, `active_tab`, `tab_count`.
#' @export
#' @examples
#' ui <- tabs(
#'   tab("Overview", label("Summary")),
#'   tab("Logs", label("No logs yet"), id = "logs")
#' )
#' render_widget(ui, 30, 4)
tabs <- function(..., active = 1L, id = NULL, classes = NULL, style = NULL) {
  Tabs$new(..., active = active, id = id, classes = classes, style = style)
}

#' @rdname tabs
#' @param label Tab label.
#' @export
tab <- function(label, ..., id = NULL, classes = NULL, style = NULL) {
  TabPane$new(label, ..., id = id, classes = classes, style = style)
}

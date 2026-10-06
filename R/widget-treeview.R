# Tree view.
#
# The tree is a model of TreeNode objects (not widgets). A TreeView shows
# the nodes whose ancestors are all expanded: they are flattened into a list
# of lines that is cached until the tree changes (any node change bumps the
# tree's version). Only the lines inside the viewport are painted, so large
# trees cost no more than small ones. Children can be loaded lazily with a
# `loader` function the first time a node is expanded.

#' @title Tree node
#' @description A node of a tree shown by [tree_view()]. See [tree_node()].
#' @export
TreeNode <- R6::R6Class(
  "TreeNode",
  public = list(
    #' @field label Text shown for the node.
    label = NULL,
    #' @field data Any R value attached to the node.
    data = NULL,
    #' @field id Optional identifier.
    id = NULL,
    #' @field loader `NULL` or `function(node)` returning children, called
    #'   the first time the node is expanded.
    loader = NULL,
    #' @field loaded Has the loader run?
    loaded = FALSE,

    #' @description Create a node. See [tree_node()].
    #' @param label Label.
    #' @param children Child nodes (or labels).
    #' @param data Attached value.
    #' @param expanded Start expanded?
    #' @param id Identifier.
    #' @param loader Lazy loader.
    initialize = function(label, children = list(), data = NULL, expanded = FALSE, id = NULL, loader = NULL) {
      check_scalar_character(label, "label")
      check_flag(expanded)
      check_function(loader, "loader", allow_null = TRUE)
      self$label <- label
      self$data <- data
      self$id <- id
      self$loader <- loader
      for (child in children) self$add(child)
      # Expanding runs the loader, so lazy nodes created expanded are filled.
      if (expanded) self$expand()
    },

    #' @description Add a child.
    #' @param node A `TreeNode` or a label.
    #' @param data Data for a node created from a label.
    #' @param expanded Expanded state for a node created from a label.
    #' @return The child node.
    add = function(node, data = NULL, expanded = FALSE) {
      if (!inherits(node, "TreeNode")) node <- TreeNode$new(as.character(node), data = data, expanded = expanded)
      if (!is.null(node$parent)) node$remove()
      np <- node$.__enclos_env__$private
      np$.parent <- self
      private$.children[[length(private$.children) + 1L]] <- node
      self$changed()
      invisible(node)
    },

    #' @description Detach the node from its parent.
    remove = function() {
      parent <- private$.parent
      if (is.null(parent)) return(invisible(self))
      pp <- parent$.__enclos_env__$private
      pp$.children <- Filter(function(n) !identical(n, self), pp$.children)
      private$.parent <- NULL
      parent$changed()
      invisible(self)
    },

    #' @description Remove all children.
    clear = function() {
      for (child in private$.children) child$remove()
      invisible(self)
    },

    #' @description Expand the node (running the loader if needed).
    expand = function() {
      if (!is.null(self$loader) && !self$loaded) {
        self$loaded <- TRUE
        for (child in self$loader(self)) self$add(child)
      }
      if (!private$.expanded) {
        private$.expanded <- TRUE
        self$changed()
      }
      invisible(self)
    },

    #' @description Collapse the node.
    collapse = function() {
      if (private$.expanded) {
        private$.expanded <- FALSE
        self$changed()
      }
      invisible(self)
    },

    #' @description Toggle between expanded and collapsed.
    toggle = function() if (private$.expanded) self$collapse() else self$expand(),

    #' @description Can the node be expanded?
    expandable = function() length(private$.children) > 0L || (!is.null(self$loader) && !self$loaded),

    #' @description Depth below the root (the root is 0).
    depth = function() length(self$ancestors()),

    #' @description Parent, grandparent, ... up to the root.
    ancestors = function() {
      out <- list()
      node <- private$.parent
      while (!is.null(node)) {
        out[[length(out) + 1L]] <- node
        node <- node$parent
      }
      out
    },

    #' @description Labels from the root to this node.
    path = function() c(rev(vapply(self$ancestors(), function(n) n$label, "")), self$label),

    #' @description This node and all descendants (depth first).
    walk = function() {
      out <- list(self)
      for (child in private$.children) out <- c(out, child$walk())
      out
    },

    #' @description Find a descendant by id.
    #' @param id Identifier.
    find = function(id) {
      for (node in self$walk()) if (identical(node$id, id)) return(node)
      NULL
    },

    #' @description Notify the tree view that the node changed.
    changed = function() {
      root <- self
      while (!is.null(root$parent)) root <- root$parent
      view <- root$.__enclos_env__$private$.view
      if (!is.null(view)) view$tree_changed()
      invisible(self)
    },

    #' @description Print the subtree.
    #' @param ... Ignored.
    print = function(...) {
      for (node in self$walk()) {
        cat(strrep("  ", node$depth() - self$depth()), if (node$expandable()) (if (node$expanded) "- " else "+ ") else "  ",
            node$label, "\n", sep = "")
      }
      invisible(self)
    }
  ),
  active = list(
    #' @field parent The parent node (read-only).
    parent = function(value) {
      if (!missing(value)) stop("`parent` is read-only; use add() / remove().", call. = FALSE)
      private$.parent
    },
    #' @field children Child nodes (read-only).
    children = function(value) {
      if (!missing(value)) stop("`children` is read-only; use add() / remove().", call. = FALSE)
      private$.children
    },
    #' @field expanded Is the node expanded? (assign to expand / collapse)
    expanded = function(value) {
      if (missing(value)) return(private$.expanded)
      check_flag(value, "expanded")
      if (value) self$expand() else self$collapse()
    }
  ),
  private = list(.parent = NULL, .children = list(), .expanded = FALSE, .view = NULL)
)

#' Tree nodes
#'
#' @param label Text shown for the node.
#' @param ... Child nodes, or strings (leaf nodes).
#' @param data Any R value attached to the node (e.g. a file path).
#' @param expanded Start expanded?
#' @param id Optional identifier (see `node$find(id)`).
#' @param loader Optional `function(node)` returning child nodes (or
#'   labels), called the first time the node is expanded. Use it for large
#'   or expensive trees such as file systems.
#' @return A `TreeNode`.
#' @export
#' @examples
#' root <- tree_node("Project", tree_node("R", "app.R", "widget.R"), "README.md", expanded = TRUE)
#' root
tree_node <- function(label, ..., data = NULL, expanded = FALSE, id = NULL, loader = NULL) {
  TreeNode$new(label, children = list(...), data = data, expanded = expanded, id = id, loader = loader)
}

#' @title TreeView widget
#' @description Shows a tree of [tree_node()]s. See [tree_view()].
#' @rdname TreeView-class
#' @export
TreeView <- R6::R6Class(
  "TreeView",
  inherit = Widget,
  public = list(
    #' @field paint_states Cursor and scroll changes only repaint.
    paint_states = c("cursor", "offset"),
    #' @field focusable Trees can be focused.
    focusable = TRUE,
    #' @field show_root Show the root node?
    show_root = TRUE,
    #' @field guides Draw guide lines?
    guides = TRUE,

    #' @description Create a tree view. See [tree_view()].
    #' @param root The root [TreeNode].
    #' @param show_root,guides See fields.
    #' @param id,classes,style,disabled See [Widget].
    initialize = function(root, show_root = TRUE, guides = TRUE, id = NULL, classes = NULL,
                          style = NULL, disabled = FALSE) {
      if (!inherits(root, "TreeNode")) stop("`root` must be a tree_node().", call. = FALSE)
      check_flag(show_root)
      check_flag(guides)
      super$initialize(id = id, classes = classes, style = style, disabled = disabled)
      self$show_root <- show_root
      self$guides <- guides
      private$.root <- root
      rp <- root$.__enclos_env__$private
      rp$.view <- self
      if (!show_root) root$expand()
      private$.state$cursor <- if (length(private$lines())) 1L else 0L
      private$.state$offset <- 0L
    },

    #' @description The built-in style.
    default_style = function() style(width = "1fr", height = "1fr"),

    #' @description Navigation keys.
    default_bindings = function() {
      list(
        bind("up", "cursor_up"), bind("down", "cursor_down"),
        bind("pageup", "page_up"), bind("pagedown", "page_down"),
        bind("home", "first"), bind("end", "last"),
        bind("right", "expand_or_child"), bind("left", "collapse_or_parent"),
        bind("enter", "activate", "Open"), bind("space", "toggle", "Toggle")
      )
    },

    #' @description Called by nodes when the tree changes.
    tree_changed = function() {
      private$.lines <- NULL
      n <- length(private$lines())
      private$.state$cursor <- as.integer(min(max(if (n) 1L else 0L, private$.state$cursor), n))
      self$invalidate()
    },

    #' @description Move the cursor to a node (expanding its ancestors).
    #' @param node A node of this tree.
    select = function(node) {
      for (a in node$ancestors()) a$expand()
      i <- private$index_of(node)
      if (!is.na(i)) private$move_to(i)
      invisible(self)
    },

    #' @description Move up.
    action_cursor_up = function() private$move_to(private$.state$cursor - 1L),
    #' @description Move down.
    action_cursor_down = function() private$move_to(private$.state$cursor + 1L),
    #' @description Move a page up.
    action_page_up = function() private$move_to(private$.state$cursor - max(1L, private$body_height() - 1L)),
    #' @description Move a page down.
    action_page_down = function() private$move_to(private$.state$cursor + max(1L, private$body_height() - 1L)),
    #' @description Go to the first node.
    action_first = function() private$move_to(1L),
    #' @description Go to the last visible node.
    action_last = function() private$move_to(length(private$lines())),
    #' @description Expand, or go to the first child.
    action_expand_or_child = function() {
      node <- self$cursor_node
      if (is.null(node)) return(invisible())
      if (node$expandable() && !node$expanded) {
        private$set_expanded(node, TRUE)
      } else if (length(node$children)) {
        private$move_to(private$.state$cursor + 1L)
      }
    },
    #' @description Collapse, or go to the parent.
    action_collapse_or_parent = function() {
      node <- self$cursor_node
      if (is.null(node)) return(invisible())
      if (node$expanded && node$expandable()) {
        private$set_expanded(node, FALSE)
      } else if (!is.null(node$parent)) {
        i <- private$index_of(node$parent)
        if (!is.na(i)) private$move_to(i)
      }
    },
    #' @description Toggle the cursor node.
    action_toggle = function() {
      node <- self$cursor_node
      if (!is.null(node) && node$expandable()) private$set_expanded(node, !node$expanded)
    },
    #' @description Toggle and send `"tree.node_activated"`.
    action_activate = function() {
      node <- self$cursor_node
      if (is.null(node)) return(invisible())
      if (node$expandable()) private$set_expanded(node, !node$expanded)
      self$post_message("tree.node_activated", node_data(node))
    },

    #' @description Clicking selects; clicking the arrow toggles.
    #' @param event A `MouseEvent`.
    on_mouse_down = function(event) {
      if (event$button != "left" || is.null(self$region)) return(invisible())
      inner <- content_rect(self$region, self$computed_style())
      i <- private$.state$offset + event$screen_y - inner$y + 1L
      lines <- private$lines()
      if (i < 1L || i > length(lines)) return(invisible())
      private$move_to(i)
      line <- lines[[i]]
      icon_x <- inner$x + str_width(line$guide)
      if (event$screen_x == icon_x && line$node$expandable()) private$set_expanded(line$node, !line$node$expanded)
      event$stop()
    },

    #' @description The wheel scrolls three lines.
    #' @param event A `MouseEvent`.
    on_mouse_scroll = function(event) {
      delta <- switch(event$direction, up = -3L, down = 3L, 0L)
      if (delta != 0L) {
        self$set_state("offset", private$clamp_offset(private$.state$offset + delta))
        event$stop()
      }
    },

    #' @description Natural width: the widest visible line.
    content_width = function() {
      lines <- private$lines()
      if (!length(lines)) return(0L)
      max(vapply(lines, function(l) str_width(l$guide) + 2L + str_width(l$node$label), integer(1))) + 1L
    },

    #' @description Natural height: visible lines (at most 1000).
    #' @param width Content width.
    content_height = function(width) min(length(private$lines()), 1000L),

    #' @description Draw the visible lines.
    #' @param buffer A [ScreenBuffer].
    #' @param area Visible part of the region.
    #' @param st Computed style.
    paint = function(buffer, area, st) {
      draw_background(buffer, area, st)
      draw_border(buffer, self$region, st, area, self$border_title)
      inner <- content_rect(self$region, st)
      lines <- private$lines()
      n <- length(lines)
      h <- inner$height
      bar <- n > h
      width <- inner$width - bar
      private$.state$offset <- private$clamp_offset(private$.state$offset)
      off <- private$.state$offset
      clip <- rect_intersect(rect(inner$x, inner$y, width, h), area)
      utf8 <- unicode_ok()
      open_icon <- if (utf8) "\u25bc" else "v"
      closed_icon <- if (utf8) "\u25b6" else ">"
      focused <- self$focused
      for (k in seq_len(min(h, n - off))) {
        i <- off + k
        line <- lines[[i]]
        node <- line$node
        icon <- if (node$expandable()) (if (node$expanded) open_icon else closed_icon) else " "
        text <- paste0(line$guide, icon, " ", node$label)
        y <- inner$y + k - 1L
        cursor <- i == private$.state$cursor
        row_st <- if (cursor) resolve_style(if (focused) table_styles$cursor else table_styles$cursor_blur, parent = st) else st
        buffer$put_text(inner$x, y, str_align(text, width), fg = row_st$foreground,
                        bg = row_st$background, attrs = if (cursor) row_st$attrs else 0L, clip = clip)
        if (nzchar(line$guide)) {
          buffer$put_text(inner$x, y, line$guide, fg = "$muted", bg = row_st$background, clip = clip)
        }
      }
      if (bar) {
        draw_scrollbar(buffer, rect(inner$x + width, inner$y, 1L, h), off, n, h, "vertical", st, area)
      }
      invisible()
    }
  ),
  active = list(
    #' @field root The root node.
    root = function(value) if (missing(value)) private$.root else read_only("root"),
    #' @field cursor_node The node under the cursor, or `NULL`.
    cursor_node = function(value) {
      if (!missing(value)) read_only("cursor_node")
      i <- private$.state$cursor
      lines <- private$lines()
      if (i < 1L || i > length(lines)) NULL else lines[[i]]$node
    },
    #' @field visible_count Number of visible (expanded) lines.
    visible_count = function(value) if (missing(value)) length(private$lines()) else read_only("visible_count"),
    #' @field offset First visible line minus one.
    offset = function(value) if (missing(value)) private$.state$offset else read_only("offset")
  ),
  private = list(
    .root = NULL,
    .lines = NULL,

    # Flatten the visible nodes into lines with their guide prefixes.
    lines = function() {
      if (!is.null(private$.lines)) return(private$.lines)
      out <- list()
      utf8 <- unicode_ok() && self$guides
      add <- function(node, prefix, last, depth) {
        guide <- if (depth == 0L) "" else paste0(prefix, if (!self$guides) "  " else if (last) (if (utf8) "\u2514\u2500" else "`-") else (if (utf8) "\u251c\u2500" else "|-"))
        out[[length(out) + 1L]] <<- list(node = node, guide = guide)
        if (node$expanded) {
          kids <- node$children
          child_prefix <- if (depth == 0L) "" else paste0(prefix, if (!self$guides || last) "  " else if (utf8) "\u2502 " else "| ")
          for (k in seq_along(kids)) add(kids[[k]], child_prefix, k == length(kids), depth + 1L)
        }
      }
      root <- private$.root
      if (self$show_root) {
        add(root, "", TRUE, 0L)
      } else {
        kids <- root$children
        for (k in seq_along(kids)) add(kids[[k]], "", k == length(kids), 0L)
      }
      private$.lines <- out
      out
    },

    index_of = function(node) {
      match(TRUE, vapply(private$lines(), function(l) identical(l$node, node), logical(1)))
    },

    body_height = function() {
      if (is.null(self$region)) return(1L)
      max(1L, content_rect(self$region, self$computed_style())$height)
    },

    clamp_offset = function(offset) {
      as.integer(min(max(0L, offset), max(0L, length(private$lines()) - private$body_height())))
    },

    move_to = function(i) {
      n <- length(private$lines())
      if (n == 0L) return(invisible())
      i <- as.integer(min(max(1L, i), n))
      changed <- i != private$.state$cursor
      self$set_state("cursor", i)
      h <- private$body_height()
      off <- private$.state$offset
      if (i <= off) off <- i - 1L
      if (i > off + h) off <- i - h
      self$set_state("offset", as.integer(off))
      if (changed) self$post_message("tree.node_selected", node_data(self$cursor_node))
      invisible()
    },

    set_expanded = function(node, expanded) {
      if (expanded) node$expand() else node$collapse()
      self$post_message(if (expanded) "tree.node_expanded" else "tree.node_collapsed", node_data(node))
    }
  )
)

node_data <- function(node) list(node = node, label = node$label, data = node$data, id = node$id)

#' Tree view
#'
#' Shows a [tree_node()] hierarchy with expand/collapse arrows and guide
#' lines. Only visible lines are painted, and children can be loaded lazily.
#'
#' Keys: Up/Down/Page Up/Page Down/Home/End move; Right expands (or goes to
#' the first child); Left collapses (or goes to the parent); Space toggles;
#' Enter toggles and sends `"tree.node_activated"`. Clicking selects a node;
#' clicking its arrow toggles it.
#'
#' Messages (`event$data`: `node`, `label`, `data`, `id`):
#' `"tree.node_selected"`, `"tree.node_expanded"`, `"tree.node_collapsed"`,
#' `"tree.node_activated"`.
#'
#' @param root The root [tree_node()].
#' @param show_root Show the root itself (otherwise its children are the
#'   top level and it is always expanded).
#' @param guides Draw guide lines?
#' @param id Optional identifier.
#' @param classes Optional classes.
#' @param style A [style()].
#' @return A `TreeView` widget with method `select(node)` and fields
#'   `root`, `cursor_node`, `visible_count`.
#' @export
#' @examples
#' root <- tree_node("Project", tree_node("R", "app.R", "widget.R", expanded = TRUE),
#'                   "README.md", expanded = TRUE)
#' render_widget(tree_view(root), 24, 5)
tree_view <- function(root, show_root = TRUE, guides = TRUE, id = NULL, classes = NULL, style = NULL) {
  TreeView$new(root, show_root = show_root, guides = guides, id = id, classes = classes, style = style)
}

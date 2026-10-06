#' Events
#'
#' Everything that happens in a termr application is an event: key
#' presses, terminal resizes, focus changes, widgets being mounted, and
#' messages that widgets send (such as `"button.pressed"`).
#'
#' Events are delivered to their *target* widget and, if they bubble, to
#' each ancestor in turn and finally to the [App]. Any handler can call
#' `event$stop()` to stop propagation.
#'
#' Event types and their classes:
#' * `"key"`: `KeyEvent` with fields `key`, `char`, `ctrl`, `alt`, `shift`;
#' * `"resize"`: `ResizeEvent` with `width` and `height`;
#' * `"paste"`: `PasteEvent` with the pasted `text` (bracketed paste);
#' * `"focus"` / `"blur"`: `FocusEvent` / `BlurEvent` (do not bubble);
#' * `"mount"` / `"unmount"`: `MountEvent` / `UnmountEvent` (do not bubble);
#' * any other name: `MessageEvent`, created by `widget$post_message()`.
#'
#' @export
Event <- R6::R6Class(
  "Event",
  public = list(
    #' @field type Event type, e.g. `"key"` or `"button.pressed"`.
    type = NULL,
    #' @field sender The widget that created the event (or `NULL`).
    sender = NULL,
    #' @field target The widget the event is delivered to first.
    target = NULL,
    #' @field current The widget currently handling the event.
    current = NULL,
    #' @field data A list of event-specific values.
    data = NULL,
    #' @field bubbles Is the event passed on to ancestors?
    bubbles = TRUE,
    #' @field time Creation time (seconds, monotonic).
    time = NULL,

    #' @description Create an event.
    #' @param type Event type.
    #' @param sender Sending widget.
    #' @param data List of values.
    #' @param bubbles Does the event bubble?
    initialize = function(type, sender = NULL, data = list(), bubbles = TRUE) {
      check_scalar_character(type, "type")
      self$type <- type
      self$sender <- sender
      self$target <- sender
      self$data <- data
      self$bubbles <- bubbles
      self$time <- now_seconds()
    },

    #' @description Stop the event from reaching further handlers.
    stop = function() {
      private$.stopped <- TRUE
      invisible(self)
    },

    #' @description Ask the sender not to perform its default behaviour.
    prevent_default = function() {
      private$.default_prevented <- TRUE
      invisible(self)
    },

    #' @description One-line description.
    #' @param ... Ignored.
    format = function(...) {
      sender <- if (is.null(self$sender)) "" else paste0(" from ", self$sender$format())
      paste0("<", class(self)[[1]], " ", self$type, private$details(), sender, ">")
    },

    #' @description Print the event.
    #' @param ... Ignored.
    print = function(...) {
      cat(self$format(), "\n", sep = "")
      invisible(self)
    }
  ),
  active = list(
    #' @field stopped Has propagation been stopped?
    stopped = function(value) if (missing(value)) private$.stopped else read_only("stopped"),
    #' @field default_prevented Was `prevent_default()` called?
    default_prevented = function(value) if (missing(value)) private$.default_prevented else read_only("default_prevented")
  ),
  private = list(
    .stopped = FALSE,
    .default_prevented = FALSE,
    details = function() ""
  )
)

#' @rdname Event
#' @export
KeyEvent <- R6::R6Class(
  "KeyEvent",
  inherit = Event,
  public = list(
    #' @field key Canonical key name, e.g. `"a"`, `"enter"`, `"ctrl+c"`.
    key = NULL,
    #' @field char The printable character, or `""`.
    char = "",
    #' @field ctrl,alt,shift Modifier flags.
    ctrl = FALSE,
    alt = FALSE,
    shift = FALSE,

    #' @description Create a key event.
    #' @param key Key name (normalised).
    #' @param char Printable character, if any.
    initialize = function(key, char = NULL) {
      super$initialize("key")
      self$key <- normalize_key(key)
      mods <- split_key(self$key)$mods
      self$ctrl <- "ctrl" %in% mods
      self$alt <- "alt" %in% mods
      self$shift <- "shift" %in% mods
      self$char <- char %||% key_char(self$key)
    },

    #' @description Is this a printable character without ctrl/alt?
    is_printable = function() nzchar(self$char) && !self$ctrl && !self$alt
  ),
  private = list(details = function() paste0(" ", self$key))
)

#' @rdname Event
#' @export
ResizeEvent <- R6::R6Class(
  "ResizeEvent",
  inherit = Event,
  public = list(
    #' @field width,height New terminal size.
    width = NULL,
    height = NULL,
    #' @description Create a resize event.
    #' @param width,height Terminal size.
    initialize = function(width, height) {
      super$initialize("resize", bubbles = FALSE)
      self$width <- as.integer(width)
      self$height <- as.integer(height)
    }
  ),
  private = list(details = function() sprintf(" %dx%d", self$width, self$height))
)

#' @rdname Event
#' @export
PasteEvent <- R6::R6Class(
  "PasteEvent",
  inherit = Event,
  public = list(
    #' @field text The pasted text, normalised: line breaks are LF, tabs
    #'   are kept and other control characters are removed.
    text = "",
    #' @description Create a paste event.
    #' @param text Pasted text.
    initialize = function(text) {
      super$initialize("paste")
      self$text <- clean_paste(text)
    }
  ),
  private = list(details = function() sprintf(" (%d chars)", nchar(self$text)))
)

# Normalise pasted text: CRLF / CR -> LF, drop other control characters
# (and with them any escape sequence a paste could carry).
clean_paste <- function(x) {
  x <- enc2utf8(paste(as.character(x), collapse = ""))
  x <- gsub("\r\n?", "\n", x, perl = TRUE)
  gsub("[\001-\010\013\014\016-\037\177]", "", x, perl = TRUE)
}

#' @rdname Event
#' @export
FocusEvent <- R6::R6Class(
  "FocusEvent",
  inherit = Event,
  public = list(
    #' @description Create a focus event.
    #' @param widget The widget that received focus.
    initialize = function(widget) super$initialize("focus", sender = widget, bubbles = FALSE)
  )
)

#' @rdname Event
#' @export
BlurEvent <- R6::R6Class(
  "BlurEvent",
  inherit = Event,
  public = list(
    #' @description Create a blur event.
    #' @param widget The widget that lost focus.
    initialize = function(widget) super$initialize("blur", sender = widget, bubbles = FALSE)
  )
)

#' @rdname Event
#' @export
MountEvent <- R6::R6Class(
  "MountEvent",
  inherit = Event,
  public = list(
    #' @description Create a mount event.
    #' @param widget The mounted widget.
    initialize = function(widget) super$initialize("mount", sender = widget, bubbles = FALSE)
  )
)

#' @rdname Event
#' @export
UnmountEvent <- R6::R6Class(
  "UnmountEvent",
  inherit = Event,
  public = list(
    #' @description Create an unmount event.
    #' @param widget The removed widget.
    initialize = function(widget) super$initialize("unmount", sender = widget, bubbles = FALSE)
  )
)

#' @rdname Event
#' @export
MessageEvent <- R6::R6Class(
  "MessageEvent",
  inherit = Event,
  public = list(
    #' @description Create a custom message.
    #' @param type Message name, e.g. `"counter.changed"`.
    #' @param sender Sending widget.
    #' @param data List of values.
    #' @param bubbles Does the message bubble?
    initialize = function(type, sender = NULL, data = list(), bubbles = TRUE) {
      super$initialize(type, sender = sender, data = data, bubbles = bubbles)
    }
  )
)

#' Create a key event
#'
#' Mostly useful in tests: see [test_app()] for simulating key presses.
#'
#' @param key Key name such as `"a"`, `"enter"`, `"ctrl+c"` or
#'   `"shift+tab"`.
#' @return A `KeyEvent`.
#' @export
#' @examples
#' key_event("ctrl+c")
key_event <- function(key) KeyEvent$new(key)

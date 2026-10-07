# Small third-party style adapters. Keep all termr references namespace-qualified
# to exercise the documented extension surface rather than package internals.

external_counter <- function() {
  termr::widget(
    "ExternalCounter",
    state = list(count = termr::reactive(0L)),
    render = function(self) sprintf("Count: %d", self$count),
    bindings = list(termr::bind("up", "increment", "Increment")),
    actions = list(increment = function(self) {
      self$count <- self$count + 1L
      self$post_message("external.counter_changed", list(count = self$count))
    }),
    focusable = TRUE
  )
}

external_stack_layout <- function(name = "custom_external_stack") {
  termr::register_layout(
    name,
    arrange = function(children, inner, parent_style) {
      lapply(seq_along(children), function(i) {
        termr::region(inner$x, inner$y + i - 1L, inner$width, 1L)
      })
    },
    measure = function(children, parent_style) list(
      width = function() 1L,
      height = function(width) length(children)
    )
  )
  name
}

external_highlighter <- function(calls = new.env(parent = emptyenv())) {
  calls$n <- 0L
  fn <- function(lines, state = NULL) {
    calls$n <- calls$n + 1L
    text <- as.character(lines)[[1L]]
    match <- regexpr("SELECT", text, ignore.case = TRUE)
    hit <- match[[1L]]
    if (hit < 0L) {
      data.frame(start = integer(), end = integer(), token = character())
    } else {
      data.frame(start = hit, end = hit + attr(match, "match.length") - 1L,
                 token = "keyword")
    }
  }
  attr(fn, "calls") <- calls
  fn
}

external_memory_source <- function(data) {
  termr::table_source(
    row_count = nrow(data),
    column_names = names(data),
    get_rows = function(start, count, columns = NULL) {
      rows <- if (count == 0L || start > nrow(data)) integer() else
        seq.int(start, min(nrow(data), start + count - 1L))
      out <- data[rows, , drop = FALSE]
      if (!is.null(columns)) out <- out[, columns, drop = FALSE]
      out
    },
    row_key = function(row) data$id[[row]],
    set_value = function(row, column, value) data[[column]][[row]] <<- value
  )
}

# A source adapter backed by an ordinary DBI driver. This demonstrates that a
# third-party driver can adapt its own query strategy through table_source().
external_db_source <- function(connection, table) {
  termr::table_source(
    row_count = function() DBI::dbGetQuery(
      connection, paste("SELECT COUNT(*) AS n FROM", DBI::dbQuoteIdentifier(connection, table))
    )$n[[1L]],
    column_names = function() DBI::dbListFields(connection, table),
    get_rows = function(start, count, columns = NULL) {
      fields <- if (is.null(columns)) "*" else paste(
        vapply(DBI::dbQuoteIdentifier(connection, columns), as.character, ""), collapse = ", "
      )
      quoted_table <- as.character(DBI::dbQuoteIdentifier(connection, table))
      DBI::dbGetQuery(connection, sprintf(
        "SELECT %s FROM %s LIMIT %d OFFSET %d", fields, quoted_table, count, start - 1L
      ))
    }
  )
}

external_theme <- function() {
  termr::termr_theme("dark", accent = "#ff6600", extension_color = "#22aa88",
                     name = "external-fixture")
}

external_app <- function() {
  Counter <- external_counter()
  counter <- Counter(id = "external_counter")
  seen <- new.env(parent = emptyenv())
  seen$count <- NULL
  application <- termr::app(
    counter,
    termr::on("external.counter_changed", "#external_counter", function(event, app) {
      seen$count <- event$data$count
    }),
    theme = external_theme()
  )
  application$add_command(termr::command(
    "External action", action = function(app) app$query_one("#external_counter")$run_action("increment"),
    category = "Extension"
  ))
  list(app = application, counter = counter, seen = seen)
}

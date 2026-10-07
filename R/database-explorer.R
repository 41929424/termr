# DBI metadata and the composed database explorer.

#' Create a DBI metadata interface
#'
#' The returned functions use DBI's portable table and field listing. SQLite
#' additionally exposes views from its catalog. Driver packages can use this
#' small callback protocol when building their own metadata views; the
#' explorer does not yet have an adapter registry.
#'
#' @param connection A [db_connection()] wrapper or open DBI connection.
#' @return A list with `schemas()`, `tables(schema)`, `views(schema)`,
#'   `columns(table, schema)`, and `quote(identifier)` functions.
#' @export
db_metadata <- function(connection) {
  if (!requireNamespace("DBI", quietly = TRUE)) {
    stop("Database support requires the DBI package.", call. = FALSE)
  }
  db <- if (inherits(connection, "TermrDBConnection")) connection else db_connection(connection)
  if (!db$is_valid()) stop("The database connection is not valid.", call. = FALSE)
  con <- db$con
  is_sqlite <- inherits(con, "SQLiteConnection")
  list(
    schemas = function() character(),
    tables = function(schema = NULL) {
      if (is_sqlite) {
        out <- DBI::dbGetQuery(con,
          "SELECT name FROM sqlite_master WHERE type = 'table' AND name NOT LIKE 'sqlite_%' ORDER BY name")[[1L]]
        return(as.character(out))
      }
      as.character(DBI::dbListTables(con))
    },
    views = function(schema = NULL) {
      if (!is_sqlite) return(character())
      as.character(DBI::dbGetQuery(con,
        "SELECT name FROM sqlite_master WHERE type = 'view' ORDER BY name")[[1L]])
    },
    columns = function(table, schema = NULL) as.character(DBI::dbListFields(con, table)),
    quote = function(identifier) as.character(DBI::dbQuoteIdentifier(con, identifier))
  )
}

#' Database explorer widget
#'
#' Composes a lazy object tree, SQLite table preview, SQL editor and result
#' table, details and in-memory query history. The object tree loads metadata
#' only when its Tables or Views node is expanded. DBI/RSQLite remain optional
#' dependencies.
#'
#' @param connection A [db_connection()] wrapper or open DBI connection.
#'   Raw connections are treated as externally owned.
#' @param id Optional widget identifier.
#' @return A `DatabaseExplorer` widget with `$refresh()`, `$run_query()`,
#'   `$open_table()`, and `$query_history()` methods.
#' @export
#' @examples
#' if (requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)) {
#'   con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   DBI::dbWriteTable(con, "mtcars", mtcars)
#'   app(db_explorer(db_connection(con, owned = TRUE)))
#' }
db_explorer <- function(connection, id = NULL) {
  DatabaseExplorer$new(connection = connection, id = id)
}

DatabaseExplorer <- R6::R6Class(
  "DatabaseExplorer",
  inherit = Widget,
  public = list(
    connection = NULL,
    object_search = NULL,
    objects = NULL,
    tabs = NULL,
    sql = NULL,
    preview = NULL,
    query_results = NULL,
    details = NULL,
    history = NULL,
    status = NULL,

    initialize = function(connection, id = NULL) {
      self$connection <- if (inherits(connection, "TermrDBConnection")) connection else
        db_connection(connection, owned = FALSE)
      private$.metadata <- db_metadata(self$connection)
      private$.metadata_cache <- new.env(parent = emptyenv())
      private$.history <- list()
      private$.selected <- NULL
      private$.filter <- ""

      root <- private$build_root()
      self$objects <- tree_view(root, show_root = FALSE, id = "db_objects")
      self$object_search <- input("", placeholder = "Filter tables and views", id = "db_object_search")

      self$preview <- data_table(data.frame(), id = "db_preview")
      self$query_results <- data_table(data.frame(), id = "db_query_results")
      self$sql <- sql_editor("SELECT * FROM ", id = "db_sql", connection = self$connection,
                             result_mode = if (inherits(self$connection$con, "SQLiteConnection")) {
                               "lazy"
                             } else {
                               "data"
                             })
      self$details <- label("Select a table or view to see its columns.", id = "db_details")
      self$history <- label("No queries run in this session.", id = "db_history")
      self$status <- label(private$status_text(), id = "db_status",
                           style = style(height = 1, foreground = "$muted"))
      preview_tab <- tab("Preview", self$preview, id = "db_preview_tab")
      sql_tab <- tab("SQL", vertical(self$sql, self$query_results, id = "db_sql_view"), id = "db_sql_tab")
      self$tabs <- tabs(preview_tab, sql_tab,
                        tab("Details", self$details, id = "db_details_tab"),
                        tab("History", self$history, id = "db_history_tab"),
                        active = "db_preview_tab", id = "db_tabs")
      content <- vertical(
        split_pane(vertical(self$object_search, self$objects, id = "db_browser"), self$tabs,
                   direction = "horizontal", ratio = 0.25,
                   id = "db_split"),
        self$status,
        id = "db_explorer_layout"
      )
      super$initialize(content, id = id)
      self$on("tree.node_selected", function(event, app) {
        data <- event$data$data
        if (is.list(data) && identical(data$kind, "object")) self$open_table(data$name, data$type)
      }, selector = "#db_objects")
      self$on("sql.query_started", function(event, app) {
        private$.running <- TRUE
        private$set_status("Running SQL query...")
      }, selector = "#db_sql")
      self$on("sql.query_completed", function(event, app) private$query_completed(event$data),
              selector = "#db_sql")
      self$on("sql.query_failed", function(event, app) private$query_failed(event$data),
              selector = "#db_sql")
      self$on("datatable.source_error", function(event, app) {
        private$set_status(paste0("Query result fetch failed: ", event$data$message), "error")
      }, selector = "#db_query_results")
      self$on("input.changed", function(event, app) private$filter_objects(self$object_search$value),
              selector = "#db_object_search")
    },

    default_style = function() style(width = "1fr", height = "1fr", layout = "vertical"),
    default_bindings = function() list(
      bind("f5", "refresh_database", "Refresh database"),
      bind("ctrl+shift+o", "open_table_in_sql", "Open table in SQL"),
      bind("ctrl+shift+c", "copy_object_name", "Copy object name"),
      bind("ctrl+shift+i", "insert_object_name", "Insert object name into SQL")
    ),

    refresh = function() {
      private$.metadata_cache <- new.env(parent = emptyenv())
      private$.selected <- NULL
      if (!self$connection$is_valid()) {
        self$preview$close()
        self$preview$set_data(data.frame())
        self$query_results$close()
        self$query_results$set_data(data.frame())
        private$set_status("Disconnected")
        self$sql$disabled <- TRUE
        return(invisible(FALSE))
      }
      self$sql$disabled <- FALSE
      private$replace_root(private$build_root(private$.filter))
      self$preview$close()
      self$preview$set_data(data.frame())
      self$details$text <- "Select a table or view to see its columns."
      private$set_status("Connected; metadata refreshed")
      invisible(TRUE)
    },

    open_table = function(table, type = "table") {
      check_scalar_character(table, "table")
      type <- check_choice(type, c("table", "view"), "type")
      if (!self$connection$is_valid()) {
        self$sql$disabled <- TRUE
        private$set_status("Disconnected")
        return(invisible(FALSE))
      }
      private$.selected <- list(name = table, type = type)
      self$preview$close()
      self$preview$set_data(data.frame())
      fields <- tryCatch(private$cached_fields(table), error = function(e) e)
      if (inherits(fields, "error")) {
        private$set_status(paste0("Unable to list fields for ", table, ": ", conditionMessage(fields)), "error")
        self$details$text <- paste0("Unable to list fields for ", table, ":\n", conditionMessage(fields))
        return(invisible(FALSE))
      }
      detail <- paste0("Name: ", table, "\nType: ", type, "\nRows: unknown\nColumns: ",
                       if (length(fields)) paste(fields, collapse = ", ") else "(none)")
      self$details$text <- detail
      if (!inherits(self$connection$con, "SQLiteConnection")) {
        private$set_status("Table metadata loaded; lazy preview adapter unavailable for this DBI driver")
        return(invisible(FALSE))
      }
      source <- tryCatch(db_table_source(self$connection$con, table), error = function(e) e)
      if (inherits(source, "error")) {
        private$set_status(paste0("Unable to preview ", table, ": ", conditionMessage(source)), "error")
        return(invisible(FALSE))
      }
      self$preview$set_data(source)
      detail <- paste0("Name: ", table, "\nType: ", type, "\nRows: ", format(self$preview$row_count),
                       "\nColumns: ", if (length(fields)) paste(fields, collapse = ", ") else "(none)")
      self$details$text <- detail
      private$set_status(paste0(type, " ", table, " | ", self$preview$row_count, " rows"))
      invisible(TRUE)
    },

    run_query = function() {
      if (!self$connection$is_valid()) {
        self$sql$disabled <- TRUE
        private$set_status("Disconnected")
        return(invisible(list(ok = FALSE, message = "The database connection is not valid.")))
      }
      out <- self$sql$execute()
      if (is.null(self$app) && isTRUE(out$ok)) private$query_completed(out)
      if (is.null(self$app) && !isTRUE(out$ok)) private$query_failed(out)
      invisible(out)
    },

    query_history = function() private$.history,
    action_refresh_database = function() self$refresh(),
    action_open_table_in_sql = function() {
      selected <- private$.selected
      if (is.null(selected)) return(invisible(FALSE))
      quoted <- tryCatch(private$.metadata$quote(selected$name), error = function(e) e)
      if (inherits(quoted, "error")) {
        private$set_status(conditionMessage(quoted), "error")
        return(invisible(FALSE))
      }
      self$sql$set_text(paste0("SELECT *\nFROM ", quoted))
      self$tabs$activate("db_sql_tab")
      invisible(TRUE)
    },
    action_copy_object_name = function() {
      selected <- private$.selected
      if (is.null(selected)) return(invisible(FALSE))
      if (!is.null(self$app)) self$app$clipboard_write(selected$name)
      invisible(selected$name)
    },
    action_insert_object_name = function() {
      selected <- private$.selected
      if (is.null(selected)) return(invisible(FALSE))
      quoted <- tryCatch(private$.metadata$quote(selected$name), error = function(e) e)
      if (inherits(quoted, "error")) {
        private$set_status(conditionMessage(quoted), "error")
        return(invisible(FALSE))
      }
      self$sql$insert(quoted)
      self$tabs$activate("db_sql_tab")
      invisible(TRUE)
    },
    on_app_shutdown = function() {
      self$preview$close()
      self$query_results$close()
      if (isTRUE(self$connection$owned)) self$connection$disconnect()
      invisible(NULL)
    }
  ),
  private = list(
    .metadata = NULL,
    .metadata_cache = NULL,
    .history = NULL,
    .selected = NULL,
    .filter = "",
    .running = FALSE,

    set_status = function(text, severity = "info") {
      self$status$text <- text
      self$status$style <- style(height = 1, foreground = if (identical(severity, "error")) "red" else "$muted")
      if (identical(severity, "error") && !is.null(self$app)) self$app$notify(text, severity = "error")
      invisible(text)
    },
    status_text = function() {
      if (!is.null(self$connection) && !self$connection$is_valid()) return("Disconnected")
      paste0("Connected: ", self$connection$name)
    },
    load_objects = function(type) {
      if (!self$connection$is_valid()) {
        private$set_status("Disconnected")
        return(list(tree_node("(disconnected)")))
      }
      names <- tryCatch(private$object_names(type), error = function(e) e)
      if (inherits(names, "error")) {
        message <- paste0("Unable to list ", type, "s: ", conditionMessage(names))
        private$set_status(message, "error")
        return(list(tree_node(message)))
      }
      private$nodes_for_names(names, type)
    },
    object_names = function(type) {
      cache_key <- paste0(type, "s")
      if (exists(cache_key, private$.metadata_cache, inherits = FALSE)) {
        names <- get(cache_key, private$.metadata_cache, inherits = FALSE)
      } else {
        loader <- if (identical(type, "table")) private$.metadata$tables else private$.metadata$views
        names <- loader()
        names <- sort(unique(as.character(names)))
        assign(cache_key, names, private$.metadata_cache)
      }
      names
    },
    nodes_for_names = function(names, type) {
      if (!length(names)) return(list(tree_node(paste0("(no ", type, "s)"))))
      lapply(names, function(name) tree_node(name,
        data = list(kind = "object", name = name, type = type), id = paste0("db_", type, "_", make.names(name))))
    },
    build_root = function(filter = "") {
      root <- tree_node("Database", id = "db_root")
      for (type in c("table", "view")) {
        title <- if (type == "table") "Tables" else "Views"
        if (!nzchar(filter)) {
          child <- tree_node(title, id = paste0("db_", type, "s"),
            data = list(kind = "group", type = type),
            loader = local({ kind <- type; function(node) private$load_objects(kind) }))
        } else {
          names <- tryCatch(private$object_names(type), error = function(e) {
            private$set_status(paste0("Unable to list ", type, "s: ", conditionMessage(e)), "error")
            character()
          })
          names <- names[grepl(tolower(filter), tolower(names), fixed = TRUE)]
          child <- tree_node(title, data = list(kind = "group", type = type),
                             expanded = TRUE, id = paste0("db_", type, "s"))
          for (item in private$nodes_for_names(names, type)) child$add(item)
        }
        root$add(child)
      }
      root
    },
    replace_root = function(root) {
      old <- self$objects$root
      old$clear()
      for (node in root$children) old$add(node)
      invisible(old)
    },
    filter_objects = function(filter) {
      if (!is.character(filter) || length(filter) != 1L || is.na(filter)) return(invisible(FALSE))
      private$.filter <- filter
      if (!self$connection$is_valid()) {
        private$set_status("Disconnected")
        return(invisible(FALSE))
      }
      private$replace_root(private$build_root(filter))
      private$set_status(if (nzchar(filter)) paste0("Object filter: ", filter) else "Connected; object filter cleared")
      invisible(TRUE)
    },
    cached_fields = function(table) {
      key <- paste0("fields:", table)
      if (exists(key, private$.metadata_cache, inherits = FALSE)) return(get(key, private$.metadata_cache, inherits = FALSE))
      fields <- private$.metadata$columns(table)
      assign(key, fields, private$.metadata_cache)
      fields
    },
    set_history_text = function() {
      rows <- utils::tail(private$.history, 8L)
      if (!length(rows)) {
        self$history$text <- "No queries run in this session."
      } else {
        lines <- vapply(rev(rows), function(x) {
          sql <- gsub("\\s+", " ", trimws(x$sql))
          if (nchar(sql) > 72L) sql <- paste0(substr(sql, 1L, 69L), "...")
          paste0(if (x$ok) "\u2713 " else "! ", format(x$timestamp, "%H:%M:%S"), "  ",
                 format(round(x$elapsed_ms, 1L), nsmall = 1L), " ms  ", sql)
        }, "")
        self$history$text <- paste(lines, collapse = "\n")
      }
    },
    query_completed = function(data) {
      private$.running <- FALSE
      result <- data$result
      source <- data$source
      if (is_table_source(source)) {
        self$query_results$close()
        self$query_results$set_data(source)
      } else if (is.data.frame(result)) {
        self$query_results$close()
        self$query_results$set_data(result)
      } else {
        self$query_results$close()
        self$query_results$set_data(data.frame())
      }
      rows <- if (is.data.frame(result)) nrow(result) else as.numeric(data$rows %||% 0)
      record <- list(sql = data$sql, timestamp = Sys.time(), elapsed_ms = data$elapsed_ms %||% data$elapsed * 1000,
                     ok = TRUE, rows = rows, result_type = data$result_type %||% "data")
      private$.history[[length(private$.history) + 1L]] <- record
      private$set_history_text()
      private$set_status(paste0("Query complete | ", rows, " rows | ",
                                format(round(record$elapsed_ms, 1L), nsmall = 1L), " ms"))
      self$post_message("db_explorer.query_completed", record)
      invisible(record)
    },
    query_failed = function(data) {
      private$.running <- FALSE
      if (!self$connection$is_valid()) self$sql$disabled <- TRUE
      record <- list(sql = data$sql, timestamp = Sys.time(), elapsed_ms = data$elapsed_ms %||% 0,
                     ok = FALSE, rows = NA_integer_, error = data$message %||% "Query failed")
      private$.history[[length(private$.history) + 1L]] <- record
      private$set_history_text()
      private$set_status(paste0("Query failed: ", record$error), "error")
      self$post_message("db_explorer.query_failed", record)
      invisible(record)
    }
  )
)

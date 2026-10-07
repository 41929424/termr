#' SQL syntax highlighter
#'
#' A tolerant, line-oriented tokenizer for SQL text areas. `dialect` is
#' reserved for small dialect-specific extensions; token classes are shared.
#' @param dialect SQL dialect: `generic`, `postgres`, `sqlite`, `mysql`, or
#'   `sqlserver`.
#' @return A highlighter function for [text_area()].
#' @export
sql_highlighter <- function(dialect = c("generic", "postgres", "sqlite", "mysql", "sqlserver")) {
  dialect <- match.arg(dialect)
  keywords <- c(
    "SELECT", "FROM", "WHERE", "JOIN", "LEFT", "RIGHT", "INNER", "OUTER", "ON",
    "GROUP", "BY", "ORDER", "HAVING", "LIMIT", "OFFSET", "INSERT", "INTO", "VALUES",
    "UPDATE", "SET", "DELETE", "CREATE", "ALTER", "DROP", "TABLE", "VIEW", "WITH", "AS",
    "UNION", "ALL", "DISTINCT", "CASE", "WHEN", "THEN", "ELSE", "END", "NULL", "TRUE",
    "FALSE", "IS", "NOT", "AND", "OR", "IN", "EXISTS", "BETWEEN", "LIKE", "ASC", "DESC",
    "OVER", "PARTITION", "RETURNING", "PRIMARY", "KEY", "FOREIGN", "REFERENCES", "INDEX",
    "DEFAULT", "COUNT", "SUM", "AVG", "MIN", "MAX"
  )
  pattern <- paste0(
    "--.*|/\\*.*?(?:\\*/|$)|'(?:''|[^'])*'?|\"(?:\"\"|[^\"])*\"?|",
    "`[^`]*`?|\\[[^]]*\\]?|\\?|:[\\p{L}_][\\p{L}\\p{N}_]*|",
    "@[\\p{L}_][\\p{L}\\p{N}_]*|\\$[0-9]+|",
    "(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)(?:[eE][+-]?[0-9]+)?|",
    "[\\p{L}_][\\p{L}\\p{N}_$]*|->>|::|<=|>=|<>|!=|==|\\|\\||&&|->|:=|",
    "[-+*/%=<>~^]|[(),.;]"
  )

  tokenize_line <- function(line, in_block = FALSE) {
    offset <- 0L
    text <- line
    spans <- data.frame(start = integer(), end = integer(), token = character(), stringsAsFactors = FALSE)
    if (in_block) {
      close <- regexpr("\\*/", text, perl = TRUE)[[1L]]
      if (close < 0L) {
        n <- nchar(text, type = "chars")
        if (n) spans <- data.frame(start = 1L, end = n, token = "comment")
        return(list(spans = spans, in_block = TRUE))
      }
      end <- close + 1L
      spans <- data.frame(start = 1L, end = end, token = "comment")
      text <- substring(text, end + 1L)
      offset <- end
      in_block <- FALSE
    }
    if (!nzchar(text)) return(list(spans = spans, in_block = in_block))

    locations <- gregexpr(pattern, text, perl = TRUE)[[1L]]
    if (locations[[1L]] < 0L) return(list(spans = spans, in_block = in_block))
    hits <- regmatches(text, list(locations))[[1L]]
    starts <- as.integer(locations) + offset
    ends <- starts + as.integer(attr(locations, "match.length")) - 1L
    types <- rep("operator", length(hits))
    comment <- startsWith(hits, "--") | startsWith(hits, "/*")
    string <- startsWith(hits, "'")
    quoted <- startsWith(hits, "\"") | startsWith(hits, "`") | startsWith(hits, "[")
    parameter <- hits == "?" | startsWith(hits, ":") | startsWith(hits, "@") | startsWith(hits, "$")
    number <- grepl("^(?:[0-9]|\\.)", hits)
    word <- grepl("^[\\p{L}_]", hits, perl = TRUE)
    punctuation <- grepl("^[(),.;]$", hits)
    types[comment] <- "comment"
    types[string] <- "string"
    types[quoted] <- "quoted_identifier"
    types[parameter] <- "parameter"
    types[number] <- "number"
    types[punctuation] <- "punctuation"
    if (any(word)) {
      upper <- toupper(hits[word])
      types[word] <- ifelse(upper %in% c("NULL", "TRUE", "FALSE"), "constant",
                            ifelse(upper %in% keywords, "keyword", "identifier"))
    }
    spans <- rbind(spans, data.frame(start = starts, end = ends, token = types, stringsAsFactors = FALSE))
    block <- which(startsWith(hits, "/*"))
    if (length(block)) in_block <- !endsWith(hits[[block[[length(block)]]]], "*/")
    graphemes <- split_graphemes(line)
    if (length(graphemes) && !is_ascii(line)) {
      cumulative <- cumsum(nchar(graphemes, type = "chars"))
      spans$start <- findInterval(spans$start - 1L, cumulative) + 1L
      spans$end <- findInterval(spans$end - 1L, cumulative) + 1L
    } else {
      spans$start <- as.integer(spans$start)
      spans$end <- as.integer(spans$end)
    }
    list(spans = spans, in_block = in_block)
  }

  cache <- new.env(parent = emptyenv())
  highlighter <- function(lines, state = NULL) {
    if (is.null(state) || is.null(state$line)) {
      in_block <- FALSE
      return(lapply(lines, function(line) {
        result <- tokenize_line(line, in_block)
        in_block <<- result$in_block
        result$spans
      }))
    }
    if (!identical(cache$version, state$version) || !identical(cache$lines, lines)) {
      preserve <- if (!is.null(cache$version) && !is.null(state$changed_from)) {
        min(cache$upto %||% 0L, max(0L, as.integer(state$changed_from) - 1L))
      } else 0L
      old_spans <- cache$spans
      old_after <- cache$after
      cache$version <- state$version
      cache$lines <- lines
      cache$upto <- preserve
      cache$in_block <- if (preserve > 0L) old_after[[preserve]] else FALSE
      cache$spans <- if (preserve > 0L) old_spans[seq_len(preserve)] else list()
      cache$after <- if (preserve > 0L) old_after[seq_len(preserve)] else logical()
    }
    target <- as.integer(state$line)
    while (cache$upto < target) {
      next_row <- cache$upto + 1L
      result <- tokenize_line(lines[[next_row]], cache$in_block)
      cache$spans[[next_row]] <- result$spans
      cache$after[[next_row]] <- result$in_block
      cache$in_block <- result$in_block
      cache$upto <- next_row
    }
    cache$spans[[target]]
  }
  attr(highlighter, "termr.contextual") <- TRUE
  attr(highlighter, "dialect") <- dialect
  highlighter
}

#' Wrap a DBI connection for use by termr SQL widgets
#'
#' @param con A DBI connection.
#' @param name Optional connection label.
#' @param owned Disconnect this connection when `disconnect()` is called?
#'   Defaults to `FALSE` for externally managed connections.
#' @return A lightweight connection wrapper with `query()`, `execute()`,
#'   `is_valid()`, and `disconnect()` methods.
#' @export
db_connection <- function(con, name = NULL, owned = FALSE) {
  if (!requireNamespace("DBI", quietly = TRUE)) {
    stop("Database support requires the DBI package.", call. = FALSE)
  }
  if (missing(con) || is.null(con)) stop("`con` must be a DBI connection.", call. = FALSE)
  check_flag(owned, "owned")
  if (!is.null(name)) check_scalar_character(name, "name")
  TermrDBConnection$new(con = con, name = name, owned = owned)
}

# Recent RSQLite versions reject an empty `params` list for a query without
# placeholders, so parameters are passed only when there are some.
db_get_query <- function(con, sql, params = NULL) {
  if (!length(params)) DBI::dbGetQuery(con, sql) else DBI::dbGetQuery(con, sql, params = params)
}

TermrDBConnection <- R6::R6Class(
  "TermrDBConnection",
  public = list(
    owned = FALSE,
    initialize = function(con, name = NULL, owned = FALSE) {
      private$.con <- con
      private$.name <- name %||% "Database"
      self$owned <- owned
      private$.disconnected <- FALSE
    },
    is_valid = function() {
      if (private$.disconnected || !requireNamespace("DBI", quietly = TRUE)) return(FALSE)
      tryCatch(isTRUE(DBI::dbIsValid(self$con)), error = function(e) FALSE)
    },
    query = function(sql, params = NULL) {
      private$check_ready(sql)
      db_get_query(self$con, sql, params)
    },
    execute = function(sql, params = NULL) {
      private$check_ready(sql)
      if (!length(params)) DBI::dbExecute(self$con, sql) else DBI::dbExecute(self$con, sql, params = params)
    },
    disconnect = function(force = FALSE) {
      check_flag(force, "force")
      if (!self$is_valid()) return(invisible(FALSE))
      if (!self$owned && !force) return(invisible(FALSE))
      DBI::dbDisconnect(self$con)
      private$.disconnected <- TRUE
      invisible(TRUE)
    }
  ),
  active = list(
    con = function(value) if (missing(value)) private$.con else read_only("con"),
    name = function(value) if (missing(value)) private$.name else read_only("name")
  ),
  private = list(
    .con = NULL, .name = NULL, .disconnected = FALSE,
    check_ready = function(sql) {
      if (!requireNamespace("DBI", quietly = TRUE)) stop("Database support requires the DBI package.", call. = FALSE)
      check_scalar_character(sql, "sql")
      if (!self$is_valid()) stop("The database connection is not valid.", call. = FALSE)
      invisible(TRUE)
    }
  )
)

#' SQL editor widget
#'
#' A thin [text_area()] subclass with SQL highlighting. When `connection` is
#' supplied, `execute_key` runs the selected SQL, or the full buffer if nothing
#' is selected. Queries run synchronously in the current R process.
#' @param value Initial SQL text.
#' @param id Optional widget id.
#' @param connection A [db_connection()] wrapper or raw DBI connection.
#' @param execute_key Key binding used to execute SQL.
#' @param execution Either `"query"` (`DBI::dbGetQuery`) or `"execute"`
#'   (`DBI::dbExecute`).
#' @param line_numbers Show line numbers? Defaults to `TRUE`.
#' @param tab_size Spaces inserted by Tab.
#' @param ... Additional arguments passed to [TextArea].
#' @param result_mode Query result mode: "data" materializes the result
#'   (the default); "lazy" returns a [db_query_source()] for a paged table.
#'   Lazy mode is available only with execution = "query".
#' @param query_adapter Optional backend pager callbacks passed to
#'   [db_query_source()].
#' @return A `TextArea` subclass with an `execute()` method.
#' @export
sql_editor <- function(value = "", id = NULL, connection = NULL, execute_key = "ctrl+enter",
                       execution = c("query", "execute"), line_numbers = TRUE, tab_size = 2L, ...,
                       result_mode = c("data", "lazy"), query_adapter = NULL) {
  SQLEditor$new(value = value, id = id, connection = connection, execute_key = execute_key,
                execution = match.arg(execution), line_numbers = line_numbers, tab_size = tab_size,
                result_mode = match.arg(result_mode), query_adapter = query_adapter, ...)
}

SQLEditor <- R6::R6Class(
  "SQLEditor", inherit = TextArea,
  public = list(
    connection = NULL,
    execution = "query",
    result_mode = "data",
    query_adapter = NULL,
    initialize = function(value = "", id = NULL, connection = NULL, execute_key = "ctrl+enter",
                          execution = "query", line_numbers = TRUE, tab_size = 2L,
                          result_mode = "data", query_adapter = NULL, ...) {
      check_scalar_character(execute_key, "execute_key")
      self$connection <- if (is.null(connection) || inherits(connection, "TermrDBConnection")) connection else db_connection(connection)
      self$execution <- check_choice(execution, c("query", "execute"), "execution")
      self$result_mode <- check_choice(result_mode, c("data", "lazy"), "result_mode")
      if (identical(self$result_mode, "lazy") && !identical(self$execution, "query")) {
        stop("result_mode = 'lazy' requires execution = 'query'.", call. = FALSE)
      }
      if (!is.null(query_adapter) && !is.list(query_adapter)) {
        stop("query_adapter must be NULL or a DBI query pager adapter list.", call. = FALSE)
      }
      self$query_adapter <- query_adapter
      super$initialize(value = value, id = id, line_numbers = line_numbers, tab_behavior = "indent",
                       tab_size = tab_size, highlighter = sql_highlighter(), ...)
      if (!is.null(self$connection)) self$bind(execute_key, "sql_execute", "Execute SQL")
    },
    execute = function() {
      sql <- self$selection
      if (!nzchar(trimws(sql))) sql <- self$value
      if (is.null(self$connection)) {
        condition <- simpleError("SQL execution requires a database connection.")
        self$post_message("sql.query_failed", list(sql = sql, error = condition, message = conditionMessage(condition)))
        return(invisible(list(ok = FALSE, sql = sql, error = condition, message = conditionMessage(condition))))
      }
      started <- proc.time()[["elapsed"]]
      self$post_message("sql.query_started", list(sql = sql))
      tryCatch({
        lazy <- identical(self$result_mode, "lazy")
        result <- if (identical(self$execution, "query")) {
          if (lazy) NULL else self$connection$query(sql)
        } else self$connection$execute(sql)
        source <- if (lazy) db_query_source(self$connection, sql, adapter = self$query_adapter) else NULL
        elapsed <- proc.time()[["elapsed"]] - started
        rows <- if (!is.null(source)) source$row_count() else if (is.data.frame(result)) nrow(result) else as.numeric(result)
        data <- list(sql = sql, elapsed = elapsed, elapsed_ms = elapsed * 1000, rows = rows,
                     result = result, result_type = if (is.null(source)) "data" else "source",
                     source = source)
        self$post_message("sql.query_completed", data)
        invisible(c(list(ok = TRUE), data))
      }, error = function(e) {
        elapsed <- proc.time()[["elapsed"]] - started
        data <- list(sql = sql, elapsed = elapsed, elapsed_ms = elapsed * 1000,
                     error = e, message = conditionMessage(e))
        self$post_message("sql.query_failed", data)
        invisible(c(list(ok = FALSE), data))
      })
    },
    run_query = function() self$execute(),
    action_sql_execute = function() self$execute(),
    on_app_shutdown = function() {
      if (!is.null(self$connection) && isTRUE(self$connection$owned)) self$connection$disconnect()
      invisible(NULL)
    }
  )
)

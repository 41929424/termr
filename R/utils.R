`%||%` <- function(x, y) if (is.null(x)) y else x

# Call `fn` with only as many positional arguments as it accepts. This lets
# users write handlers as `function(event)` or `function(event, app)`.
call_flex <- function(fn, ...) {
  args <- list(...)
  if (!is.primitive(fn)) {
    fmls <- names(formals(fn))
    if (!("..." %in% fmls)) args <- args[seq_len(min(length(args), length(fmls)))]
  }
  do.call(fn, args)
}

is_scalar_character <- function(x) is.character(x) && length(x) == 1L && !is.na(x)

is_scalar_number <- function(x) is.numeric(x) && length(x) == 1L && !is.na(x)

check_scalar_character <- function(x, arg = deparse(substitute(x)), allow_null = FALSE) {
  if (allow_null && is.null(x)) return(invisible(x))
  if (!is_scalar_character(x)) {
    stop(sprintf("`%s` must be a single string.", arg), call. = FALSE)
  }
  invisible(x)
}

check_flag <- function(x, arg = deparse(substitute(x))) {
  if (!(is.logical(x) && length(x) == 1L && !is.na(x))) {
    stop(sprintf("`%s` must be TRUE or FALSE.", arg), call. = FALSE)
  }
  invisible(x)
}

check_function <- function(x, arg = deparse(substitute(x)), allow_null = FALSE) {
  if (allow_null && is.null(x)) return(invisible(x))
  if (!is.function(x)) {
    stop(sprintf("`%s` must be a function.", arg), call. = FALSE)
  }
  invisible(x)
}

# Monotonic wall clock in seconds.
now_seconds <- function() proc.time()[["elapsed"]]

read_only <- function(name) {
  stop(sprintf("`%s` is read-only.", name), call. = FALSE)
}

# Shared validation protocol (Input, TextArea, dropdown, forms): a validator
# is a function(value) returning NULL or TRUE (valid), FALSE (invalid) or a
# message (invalid). Returns NULL when valid, else the message.
run_validator <- function(fn, value) {
  if (is.null(fn)) return(NULL)
  msg <- fn(value)
  if (is.null(msg) || isTRUE(msg) || length(msg) == 0L) return(NULL)
  if (isFALSE(msg)) return("Invalid value")
  paste(as.character(msg), collapse = " ")
}

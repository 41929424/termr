# Opt-in frame diagnostics. No timers or counter allocations on the default
# path; a collector exists only during a frame with options(termr.profile=TRUE).
profile_add <- function(name, value = 1) {
  collector <- termr_env$profile
  if (!is.null(collector)) collector[[name]] <- (collector[[name]] %||% 0) + value
  invisible()
}

profile_start <- function() {
  collector <- new.env(parent = emptyenv())
  for (name in c("widgets_measured", "widgets_laid_out", "widgets_painted", "style_resolutions",
                 "selector_matches", "layout_ms", "paint_ms", "diff_ms", "ansi_ms")) collector[[name]] <- 0
  collector
}

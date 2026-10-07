test_that("numeric data profile agrees with base R summaries", {
  x <- c(1, 2, 3, 4, NA_real_, 6)
  profile <- data_profile(x, name = "x")
  stats <- profile$summary$stats
  expect_equal(stats$Length, length(x))
  expect_equal(stats$Missing, sum(is.na(x)))
  expect_equal(stats$Unique, length(unique(x[!is.na(x)])))
  expect_equal(stats$Mean, mean(x, na.rm = TRUE))
  expect_equal(stats$SD, stats::sd(x, na.rm = TRUE))
  expect_equal(unname(unlist(stats[c("Min", "Q25", "Median", "Q75", "Max")])),
               unname(stats::quantile(x, c(0, .25, .5, .75, 1), na.rm = TRUE, type = 7)))
  expect_length(profile$summary$chart, 8L)
  expect_true(inherits(profile, "DataProfile"))
})

test_that("data profile supports logical, character, factor, date and POSIXct vectors", {
  logical_stats <- data_profile(c(TRUE, FALSE, NA))$summary$stats
  expect_equal(logical_stats[["TRUE"]], 1)
  expect_equal(logical_stats[["FALSE"]], 1)
  expect_equal(logical_stats[["NA"]], 1)

  character_stats <- data_profile(c("apple", "pear", "apple", NA_character_))$summary$stats
  expect_equal(character_stats$Unique, 2)
  expect_match(character_stats$`Top 1`, "apple")
  factor_stats <- data_profile(factor(c("b", "a", "b")))$summary$stats
  expect_equal(factor_stats$Type, "factor")
  expect_match(factor_stats$`Top 1`, "b")

  dates <- as.Date(c("2026-10-07", "2026-10-08", NA))
  date_stats <- data_profile(dates)$summary$stats
  expect_equal(date_stats$Min, as.Date("2026-10-07"))
  times <- as.POSIXct(c("2026-10-07 00:00:00", "2026-10-09 00:00:00"), tz = "UTC")
  expect_equal(data_profile(times)$summary$stats$Median,
               as.POSIXct("2026-10-08 00:00:00", tz = "UTC"))
})

test_that("data profile handles empty and all-missing vectors and explicit samples", {
  empty <- data_profile(numeric())$summary$stats
  expect_equal(empty$Length, 0)
  expect_true(is.na(empty$Mean))
  all_na <- data_profile(c(NA_real_, NA_real_))$summary$stats
  expect_equal(all_na$Missing, 2)
  expect_true(is.na(all_na$Median))
  sampled <- data_profile(seq_len(1000000L), sample = 1000L)$summary$stats
  expect_equal(sampled$Length, 1000000)
  expect_equal(sampled$Analyzed, 1000)
  text_stats <- data_profile(rep(sprintf("value-%06d", 1:100001), 1L))$summary$stats
  expect_equal(text_stats$`Top values sampled`, 100000)
  expect_error(data_profile(table_source(1L, "x", function(...) data.frame(x = 1L))), "does not scan a lazy")
  expect_error(data_profile(1:5, sample = 0), "positive whole number")
})

test_that("data profile updates reactively and handles Unicode strings", {
  x <- signal(c("\u6771\u4eac", "A\u0301", "\U0001f642"))
  profile <- data_profile(function() x())
  expect_equal(profile$summary$stats$Unique, 3)
  x(c("na\u00efve", "\u4e16\u754c"))
  expect_equal(profile$summary$stats$Unique, 2)
  expect_no_error(render_widget(profile, 1, 1))
  expect_no_error(render_widget(profile, 80, 12))
})

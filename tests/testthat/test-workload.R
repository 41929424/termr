test_that("stress budgets retain full CI counts and bound only CRAN workloads", {
  withr::local_envvar(NOT_CRAN = NA_character_)
  expect_identical(stress_workload(1:5, 1L), 1L)
  expect_identical(stress_workload(100L, 40L), 40L)
  for (value in c("false", "FALSE", "")) {
    Sys.setenv(NOT_CRAN = value)
    expect_identical(stress_workload(100L, 40L), 40L)
  }
  for (value in c("true", "TRUE")) {
    Sys.setenv(NOT_CRAN = value)
    expect_identical(stress_workload(1:5, 1L), 1:5)
    expect_identical(stress_workload(100L, 40L), 100L)
  }
})

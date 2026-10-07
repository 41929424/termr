test_that("UTF-8 graphemes survive input and repaint at tiny dimensions", {
  text <- paste0("e\u0301", "\u4e2d", "\U0001F469\u200d\U0001F4BB")
  editor <- input(id = "utf8")
  pilot <- test_app(app(editor), width = 1, height = 1)
  on.exit(pilot$stop(), add = TRUE)

  pilot$type(text)
  expect_identical(editor$value, text)
  expect_identical(length(text_cells(editor$value)$chars), 3L)
  expect_no_error(pilot$resize(1, 1))
  expect_identical(c(pilot$app$frame$width, pilot$app$frame$height), c(1L, 1L))
})

test_that("very wide terminal frames and resize storms settle on the final size", {
  pilot <- test_app(app(label("wide terminal")), width = 80, height = 24)
  on.exit(pilot$stop(), add = TRUE)

  pilot$resize(4096, 2)
  expect_identical(c(pilot$app$frame$width, pilot$app$frame$height), c(4096L, 2L))
  sizes <- rbind(
    c(1L, 1L), c(200L, 50L), c(2L, 1L), c(1024L, 32L),
    c(1L, 1L), c(4096L, 2L), c(37L, 9L)
  )
  for (i in seq_len(nrow(sizes))) {
    expect_no_error(pilot$resize(sizes[i, 1L], sizes[i, 2L]))
  }
  expect_identical(pilot$app$size, c(width = 37L, height = 9L))
  expect_identical(c(pilot$app$frame$width, pilot$app$frame$height), c(37L, 9L))
})

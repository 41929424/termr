test_that("a new buffer is blank", {
  buf <- screen_buffer(4, 2)
  expect_identical(buf$to_text(), c("    ", "    "))
  expect_identical(buf$get_cell(1, 1)$char, " ")
  expect_error(buf$get_cell(5, 1), "outside")
})

test_that("put_text writes styled cells", {
  buf <- screen_buffer(8, 1)
  nxt <- buf$put_text(2, 1, "abc", fg = "red", bg = "blue", attrs = attrs_encode(bold = TRUE))
  expect_identical(nxt, 5L)
  expect_identical(buf$to_text(), " abc    ")
  cell <- buf$get_cell(3, 1)
  expect_identical(cell$char, "b")
  expect_identical(cell$fg, "red")
  expect_identical(cell$bg, "blue")
  expect_true(attrs_decode(cell$attrs)[["bold"]])
})

test_that("put_text with bg = NULL keeps the existing background", {
  buf <- screen_buffer(5, 1)
  buf$fill(bg = "blue")
  buf$put_text(1, 1, "hi", fg = "white")
  expect_identical(buf$get_cell(1, 1)$bg, "blue")
})

test_that("put_text clips to the buffer and to a clip rect", {
  buf <- screen_buffer(5, 2)
  buf$put_text(4, 1, "abcdef")
  expect_identical(buf$to_text()[[1]], "   ab")
  buf$put_text(-1, 2, "abcdef")
  expect_identical(buf$to_text()[[2]], "cdef ")
  buf2 <- screen_buffer(6, 1)
  buf2$put_text(1, 1, "abcdef", clip = rect(2, 1, 3, 1))
  expect_identical(buf2$to_text(), " bcd  ")
  buf2$put_text(1, 5, "zzz")
  expect_identical(buf2$to_text(), " bcd  ")
})

test_that("wide characters occupy two cells", {
  buf <- screen_buffer(6, 1)
  buf$put_text(1, 1, "\u4e2d\u6587")
  expect_identical(buf$chars[1, ], c("\u4e2d", "", "\u6587", "", " ", " "))
  expect_identical(buf$to_text(), "\u4e2d\u6587  ")
})

test_that("overwriting half of a wide character breaks it cleanly", {
  buf <- screen_buffer(4, 1)
  buf$put_text(1, 1, "\u4e2d")
  buf$put_text(2, 1, "x")
  expect_identical(buf$chars[1, ], c(" ", "x", " ", " "))
  buf$put_text(1, 1, "\u4e2d")
  buf$put_text(1, 1, "y")
  expect_identical(buf$chars[1, ], c("y", " ", " ", " "))
})

test_that("wide characters cut by the clip edge become spaces", {
  buf <- screen_buffer(5, 1)
  buf$fill(char = ".")
  buf$put_text(1, 1, "a\u4e2db", clip = rect(1, 1, 2, 1))
  expect_identical(buf$to_text(), "a ...")
  buf$fill(char = ".")
  buf$put_text(1, 1, "\u4e2db", clip = rect(2, 1, 4, 1))
  expect_identical(buf$to_text(), ". b..")
})

test_that("fill paints a region and set_cell writes single cells", {
  buf <- screen_buffer(4, 3)
  buf$fill(rect(2, 2, 2, 2), char = "#", bg = "red")
  expect_identical(buf$to_text(), c("    ", " ## ", " ## "))
  expect_identical(buf$get_cell(3, 3)$bg, "red")
  buf$set_cell(1, 1, cell("Z", fg = "green"))
  expect_identical(buf$get_cell(1, 1)$fg, "green")
  buf$set_cell(3, 1, cell("\u4e2d"))
  expect_identical(buf$chars[1, 3:4], c("\u4e2d", ""))
})

test_that("resize keeps top-left content", {
  buf <- screen_buffer(4, 2)
  buf$put_text(1, 1, "abcd")
  buf$put_text(1, 2, "ef")
  buf$resize(6, 1)
  expect_identical(buf$to_text(), "abcd  ")
  buf$put_text(1, 1, "ab\u4e2d")
  buf$resize(3, 1)
  expect_identical(buf$chars[1, ], c("a", "b", " "))
})

test_that("copy is independent and equals compares content", {
  buf <- screen_buffer(3, 1)
  other <- buf$copy()
  expect_true(buf$equals(other))
  other$put_text(1, 1, "x")
  expect_false(buf$equals(other))
  expect_identical(buf$to_text(), "   ")
})

test_that("uint32 helper fields retain their exact unsigned values", {
  text <- c("0", "1", "2147483647", "2147483648", "4294967295")
  values <- c(0, 1, 2147483647, 2147483648, 4294967295)
  for (i in seq_along(text)) {
    expect_silent(value <- parse_uint32(text[[i]]))
    expect_identical(value, values[[i]])
  }
  expect_identical(vapply(values, signed_high_word, 0), c(0, 0, 32767, -32768, -1))
})

test_that("Windows console writes keep large UTF-8 frames in bounded pieces", {
  # The native Rterm/UCRT crash occurred in fwrite() with a multi-KiB frame.
  frame <- paste0("\033[?2026h", strrep("\u2502\U0001f642", 1500), "\033[?2026l")
  pieces <- character()
  write_windows_stdout(frame, writer = function(part) {
    pieces <<- c(pieces, part)
    invisible(NULL)
  })
  expect_identical(paste0(pieces, collapse = ""), frame)
  expect_gt(length(pieces), 1L)
  expect_true(all(nchar(pieces, type = "bytes") <= 4096L))
  expect_true(all(vapply(pieces, function(part) !is.na(iconv(part, "UTF-8", "UTF-8", sub = NA)), logical(1))))
})

test_that("invalid uint32 fields are rejected without coercion warnings", {
  invalid <- list("", "NA", "NaN", "Inf", "-1", "-0", "4294967296",
                  "2147483648.5", "1.00000000000000001", "1e2", " 1", "1 ",
                  "text", strrep("9", 400), NA_character_, character(), c("0", "1"))
  for (text in invalid) {
    expect_silent(expect_error(parse_uint32(text), "input helper: invalid uint32"))
  }
})

test_that("Windows wheel DWORDs decode signed high words on both axes", {
  deltas <- c(120, -120, 32767, -32768, 1, -1)
  for (delta in deltas) {
    # Keep low-word button bits set to verify they do not affect the delta.
    buttons <- (delta %% 65536) * 65536 + 7
    expect_identical(signed_high_word(buttons), delta)
    for (flags in c(4, 8, 2147483652, 2147483656)) {
      expect_silent(events <- windows_mouse_events(5L, 3L, buttons, flags, 7L, 1L))
      expect_length(events, 1)
      event <- events[[1]]
      expect_identical(event$type, "mouse.scroll")
      expected <- if (flags %% 65536 == 4) {
        if (delta > 0) "up" else "down"
      } else {
        if (delta > 0) "right" else "left"
      }
      expect_identical(event$direction, expected)
      expect_true(event$alt && event$shift && event$ctrl)
      expect_identical(c(event$screen_x, event$screen_y), c(5L, 3L))
    }
  }
})

test_that("the Windows driver parses unsigned helper mouse lines and tracks buttons", {
  driver <- WindowsDriver$new(color_mode = "16")
  private <- driver$.__enclos_env__$private
  parse <- private$parse_line
  expect_silent(down <- parse("M\t5\t3\t1\t0\t0"))
  expect_identical(down[[1]]$type, "mouse.down")
  expect_identical(down[[1]]$button, "left")
  expect_silent(move <- parse("M\t6\t3\t1\t1\t0"))
  expect_identical(move[[1]]$type, "mouse.move")
  expect_identical(move[[1]]$button, "left")

  # -120 as a uint32, exactly as the PowerShell helper emits it.
  expect_silent(wheel <- parse("M\t6\t3\t4287102976\t4\t7"))
  expect_identical(wheel[[1]]$direction, "down")
  expect_true(wheel[[1]]$alt && wheel[[1]]$shift && wheel[[1]]$ctrl)
  expect_identical(private$buttons, 1L) # Wheel records do not reset held buttons.
  expect_silent(wheel <- parse("M\t6\t3\t7864320\t4\t0"))
  expect_identical(wheel[[1]]$direction, "up")
  expect_silent(wheel <- parse("M\t6\t3\t4287102976\t8\t0"))
  expect_identical(wheel[[1]]$direction, "left")
  expect_silent(wheel <- parse("M\t6\t3\t7864320\t8\t0"))
  expect_identical(wheel[[1]]$direction, "right")

  expect_silent(up <- parse("M\t6\t3\t0\t0\t2"))
  expect_identical(up[[1]]$type, "mouse.up")
  expect_identical(up[[1]]$button, "left")
  expect_true(up[[1]]$shift)
  expect_identical(private$buttons, 0L)
  expect_silent(move <- parse("M\t7\t0\t0\t1\t0"))
  expect_identical(move[[1]]$button, "none")
  expect_identical(move[[1]]$screen_y, 0L) # Position outside the viewport.
})

test_that("complete helper lines accept DWORD boundaries for buttons and flags", {
  values <- c("0", "1", "2147483647", "2147483648", "4294967295")
  for (text in values) {
    driver <- WindowsDriver$new(color_mode = "16")
    private <- driver$.__enclos_env__$private
    expect_silent(events <- private$parse_line(paste("M", 1, 1, text, 0, 0, sep = "\t")))
    expect_identical(private$buttons, as.integer(as.double(text) %% 65536))
    expected <- switch(text, "0" = character(), "1" = "left",
                       "2147483647" = c("left", "right", "middle"),
                       "2147483648" = character(), "4294967295" = c("left", "right", "middle"))
    expect_identical(vapply(events, function(e) e$button, ""), expected)
    expect_silent(private$parse_line(paste("M", 1, 1, 0, text, 0, sep = "\t")))
  }
})

test_that("Windows helper key and size fields validate before changing state", {
  driver <- WindowsDriver$new(color_mode = "16")
  private <- driver$.__enclos_env__$private
  expect_silent(private$parse_line("S\t100\t30"))
  expect_identical(driver$size(), c(width = 100L, height = 30L))
  driver$started <- TRUE
  size <- private$parse_line("S\t120\t40")
  expect_s3_class(size, "ResizeEvent")
  expect_identical(private$parse_line("K\t65\t97\t0")$key, "a")
  expect_identical(private$parse_line("K\t65\t1\t4")$key, "ctrl+a")
  expect_identical(private$parse_line("K\t67\t3\t4")$key, "ctrl+c")
  expect_identical(private$parse_line("K\t65535\t65535\t0")$char, intToUtf8(65535L))
  expect_null(private$parse_line("K\t0\t55357\t0"))
  expect_identical(private$parse_line("K\t0\t56832\t0")$char, intToUtf8(0x1F600L))
})

test_that("malformed Windows helper records fail explicitly and preserve state", {
  driver <- WindowsDriver$new(color_mode = "16")
  private <- driver$.__enclos_env__$private
  private$parse_line("M\t1\t1\t1\t0\t0")
  private$parse_line("K\t0\t55357\t0")
  invalid <- c(
    "S", "S\t80", "S\t80\t24\t0", "S\t80\t24\t", "S\tNA\t24",
    "S\t0\t24", "S\t80\t2147483648",
    "K\t65\t97", "K\t65536\t97\t0", "K\t65\t65536\t0",
    "K\t65\tNA\t0", "K\t65\t97\t8", "K\t0\t55357\tNA",
    "M\t1\t1\t0\t0", "M\t1\t1\t0\t0\t0\t0", "M\t\t1\t0\t0\t0",
    "M\t2147483648\t1\t0\t0\t0", "M\t1\tNaN\t0\t0\t0",
    "M\t1\t1\t-1\t0\t0", "M\t1\t1\t4294967296\t0\t0",
    "M\t1\t1\tNA\t0\t0", "M\t1\t1\t0\t4294967296\t0",
    "M\t1\t1\t0\tNA\t0", "M\t1\t1\t0\t0\t-1",
    "M\t1\t1\t0\t0\t", "M\t1\t1\t0\t0\t1.5"
  )
  for (line in invalid) {
    expect_silent(expect_error(private$parse_line(line), "input helper: invalid"))
    expect_identical(private$buttons, 1L)
    expect_identical(private$last_size, c(width = 80L, height = 24L))
    expect_identical(private$high_surrogate, 55357L)
  }
  expect_error(private$parse_line(NA_character_), "input helper: invalid record")
  expect_error(private$parse_line("E\thelper failed"), "input helper: helper failed")
})

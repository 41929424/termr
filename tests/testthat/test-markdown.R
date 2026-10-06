md_text <- function(markdown, width = 40, height = 30) {
  text <- render_widget(markdown_view(markdown, scroll = FALSE), width, height)$to_text()
  sub("[[:space:]]+$", "", text)
}

segments <- function(markdown, width = 60) {
  lines <- markdown_lines(markdown_blocks(markdown), width)
  lines
}

test_that("blocks are parsed", {
  blocks <- markdown_blocks("# T\n\npara one\nline two\n\n- a\n- b\n\n```\ncode\n```\n\n---\n\n> q")
  types <- vapply(blocks, `[[`, "", "type")
  expect_identical(types, c("heading", "paragraph", "list", "code", "rule", "quote"))
  expect_identical(blocks[[1]]$level, 1L)
  expect_identical(blocks[[3]]$items[[2]]$text, "b")
  expect_identical(blocks[[4]]$lines, "code")
})

test_that("headings, paragraphs and wrapping", {
  out <- md_text("# Title\n\nA paragraph with enough words to wrap around the narrow view.", width = 20)
  expect_identical(out[[1]], "Title")
  expect_match(out[[2]], "^\u2550+$")
  expect_identical(out[[4]], "A paragraph with")
  expect_true(all(nchar(out) <= 20))
  expect_identical(md_text("## Two")[[1]], "Two")
})

test_that("inline styles are applied", {
  lines <- segments("plain **bold** *it* `code` ~~gone~~ [link](https://x.org)")
  segs <- lines[[1]]
  texts <- vapply(segs, `[[`, "", "text")
  expect_identical(paste(texts, collapse = ""), "plain bold it code gone link")
  find <- function(txt) segs[[which(texts == txt)]]
  expect_true(find("bold")$style$attrs > 0 || isTRUE(find("bold")$style$props$bold))
  expect_identical(find("link")$link, "https://x.org")
  expect_true(!is.null(find("code")$style))
})

test_that("nested emphasis and escapes", {
  lines <- segments("**bold and *italic* inside** and \\*not italic\\*")
  texts <- vapply(lines[[1]], `[[`, "", "text")
  expect_identical(paste(texts, collapse = ""), "bold and italic inside and *not italic*")
  expect_true(length(lines[[1]]) >= 4)
})

test_that("snake_case words and math are not italicised", {
  lines <- segments("use my_snake_case_name and 2*3*4 here")
  expect_identical(paste(vapply(lines[[1]], `[[`, "", "text"), collapse = ""), "use my_snake_case_name and 2*3*4 here")
})

test_that("lists nest and restart numbering", {
  out <- md_text("- one\n  - two\n- three\n1. first\n2. second\n\n3. third", width = 30)
  expect_identical(out[[1]], "\u2022 one")
  expect_identical(out[[2]], "  \u25e6 two")
  expect_identical(out[[3]], "\u2022 three")
  expect_identical(out[[4]], "1. first")
  expect_identical(out[[5]], "2. second")
  wrapped <- md_text("- a very long list item that must wrap onto a second line", width = 20)
  expect_match(wrapped[[2]], "^  [a-z]")
})

test_that("block quotes, rules and code blocks", {
  out <- md_text("> quoted\n\n---\n\n```r\nx <- 1\n```", width = 20)
  expect_identical(out[[1]], "\u2502 quoted")
  expect_match(out[[3]], "^\u2500{20}$")
  expect_identical(out[[5]], "  x <- 1")
  buf <- render_widget(markdown_view("```\ncode\n```", scroll = FALSE), 20, 3)
  expect_identical(buf$get_cell(3, 1)$bg, "bright_black")
})

test_that("tables are aligned", {
  out <- md_text("| name | n |\n|:--|--:|\n| a | 1 |\n| bcd | 22 |", width = 30)
  expect_identical(out[[1]], "name \u2502  n")
  expect_match(out[[2]], "^\u2500+\u253c\u2500+$")
  expect_identical(out[[3]], "a    \u2502  1")
  expect_identical(out[[4]], "bcd  \u2502 22")
})

test_that("hard line breaks and soft breaks", {
  out <- md_text("one  \ntwo\nthree", width = 40)
  expect_identical(out[1:2], c("one", "two three"))
})

test_that("unknown syntax stays plain and raw HTML is not interpreted", {
  out <- md_text("<b>bold?</b> and &amp; stay as written", width = 50)
  expect_identical(out[[1]], "<b>bold?</b> and &amp; stay as written")
})

test_that("control characters and escape sequences cannot reach the terminal", {
  hostile <- "# T\u001b[31m\n\ntext \u001b]52;c;AAAA\u0007 end\u0001\n\n```\n\u001b[2Jcode\n```\n\n[x\u001b](https://a.b/\u001b[0m)"
  a <- app(markdown_view(hostile))
  pilot <- test_app(a, 40, 12)
  out <- paste(pilot$driver$output, collapse = "")
  expect_false(grepl("\u001b[31m", out, fixed = TRUE))
  expect_false(grepl("\u001b]52", out, fixed = TRUE))
  plain <- test_app(app(markdown_view("# T

text")), 40, 12)
  count <- function(x) lengths(regmatches(x, gregexpr("[2J", x, fixed = TRUE)))
  expect_lte(count(out), count(paste(plain$driver$output, collapse = "")))
  expect_null(pilot$system_clipboard())
})

test_that("links are listed and the view scrolls inside a scroll view", {
  m <- markdown_view("see [one](https://a.org) and [two](http://b.org)", scroll = FALSE)
  expect_identical(m$links$url, c("https://a.org", "http://b.org"))
  long <- paste0("line ", 1:60, collapse = "\n\n")
  a <- app(markdown_view(long, id = "doc"))
  pilot <- test_app(a, 30, 8)
  expect_match(pilot$screen_text()[[1]], "^line 1")
  pilot$press("tab")
  pilot$press("pagedown")
  expect_false(grepl("^line 1 ", pilot$screen_text()[[1]]))
})

test_that("set_markdown replaces the document", {
  m <- markdown_view("# A", scroll = FALSE)
  expect_identical(md_text("# A")[[1]], "A")
  a <- app(m)
  pilot <- test_app(a, 20, 4)
  m$set_markdown("# B")
  pilot$step()
  expect_identical(sub(" +$", "", pilot$screen_text()[[1]]), "B")
  expect_identical(m$markdown, "# B")
})

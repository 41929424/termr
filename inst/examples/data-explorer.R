# Data explorer: filter, sort and inspect a data frame - the typical termr
# data application. Synthetic data, no packages beyond termr.
#
#   Rscript -e 'termr::run_example("data-explorer")'
#
# Keys: / search   r region filter   click a header (or s) to sort   Enter row details
#       F1 help    Ctrl+P commands   Ctrl+F find in table   q quit
# The metrics and the details pane are driven by signals: change a signal and
# everything that reads it updates.

library(termr)

set.seed(42)
n <- 20000
orders <- data.frame(
  id = seq_len(n),
  date = as.Date("2025-01-01") + sample(0:364, n, replace = TRUE),
  region = factor(sample(c("North", "South", "East", "West"), n, replace = TRUE)),
  product = sample(c("Apples", "Pears", "Plums", "Cherries", "Figs", "Dates"), n, replace = TRUE),
  units = rpois(n, 12),
  price = round(runif(n, 0.5, 9.5), 2),
  stringsAsFactors = FALSE
)
orders$revenue <- round(orders$units * orders$price, 2)
orders$status <- sample(c("shipped", "pending", "returned"), n, replace = TRUE, prob = c(0.8, 0.15, 0.05))

# State -----------------------------------------------------------------------
view_rows <- signal(seq_len(n))     # rows currently shown (row numbers of `orders`)
current <- signal(1L)               # row under the cursor
query <- signal("")
region_sel <- signal("All")

shown <- computed(function() orders[view_rows(), , drop = FALSE])
revenue <- computed(function() sum(shown()$revenue))
returned <- computed(function() mean(shown()$status == "returned") * 100)

apply_filters <- function(table) {
  rows <- seq_len(n)
  if (region_sel() != "All") rows <- rows[orders$region[rows] == region_sel()]
  q <- tolower(trimws(query()))
  if (nzchar(q)) {
    text <- paste(orders$product, orders$status, orders$region, orders$date)[rows]
    rows <- rows[grepl(q, tolower(text), fixed = TRUE)]
  }
  view_rows(rows)
  table$filter(if (length(rows) == n) NULL else rows)
  sel <- table$selected_row()
  if (!is.na(sel)) current(sel)
}

# Interface ---------------------------------------------------------------------
table <- data_table(
  orders, id = "table", cursor = "cell", zebra = TRUE, header_sort = TRUE, frozen_columns = 1,
  formatters = list(revenue = function(x) formatC(x, format = "f", digits = 2, big.mark = ","),
                    date = function(x) format(x))
)
table$set(help = "Click a header (or press **S**) to sort; *Shift+click* adds a second sort key.\nCtrl+F searches the table, Ctrl+G jumps to a row, Alt+Left/Right resize a column.")

details <- key_value(function() {
  row <- orders[current(), , drop = FALSE]
  as.list(vapply(row, function(v) format(v), ""))
}, id = "details")

summary_md <- markdown_view("", id = "summary", scroll = FALSE)

ui <- vertical(
  label("Order explorer (synthetic data)", style = style(bold = TRUE, foreground = "$accent")),
  horizontal(
    metric("Orders", function() format(length(view_rows()), big.mark = ",")),
    metric("Revenue", function() formatC(revenue(), format = "f", digits = 0, big.mark = ",")),
    metric("Returned", function() sprintf("%.1f%%", returned())),
    style = style(height = "auto")
  ),
  horizontal(
    input(placeholder = "search products, status, region...", id = "search"),
    dropdown(c("All", levels(orders$region)), value = "All", id = "region", style = style(width = 18)),
    style = style(height = "auto")
  ),
  split_pane(
    table,
    vertical(panel(details, title = "Selected order"), panel(summary_md, title = "Column", style = style(height = "1fr")),
             style = style(padding = c(0, 1))),
    ratio = 0.68, id = "split"
  ),
  label(function() sprintf("%s of %s orders   F1 help   Ctrl+P commands   q quit",
                           format(length(view_rows()), big.mark = ","), format(n, big.mark = ",")),
        id = "status", style = style(foreground = "$muted")),
  style = style(padding = c(0, 1))
)

describe_column <- function(name) {
  x <- shown()[[name]]
  if (!length(x)) return(sprintf("## %s\n\nNo rows.", name))
  if (is.numeric(x)) {
    q <- stats::quantile(x, c(0, 0.25, 0.5, 0.75, 1), names = FALSE)
    sprintf("## %s\n\n| stat | value |\n|---|--:|\n| min | %s |\n| median | %s |\n| mean | %s |\n| max | %s |", name,
            format(q[[1]]), format(q[[3]]), format(round(mean(x), 2)), format(q[[5]]))
  } else {
    counts <- utils::head(sort(table(as.character(x)), decreasing = TRUE), 5L)
    sprintf("## %s\n\n%s", name, paste0("- ", names(counts), ": ", counts, collapse = "\n"))
  }
}

explorer <- app(
  ui,
  title = "Order explorer",
  bind("q", "quit", "Quit"),
  bind("/", function(app) app$query_one("#search")$focus(), "Search"),
  bind("r", function(app) app$query_one("#region")$focus(), "Region filter"),
  on("input.submitted", "#search", function(event, app) {
    query(event$data$value)
    apply_filters(table)
    app$notify(sprintf("%s matching orders", format(length(view_rows()), big.mark = ",")), timeout = 2)
    table$focus()
  }),
  on("dropdown.changed", "#region", function(event, app) {
    region_sel(event$data$value)
    apply_filters(table)
    table$focus()
  }),
  on("datatable.cell_selected", "#table", function(event, app) {
    current(event$data$row)
    summary_md$set_markdown(describe_column(event$data$column))
  }),
  on("datatable.row_activated", "#table", function(event, app) {
    row <- orders[event$data$row, , drop = FALSE]
    app$push_screen(modal(key_value(lapply(row, format)), title = paste("Order", row$id), width = 50))
  })
)
explorer$add_command(command("Reset filters", function(app) {
  query("")
  region_sel("All")
  app$query_one("#search")$clear()
  apply_filters(table)
}, category = "Data", description = "Show every order again"))
explorer$add_command(command("Toggle high contrast", function(app) {
  app$theme <- if (app$theme$name == "high-contrast") "default" else "high-contrast"
}, category = "View"))
explorer$add_command(command("Hide the id column", function(app) table$set_column_visible("id", FALSE), category = "Data"))
explorer$call_later(function(app) {
  summary_md$set_markdown(describe_column("id"))
  table$focus()
})

run(explorer)

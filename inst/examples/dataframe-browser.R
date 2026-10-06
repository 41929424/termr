# Data frame browser: search, sort, column summaries and row details.
#
#   Rscript -e 'termr::run_example("dataframe-browser")'
#
# The same viewer is available as termr::browse_data(your_data).

library(termr)

n <- 200000
set.seed(1)
sales <- data.frame(
  id = seq_len(n),
  region = factor(sample(c("North", "South", "East", "West"), n, replace = TRUE)),
  product = sample(c("Apples", "Pears", "Plums", "Cherries", "Figs"), n, replace = TRUE),
  units = rpois(n, 20),
  price = round(runif(n, 0.5, 9.5), 2),
  date = as.Date("2025-01-01") + sample(0:364, n, replace = TRUE)
)
sales$revenue <- sales$units * sales$price

run(data_browser(sales, title = "Fruit sales (200,000 rows)"))

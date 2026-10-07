# Lightweight SQL tokenizer timing by document size.
# Run from the package root with: Rscript tools/bench/sql-highlighter.R

library(termr)

for (n in c(100L, 1000L, 10000L)) {
  lines <- rep("SELECT id, name FROM users WHERE score >= 3.14 AND active = TRUE; -- sample", n)
  highlighter <- sql_highlighter()
  elapsed <- system.time(spans <- highlighter(lines))[['elapsed']]
  cat(sprintf("%5d lines: %.3f seconds; %d spans\n",
              n, elapsed, sum(vapply(spans, nrow, integer(1)))))
}

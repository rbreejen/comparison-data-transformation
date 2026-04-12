# Data transformation tool comparison table
# Loads the generic engine and calls it with transformation-specific configuration.

list.of.packages <- c("reactable", "data.table", "here", "htmltools", "htmlwidgets", "webshot2", "stringr", "reactablefmtr", "markdown")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
Sys.setlocale("LC_TIME", "C")

source(here::here("build-table.R"))

build_comparison_table(list(
  csv_path = "comparison-data-transformation.csv",
  title    = "Feature comparison data transformation tools",
  html_out = "www/table.html",
  img_out  = "www/assets/images/img.png"
))

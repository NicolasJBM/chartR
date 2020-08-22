# Create a list of journalswhich a template is available
library(dplyr)
library(tibble)


charts <- read.csv("inst/scripts/charts.csv", stringsAsFactors = FALSE)
save(charts, file="data/charts.RData")

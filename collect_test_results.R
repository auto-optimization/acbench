source("common.R")
scenarios_file <- "scenarios.txt"
scenarios <- read_scenarios_file(scenarios_file)
# exec_dir <- "./execdir"
exec_dir <- "~/scratch/execdir"
library(data.table)

res <- collect_test_results(exec_dir, scenarios)


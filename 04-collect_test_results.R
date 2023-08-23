source("common.R")
# exec_dir <- "./execdir"
exec_dir <- "~/scratch/execdir"
scenarios_file <- "scenarios.txt"
scenarios <- read_scenarios_file(scenarios_file)
res <- collect_test_results(exec_dir, scenarios)


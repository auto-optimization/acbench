source("acbench.R")
# exec_dir <- "./execdir"
setup_file <- "sge_setup.txt"
acbench <- read_setup_file(setup_file)
scenarios <- read_scenarios_file("scenarios.txt")
res <- acbench$collect_test_results(scenarios)

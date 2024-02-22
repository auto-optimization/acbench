source("common.R")
setup_file <- "sge_setup.txt"
acbench <- read_setup_file(setup_file)
scenarios <- read_scenarios_file("scenarios.txt")
acbench$collect_train_results(scenarios, verbose = TRUE)
acbench$collect_best_confs(scenarios)

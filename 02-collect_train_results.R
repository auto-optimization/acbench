source("acbench.R")
setup_file <- "sge_setup.txt"
acbench <- read_setup_file(setup_file)
#scenarios <- read_scenarios_file("scenarios.txt")
scenarios <- acbench$saved_setup$scenarios
acbench$collect_train_results(scenarios, verbose = TRUE)
acbench$collect_best_confs(scenarios)

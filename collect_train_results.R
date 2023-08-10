source("common.R")
setup_file <- "sge_setup.txt"
acbench <- read_setup_file(setup_file)
scenarios <- read_scenarios_file("scenarios.txt")
res <- collect_train_results(acbench$exec_dir,  scenarios)
collect_best_confs(acbench$exec_dir, scenarios)

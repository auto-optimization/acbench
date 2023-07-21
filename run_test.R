source("common.R")
setup_file <- "local_setup.txt"
acbench <- read_setup_file(setup_file)
res <- collect_train_results(acbench$exec_dir,
                             acbench$saved_setup$scenarios)

#collect_best_confs(exec_dir, scenarios)
collect_best_confs(acbench$exec_dir,
                   acbench$saved_setup$scenarios)

for (scenario_name in scenarios) {
  acbench$run_test(scenario_name)
}

source("common.R")
scenarios_file <- "scenarios.txt"
scenarios <- read_scenarios_file(scenarios_file)
exec_dir <- "./execdir"
install_dir <- "./install/"
tuner <- "irace"
tuner_versions <- c("git", "3.5")
nreps <- 10

setup_future_plan()
reps <- seq_len(nreps)
for (scenario_name in scenarios) {
  run_test(scenario_name, install_dir, exec_dir, tuner, tuner_version = "git", reps)
}
# FIXME: Add append parameter
collect_best_confs(exec_dir, scenarios)
collect_test_results(exec_dir, scenarios)


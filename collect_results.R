source("common.R")
scenarios_file <- "scenarios.txt"
scenarios <- read_scenarios_file(scenarios_file)
exec_dir <- "~/scratch/execdir"
install_dir <- "~/scratch/install/"
tuner <- "irace"
tuner_versions <- c("git", "3.5")
nreps <- 10

acbench <- ACBench$new(exec_dir = exec_dir, install_dir = install_dir,
                       cluster = TRUE, ncpus = 12)

collect_best_confs(exec_dir, scenarios)

for (scenario_name in scenarios) {
  acbench$run_test(scenario_name)
}


# FIXME: Add append parameter
collect_test_results(exec_dir, scenarios)


source("common.R")
scenarios_file <- "scenarios.txt"
scenarios <- read_scenarios_file(scenarios_file)
exec_dir <- "~/scratch/execdir"
install_dir <- "~/scratch/install/"
tuner <- "irace"
tuner_versions <- c("git", "3.5")
nreps <- 10

setup_future_plan(cluster=TRUE)

for (tuner_version in tuner_versions) {
  for (scenario_name in scenarios) {
    run_scenario(scenario_name, install_dir = install_dir, exec_dir = exec_dir,
                 tuner, tuner_version, nreps)
  }
}

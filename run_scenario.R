source("common.R")
scenarios_file <- "scenarios.txt"
scenarios <- read_scenarios_file(scenarios_file)
exec_dir <- "~/scratch/execdir"
install_dir <- "~/scratch/install/"
tuner <- "irace"
tuner_versions <- c("git") #, "3.5")
#nreps <- 10
nreps <- 4:10

acbench <- ACBench$new(exec_dir = exec_dir, install_dir = install_dir,
                       cluster = TRUE, ncpus = 12)

for (tuner_version in tuner_versions) {
  for (scenario_name in scenarios) {
    acbench$run_scenario(scenario_name, tuner, tuner_version, nreps)
  }
}

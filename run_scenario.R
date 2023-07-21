source("common.R")
setup_file <- "local_setup.txt"
acbench <- read_setup_file(setup_file)
acbench$run_saved_setup()

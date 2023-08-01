source("common.R")
setup_file <- "sge_setup.txt"
acbench <- read_setup_file(setup_file)
res <- collect_train_results(acbench$exec_dir,
                             acbench$saved_setup$scenarios)

collect_best_confs(acbench$exec_dir,
                   acbench$saved_setup$scenarios, verbose = TRUE)

acbench$run_test(acbench$saved_setup$scenarios)

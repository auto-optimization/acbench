library(fs, quiet = TRUE)
library(devtools, quiet = TRUE)
library(cli, quiet = TRUE)
library(data.table, quiet = TRUE)
library(future.apply, quiet = TRUE)
library(irace)

install_irace <- function(install_dir, version, reinstall = FALSE)
{
  lib <- file.path(install_dir, paste0("irace_", version))
  if (reinstall || !fs::file_exists(lib)) {
    fs::dir_create(lib)
    if (version == "git") {
      devtools::install_github("MLopez-Ibanez/irace", upgrade = "never", lib = lib)
    } else { 
      devtools::install_version("irace", version = version, upgrade = "never", lib = lib)
    }
  }
  lib
}

get_installed_irace <- function(install_dir, version)
{
  lib <- install_irace(install_dir, version = version)
  exe <- fs::path_abs(file.path(lib, "irace", "bin", "irace"))
  if (!fs::file_access(exe, mode = "execute"))
    cli_abort("Executable {.filename {exe}} not found or not executable ")
  exe
}

run_irace <- function(exe, scenario_file, exec_dir, run)
{
  system2(exe, args = c("-s", scenario_file, "--exec-dir", exec_dir, "--seed", 42 + run),
          stdout = file.path(exec_dir, "stdout.txt"),
          stderr = file.path(exec_dir, "stderr.txt"))
}

run_irace_testing <- function(exe, scenario_file, exec_dir, confs_file)
{
  system2(exe, args = c("-s", scenario_file, "--exec-dir", exec_dir, "--seed", 42, "--only-test", confs_file),
          stdout = file.path(exec_dir, "stdout.txt"),
          stderr = file.path(exec_dir, "stderr.txt"))
}

get_tuner_executable <- function(install_dir, tool, version)
{
  switch(tool,
         "irace" = get_installed_irace(install_dir, version))
}

find_scenario <- function(scenario_name)
{
  scenario <- file.path("scenarios", paste0(scenario_name, ".txt"))
  names(scenario) <- scenario_name
  if (!fs::file_exists(scenario))
    cli_abort("Unknown scenario {.var {scenario_name}}")
  scenario
}

run_scenario <- function(scenario_name, install_dir, exec_dir, tuner, tuner_version, nreps)
{
  reps <- seq_len(nreps)
  exec_dir <- sapply(reps, function(r) {
    d <- file.path(exec_dir, scenario_name, sprintf("%s_%s-%02d", tuner, tuner_version, r))
    if (fs::file_exists(d)) {
      fs::dir_delete(d)
    }
    fs::dir_create(d)
    d
  })
  exe <- get_tuner_executable(install_dir, tuner, tuner_version)
  scenario <- find_scenario(scenario_name)
  future_mapply(run_irace, exe = exe, scenario_file = scenario, exec_dir = exec_dir, run = reps)
}

read_configurations <- function(scenario_name, file = "best_confs.rds",
                                metadata = TRUE)
{
  x <- readRDS(file)[[scenario_name]]
  if (metadata) return(x)
  drop_cols <- colnames(x)
  drop_cols <- drop_cols[startsWith(drop_cols, ".")]
  x[, (drop_cols):=NULL]
}

run_test <- function(scenario_name, install_dir, exec_dir, tuner, tuner_version, reps)
{
  exec_dir <- file.path(exec_dir, scenario_name, sprintf("%s_%s-%02d", tuner, tuner_version, reps))
  exe <- get_tuner_executable(install_dir, tuner, tuner_version)
  scenario <- find_scenario(scenario_name)
  confs <- read_configurations(scenario_name, metadata = FALSE)
  confs_file <- tempfile(pattern = scenario_name, fileext = ".txt")
  cat(confs_file, "\n")
  write.table(confs, file = confs_file, row.names = FALSE)  
  future_mapply(run_irace_testing, exe = exe, scenario_file = scenario, exec_dir = exec_dir, confs_file = confs_file)
}

  
read_scenarios_file <- function(file)
  scan(scenarios_file, what = character(), strip.white=TRUE, comment.char = "#", quiet = TRUE)


read_completed_logfile <- function(p)
{
  p <- file.path(p, "irace.Rdata")
  if (!fs::file_exists(p)) cli_abort("File {.filename {p}} not found !")
  res <- read_logfile(p)
  if (!is.null(res$state$completed) && res$state$completed == "Incomplete")
    cli_abort("File {.filename {p}} does not contain a completed irace run !")
  res
}

get_best_configuration <- function(p)
{
  res <- read_completed_logfile(p)
  irace::getFinalElites(res, n=1, drop.metadata = TRUE)
}

get_irace_test_results <- function(p)
{
  res <- read_completed_logfile(p)
  if (!irace::has_testing_data(res))
    cli_abort("File {.filename {p}} does not contain testing data !")
  instances <- sub("^/", "", perl = TRUE,
                   sub(res$scenario$testInstancesDir, "", res$scenario$testInstances, fixed=TRUE))
  data.table(instance = instances, seed = res$testing$seeds,
             cost = res$testing$experiments[, 1L])
}

collect_test_results <- function(exec_dir, scenarios, file = "test_results.rds", verbose = TRUE)
{
  res <- list()
  for (scenario_name in scenarios) {
    scenario_path <- file.path(exec_dir, scenario_name)
    paths <- fs::dir_ls(scenario_path, type="directory")
    tuner <- sub("-[0-9]+$", "", fs::path_rel(paths, start = scenario_path), perl=TRUE)
    reps <- as.integer(sub("^.+-([0-9]+)$", "\\1", paths, perl=TRUE))
    if (verbose) {
      for (x in unique(tuner)) {
        cat(sprintf("%s: %s: %s\n", scenario_name, x,
                    paste0(collapse=", ", reps[x == tuner])))
      }
    }
    cost <- lapply(paths, get_irace_test_results)
    print(cost)
    names(cost) <- NULL
    dt <- cbind(scenario = scenario_name, tuner = rep(tuner, each = nrow(cost[[1L]])),
                rep = rep(reps, each = nrow(cost[[1L]])),
                rbindlist(cost, use.names=TRUE))
    res <- c(res, list(dt))
  }
  res <- rbindlist(res)
  if (!is.null(file))
    saveRDS(res, file = file)
  invisible(res)
}

collect_best_confs <- function(exec_dir, scenarios, file = "best_confs.rds", verbose = FALSE)
{
  res <- sapply(scenarios, function(scenario_name) {
    scenario_path <- file.path(exec_dir, scenario_name)
    paths <- fs::dir_ls(scenario_path, type="directory")
    tuner <- sub("-[0-9]+$", "", fs::path_rel(paths, start = scenario_path), perl=TRUE)
    reps <- as.integer(sub("^.+-([0-9]+)$", "\\1", paths, perl=TRUE))
    if (verbose) {
      for (x in unique(tuner)) {
        cat(sprintf("%s: %s: %s\n", scenario_name, x,
                    paste0(collapse=", ", reps[x == tuner])))
      }
    }
    conf <- lapply(paths, get_best_configuration)
    names(conf) <- NULL
    cbind(.scenario = scenario_name, .tuner = tuner,
               .rep = reps, rbindlist(conf, use.names=TRUE))
  }, simplify = FALSE)
  if (!is.null(file))
    saveRDS(res, file = file)
  invisible(res)
}

setup_future_plan <- function(cluster = FALSE)
{
  if (cluster) {
    library(future.batchtools, quiet = TRUE)
    future::plan(batchtools_sge, template = "./batchtools.sge.tmpl",
                 resources = list(cpu="haswell", numcores=12))
  } else {
    Sys.setenv(PROCESSX_NOTIFY_OLD_SIGCHLD="true") # https://github.com/r-lib/processx/issues/236
    future::plan(multicore, workers = 4)
  }
}

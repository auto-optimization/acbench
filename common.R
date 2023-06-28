library(devtools, quiet = TRUE)

require_or_install <- function(package)
{
 package <- as.character(substitute(package))
 if (!require(package, quiet = TRUE, character.only = TRUE)) {
     devtools::install_cran(package, upgrade = "never")
     require(package, quiet = TRUE, character.only = TRUE)
 }
}

require_or_install(fs)
require_or_install(cli)
require_or_install(data.table)
# Debugging:
require_or_install(debugme)
Sys.setenv(DEBUGME = "batchtools")
#options(future.debug = TRUE)

install_irace <- function(install_dir, version, reinstall = FALSE)
{
  lib <- file.path(install_dir, paste0("irace_", version))
  if (reinstall || !fs::file_exists(file.path(lib, "irace", "bin"))) {
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

get_irace_cmdline <- function(scenario_file, exec_dir, seed, ncpus = NULL, test = NULL)
{
  args <- c("-s", scenario_file, "--exec-dir", exec_dir, "--seed", seed)
  if (!is.null(ncpus))
    args <- c(args, "--parallel", ncpus)
  if (!is.null(test))
    args <- c(args, "--only-test", test)
  args
}

sge_run_irace <- function(exe, scenario_file, exec_dir, run, jobname, ncpus)
{
  require_or_install(brew)
  outfile <- fs::path_abs(file.path(exec_dir, paste0(jobname, ".log")))
  launch_file <- tempfile(pattern = "launch_sge", tmpdir = tempdir(), fileext = ".sh")
  brew("launch_sge.tmpl", output = launch_file)
  fs::file_chmod(launch_file, "u+x")
  system2(launch_file, args = c(exe, get_irace_cmdline(scenario_file, exec_dir, seed = 42 + run, ncpus = ncpus)), stdout = "", stderr = "")
  fs::file_delete(launch_file)
}

run_irace <- function(exe, scenario_file, exec_dir, run, jobname, ncpus)
{
  system2(exe, args = get_irace_cmdline(scenario_file, exec_dir, seed = 42 + run, ncpus = ncpus),
          stdout = file.path(exec_dir, "stdout.txt"),
          stderr = file.path(exec_dir, "stderr.txt"))
}


sge_run_irace_testing <- function(exe, scenario_file, exec_dir, confs_file, jobname, ncpus)
{
  require_or_install(brew)
  outfile <- fs::path_abs(file.path(exec_dir, paste0(jobname, ".log")))
  launch_file <- tempfile(pattern = "launch_sge", tmpdir = tempdir(), fileext = ".sh")
  brew("launch_sge.tmpl", output = launch_file)
  fs::file_chmod(launch_file, "u+x")
  system2(launch_file, args = c(exe, get_irace_cmdline(scenario_file, exec_dir, seed = 42, test = confs_file, ncpus = ncpus)),
          stdout = "", stderr = "")
  fs::file_delete(launch_file)
}

run_irace_testing <- function(exe, scenario_file, exec_dir, confs_file, jobname, ncpus)
{
  system2(exe, args = get_irace_cmdline(scenario_file, exec_dir, seed = 42, test = confs_file, ncpus = ncpus),
          stdout = file.path(exec_dir, "stdout.txt"),
          stderr = file.path(exec_dir, "stderr.txt"))
}

get_tuner_executable <- function(install_dir, tool, version)
  switch(tool,
         "irace" = get_installed_irace(install_dir, version))

find_scenario <- function(scenario_name)
{
  scenario <- file.path("scenarios", paste0(scenario_name, ".txt"))
  names(scenario) <- scenario_name
  if (!fs::file_exists(scenario))
    cli_abort("Unknown scenario {.var {scenario_name}}")
  scenario
}

setup_scenario <- function(scenario_name)
{
  scenario <- find_scenario(scenario_name)
  system2(file.path("./setups", paste0(scenario_name, ".sh")), 
  stdout = "", stderr = "", timeout = 120)
  scenario
}

make_jobname <- function(scenario_name, tuner, tuner_version, rep)
 sprintf("%s_%s-%s-%02d", tuner, tuner_version, scenario_name, rep)

make_execdir_name <- function(exec_dir, scenario_name, tuner, tuner_version, rep)
 file.path(exec_dir, scenario_name, sprintf("%s_%s-%02d", tuner, tuner_version, rep))

run_scenario <- function(scenario_name, install_dir, exec_dir, tuner, tuner_version, nreps)
{
  reps <- seq_len(nreps)
  exec_dirs <- sapply(reps, function(r) {
    d <- make_execdir_name(exec_dir, scenario_name, tuner, tuner_version, r)
    if (fs::file_exists(d)) {
      fs::dir_delete(d)
    }
    fs::dir_create(d)
    d
  })
  exe <- get_tuner_executable(install_dir, tuner, tuner_version)
  scenario <- setup_scenario(scenario_name)
  cli_inform("running irace {.file {exe}} on scenario {scenario_name}, exec_dir ={.file {exec_dir}}, {nreps} times")
  mapply(sge_run_irace, exe = exe, scenario_file = scenario, exec_dir = exec_dirs, run = reps,
         jobname = make_jobname(scenario_name, tuner, tuner_version, reps),
         ncpus = 12)
    #  future_mapply(run_irace, exe = exe, scenario_file = scenario, exec_dir = exec_dirs, run = reps,
  #        future.label = paste0(tuner, "_", tuner_version, "-", scenario_name, "-%d"),
#	  future.conditions = NULL)
# ids <- batchtools::batchMap(run_irace, exe = exe, scenario_file = scenario, exec_dir = exec_dirs, run = reps)
# batchtools::setJobNames(ids, names = paste0(tuner, "_", tuner_version, "-", scenario_name, "-", reps))
#         ncpus <- 12
#     	cpu <- "haswell"
# batchtools::submitJobs(resources = list(cpu=cpu, ncpus=ncpus))
# batchtools::waitForJobs()
}

read_configurations <- function(scenario_name, file = "best_confs.rds", metadata = TRUE)
{
  x <- readRDS(file)[[scenario_name]]
  if (metadata) return(x)
  drop_cols <- colnames(x)
  drop_cols <- drop_cols[startsWith(drop_cols, ".")]
  x[, (drop_cols):=NULL]
}

run_test <- function(scenario_name, install_dir, exec_dir, tuner, tuner_version, reps)
{
  exec_dir <- make_execdir_name(exec_dir, scenario_name, tuner, tuner_version, reps)
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
  res <- irace::read_logfile(p)
  if (!is.null(res$state$completed) && res$state$completed == "Incomplete")
    cli_abort("File {.filename {p}} does not contain a completed irace run !")
  res
}

get_best_configuration <- function(p)
{
  require_or_install(irace)
  res <- read_completed_logfile(p)
  irace::getFinalElites(res, n=1, drop.metadata = TRUE)
}

get_irace_test_results <- function(p)
{
  require_or_install(irace)
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
    # FIXME: Collect CPU time, max_experiments, nb_configurations, nb_instances.
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
  require_or_install(future.apply)
  if (cluster) {
    #require_or_install(future.batchtools)
    ncpus <- 12
    cpu <- "haswell"
#    future::plan(batchtools_sge, template = "./batchtools.sge.tmpl",
#                 resources = list(cpu=cpu, ncpus=ncpus),
# https://github.com/HenrikBengtsson/future.batchtools/issues/68
# finalize=FALSE)
#    cli_inform("batchtools_sge plan setup completed with {.var {ncpus}} cpus of type {.var {cpu}}")
# require_or_install(batchtools)
# d <- "~/scratch/execdir/registry"
#     if (fs::file_exists(d)) {
#       fs::dir_delete(d)
#     }
# reg <- batchtools::makeRegistry(file.dir=d)
# reg$cluster.functions <- batchtools::makeClusterFunctionsSGE(template = "./batchtools.sge.tmpl")
# setDefaultRegistry(reg)
    
  } else {
    Sys.setenv(PROCESSX_NOTIFY_OLD_SIGCHLD="true") # https://github.com/r-lib/processx/issues/236
    ncpus <- 4
    future::plan(multicore, workers = ncpus)
    cli_inform("multicore plan setup completed with {.var {ncpus}} cpus")
  }
}

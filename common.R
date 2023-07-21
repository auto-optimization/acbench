library(devtools, quiet = TRUE)

require_or_install <- function(package)
{
 package <- as.character(substitute(package))
 if (!require(package, quiet = TRUE, character.only = TRUE)) {
     devtools::install_cran(package, upgrade = "never")
     require(package, quiet = TRUE, character.only = TRUE)
 }
}

require_or_install(R6)
require_or_install(fs)
require_or_install(cli)
require_or_install(withr)
require_or_install(data.table)
# Debugging:
#require_or_install(debugme)
#Sys.setenv(DEBUGME = "batchtools")
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

get_tuner_executable <- function(install_dir, tool, version)
  switch(tool,
         "irace" = get_installed_irace(install_dir, version))


get_irace_cmdline <- function(scenario_file, exec_dir, seed, ncpus = NULL, test = NULL)
{
  args <- c("-s", scenario_file, "--exec-dir", exec_dir, "--seed", seed)
  if (!is.null(ncpus))
    args <- c(args, "--parallel", ncpus)
  if (!is.null(test))
    args <- c(args, "--only-test", test)
  args
}

sge_run <- function(ncpus) {
  .ncpus <- force(ncpus)
  function(exe, args, exec_dir, jobname, ncpus = .ncpus) {
    outfile <- fs::path_abs(file.path(exec_dir, "stdout.txt"))
    errfile <- fs::path_abs(file.path(exec_dir, "stderr.txt"))
    launch_file <- tempfile(pattern = "launch_sge", tmpdir = tempdir(), fileext = ".sh")
    brew::brew("launch_sge.tmpl", output = launch_file)
    fs::file_chmod(launch_file, "u+x")
    #cat("sytem2(", launch_file, ", args = c(", exe, paste0(collapse=",", args), "\n")
    system2(launch_file, args = c(exe, args), stdout = "", stderr = "")
    fs::file_delete(launch_file)
  }
}

local_run <- function(exe, args, exec_dir, jobname = NULL)
{
  system2(exe, args = args,
          stdout = file.path(exec_dir, "stdout.txt"),
          stderr = file.path(exec_dir, "stderr.txt"))
}

find_scenario <- function(scenario_name)
{
  scenario <- file.path("scenarios", paste0(scenario_name, ".txt"))
  names(scenario) <- scenario_name
  if (!fs::file_exists(scenario))
    cli_abort("Unknown scenario {.var {scenario_name}}")
  scenario
}

invoke_make <- function(args)
  system2("make", args = args)

setup_acotsp <- function()
  invoke_make("-C ./algorithms/acotspqap/ acotsp")

setup_acoqap <- function()
  invoke_make("-C ./algorithms/acotspqap/ acoqap")

setup_runsolver <- function()
{
  withr::with_dir("./algorithms/runsolver/", {
    invoke_make("clean")
    invoke_make("NUMA=0")
  })    
}

setup_highs_v153 <- function()
{
  withr::with_dir("./algorithms/highs-v153/", {
    if (!fs::file_exists("./HiGHS-1.5.3/")) {
      download_uncompress('https://github.com/ERGO-Code/HiGHS/archive/refs/tags/v1.5.3.tar.gz',
                          exdir = ".") # files = c("Makefile", "README.txt", "src/"))
      # mv src-lkh-original/Makefile src-lkh-original/README.txt src-lkh-original/src -t .
    }
    srcdir <- file.path(getwd(), "HiGHS-1.5.3")
    install_dir <- getwd()
    withr::with_tempdir(pattern = "build", {
      buildir <- getwd()
      system2("cmake", args = c("-DBUILD_EXAMPLES=OFF", "-DSHARED=OFF", "-DBUILD_SHARED_LIBS=OFF", "-DBUILD_TESTING=OFF",
                                paste0("-DCMAKE_INSTALL_PREFIX=", install_dir), srcdir))
      system2("cmake", args = c("--build ."))
      system2("cmake", args = c("--install ."))
    })
    # We do not need these directories.
    fs::dir_delete(file.path(install_dir, "include"))
    fs::dir_delete(file.path(install_dir, "lib"))
  })
}

setup_lkh_209 <- function()
{
  withr::with_dir("./algorithms/lkh-209/", {
    if (!fs::file_exists("./src-lkh-original/")) {
      download_uncompress('https://github.com/souzamarcelo/supp-cor-capopt/raw/master/files/src/src-lkh-original.zip',
                          exdir = ".") # files = c("Makefile", "README.txt", "src/"))
      # mv src-lkh-original/Makefile src-lkh-original/README.txt src-lkh-original/src -t .
      withr::with_dir("./src-lkh-original/src", {
        system2("patch", args = "-p1", stdin="../../lkh-209.patch")
      })
    }
    invoke_make("-C ./src-lkh-original")
  })
}

download_uncompress <- function(url, exdir, flat = FALSE)
{
  exts <- c(".tar.bz2", ".tar.gz", ".tgz", ".tar.xz", ".zip")
  fileext <- exts[endsWith(url, exts)]
  withr::with_tempfile("tf", fileext = fileext, {
    download.file(url, destfile=tf)
    if (fileext == ".zip") {
      unzip(tf, exdir = exdir, junkpaths = flat)
    } else {
      untar(tf, exdir = exdir, restore_times = FALSE, verbose = TRUE)
      if (flat) {
        files <- untar(tf, list=TRUE)
        withr::with_dir(exdir, {
          for (p in files) {
            if (fs::is_file(p)) {
              fs::file_move(p, ".")
            }
          }
          paths <- fs::dir_ls(".", type="directory")
          for (p in paths) {
            n <- length(fs::dir_ls(p, recurse = TRUE, type ="file"))
            if (n == 0L) fs::dir_delete(p)
          }
        })
      }
    }
  })
}

setup_tsp_rue_2000 <- function()
{
  exdir <- fs::path_abs("data/tsp/tsp-rue-2000")
  exdir_p <- file.path(exdir, "train")
  if (!fs::file_exists(exdir_p)) {
    download_uncompress("https://iridia.ulb.ac.be/irace/files/tsp-instances-training.tar.bz2",
                        exdir = exdir_p)
  }
  exdir_p <- file.path(exdir, "test")
  if (!fs::file_exists(exdir_p)) {
    download_uncompress("https://iridia.ulb.ac.be/irace/files/tsp-instances-testing.tar.bz2",
                        exdir = exdir_p)
  }
}

setup_qap_rs_00 <- function()
{
  exdir <- fs::path_abs("data/qap/RS")
  if (!fs::file_exists(exdir)) {
    download_uncompress("https://github.com/MLopez-Ibanez/ACOTSPQAP/raw/master/irace/qap/Instances/RS-0.0.tar.xz",
                        exdir = exdir)
  }
}

setup_mip_regions200 <- function()
{
  exdir <- fs::path_abs("data/mip/Regions200")
  if (!fs::file_exists(exdir)) {
    download_uncompress("http://aad.informatik.uni-freiburg.de/~lindauer/aclib/mip_Regions200.tar.gz",
                        exdir = exdir, flat = TRUE)
  }
}

setup_scenario <- function(scenario_name, install_dir)
{
  scenario <- find_scenario(scenario_name)
  if (scenario_name %in% c("acotsp-tsp-rue-2000","acotsp-tsp-rue-2000-default")) {
    setup_acotsp()
    setup_tsp_rue_2000()
  } else if (scenario_name %in% c("acoqap-qap-rs-00", "acoqap-qap-rs-00-default")) {
    setup_acoqap()
    setup_qap_rs_00()
  } else if (scenario_name == "lkh-209-tsp-rue-2000") {
    setup_runsolver()
    setup_lkh_209()
    setup_tsp_rue_2000()
  } else if (scenario_name == "highs-153-mip-regions200") {
    setup_highs_v153()
    setup_mip_regions200()
  } else {
    cli_abort("No setup defined for scenario {scenario_name} !")
  }
  scenario
}

make_jobname <- function(scenario_name, tuner, tuner_version, rep)
 sprintf("%s_%s-%s-%02d", tuner, tuner_version, scenario_name, rep)

make_execdir_name <- function(exec_dir, scenario_name, tuner, tuner_version, rep)
 file.path(exec_dir, scenario_name, sprintf("%s_%s-%02d", tuner, tuner_version, rep))

read_configurations <- function(scenario_name, file = "best_confs.rds", metadata = TRUE)
{
  x <- readRDS(file)[[scenario_name]]
  if (metadata) return(x)
  drop_cols <- colnames(x)
  drop_cols <- drop_cols[startsWith(drop_cols, ".")]
  x[, (drop_cols):=NULL]
}


  
read_scenarios_file <- function(file)
  scan(scenarios_file, what = character(), strip.white=TRUE, comment.char = "#", quiet = TRUE)


read_completed_logfile <- function(p, has_testing = FALSE)
{
  p <- file.path(p, "irace.Rdata")
  if (!fs::file_exists(p)) cli_abort("File {.filename {p}} not found !")
  res <- irace::read_logfile(p)
  if (!is.null(res$state$completed) && res$state$completed == "Incomplete")
    cli_abort("File {.filename {p}} does not contain a completed irace run !")
  if (has_testing && !irace::has_testing_data(res))
    cli_abort("File {.filename {p}} does not contain testing data !")
  res
}

get_best_configuration <- function(p)
{
  require_or_install(irace)
  res <- read_completed_logfile(p)
  irace::getFinalElites(res, n=1, drop.metadata = TRUE)
}

get_irace_test_results <- function(res)
{
  if (is.character(res)) {
    res <- read_completed_logfile(res, has_testing = TRUE)
  } else if (!irace::has_testing_data(res)) {
    cli_abort("log data does not contain testing data !")
  }
  instances <- sub("^/", "", perl = TRUE,
                   sub(res$scenario$testInstancesDir, "", res$scenario$testInstances, fixed=TRUE))
  experiments <- res$testing$experiments
  cbind(as.data.table(
    # Cartesian product of matrix row and column names
    # returns a data.frame with 2 column
    expand.grid(instance_id = rownames(experiments), configuration_id = as.integer(colnames(experiments)), stringsAsFactors = FALSE)),
    # Store matrix value into a vector:
    # first all values from 1st column, then 2nd, and so on.
    cost = as.vector(experiments),
    seed = res$testing$seeds,
    instance = instances)
}

collect_test_results <- function(exec_dir, scenarios, file = "test_results.rds", verbose = TRUE)
{
  require_or_install(irace)
  res <- list()
  if (fs::file_exists(file))
    res <- readRDS(file = file)
  
  for (scenario_name in scenarios) {
    p <- file.path(exec_dir, paste0("test-", scenario_name))
    log <- read_completed_logfile(p, has_testing = TRUE)
    results <- get_irace_test_results(log)
    allConfigurations <- as.data.table(log$allConfigurations)
    
    confs <- read_configurations(scenario_name)
    confs <- allConfigurations[confs, on = colnames(confs)[!startsWith(colnames(confs), ".")]]
    set(confs, j=c(".PARENT.", colnames(confs)[!startsWith(colnames(confs), ".")]), value=NULL)
    results <- results[confs, on=c(configuration_id=".ID.")]
    colnames(results) <- sub(".", "", colnames(results), fixed=TRUE)
    # FIXME: merge
    old <- res[[scenario_name]]
    if (!is.null(old)) {
      results <- rbind(old[!results, on = c("scenario", "tuner", "rep")], results)
    }
    res[[scenario_name]] <- results
  }
  if (!is.null(file))
    saveRDS(res, file = file)
  invisible(res)
}

get_train_summary <- function(p)
{
  res <- read_completed_logfile(p)
  res <- irace::irace_summarise(res)
  as.data.table(res[c("n_iterations", "n_instances", "n_experiments", "time_targetrunner",
                      "time_cpu_total", "time_wallclock")])
}

collect_train_results <- function(exec_dir, scenarios, file = "train_results.rds", verbose = FALSE)
{
  results <- sapply(scenarios, function(scenario_name) {
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
    res <- data.table::rbindlist(lapply(paths, get_train_summary))
    cbind(scenario = as.character(scenario_name), tuner = as.character(tuner), rep = reps,
          res)
  }, simplify = FALSE)

  old <- NULL
  if (!is.null(file) && fs::file_exists(file))
    old <- readRDS(file = file)

  results <- rbindlist(results, use.names=TRUE)
  
  if (!is.null(old)) {
    results <- rbind(old[!results, on = c("scenario", "tuner", "rep")], results)
  }

  if (!is.null(file))
    saveRDS(results, file = file)
  invisible(results)
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
    cbind(.scenario = as.character(scenario_name), .tuner = as.character(tuner),
          .rep = reps, rbindlist(conf, use.names=TRUE))
  }, simplify = FALSE)
  if (!is.null(file))
    saveRDS(res, file = file)
  invisible(res)
}

## setup_future_plan <- function(cluster = FALSE)
## {
##   if (cluster) {
##     #require_or_install(future.batchtools)
##     ncpus <- 12
##     require_or_install(brew)
    
## #    future::plan(batchtools_sge, template = "./batchtools.sge.tmpl",
## #                 resources = list(cpu=cpu, ncpus=ncpus),
## # https://github.com/HenrikBengtsson/future.batchtools/issues/68
## # finalize=FALSE)
## #    cli_inform("batchtools_sge plan setup completed with {.var {ncpus}} cpus of type {.var {cpu}}")
## # require_or_install(batchtools)
## # d <- "~/scratch/execdir/registry"
## #     if (fs::file_exists(d)) {
## #       fs::dir_delete(d)
## #     }
## # reg <- batchtools::makeRegistry(file.dir=d)
## # reg$cluster.functions <- batchtools::makeClusterFunctionsSGE(template = "./batchtools.sge.tmpl")
## # setDefaultRegistry(reg)
    
##   } else {
##     require_or_install(future.apply)
##     Sys.setenv(PROCESSX_NOTIFY_OLD_SIGCHLD="true") # https://github.com/r-lib/processx/issues/236
##     ncpus <- 4
##     future::plan(multicore, workers = ncpus)
##     cli_inform("multicore plan setup completed with {.var {ncpus}} cpus")
##   }
## }

ACBench <- R6::R6Class("ACBench", cloneable = TRUE, lock_class = TRUE, portable = TRUE,
 public = list(
   exec_dir = NULL,
   install_dir = NULL,
   do_run = NULL,
   ncpus = 1L,
   saved_setup = NULL,
   initialize = function(exec_dir, install_dir, ncpus = 1, cluster = FALSE) {
     self$exec_dir <- exec_dir
     self$install_dir <- install_dir
     self$ncpus <- ncpus
     if (cluster) {
       require_or_install(brew)
       self$do_run <- sge_run(ncpus)
       cli_inform("SGE plan setup completed with {ncpus} cpus")
     } else {
       require_or_install(future.apply)
       Sys.setenv(PROCESSX_NOTIFY_OLD_SIGCHLD="true") # https://github.com/r-lib/processx/issues/236
       future::plan(multicore, workers = ncpus)
       cli_inform("multicore plan setup completed with {ncpus} cpus")
       self$do_run <- local_run
     }
   },
   save_setup = function(scenarios, tuners, tuner_versions, nreps) {
     self$saved_setup <- list(scenarios = scenarios,
                             tuners = tuners,
                             tuner_versions = tuner_versions,
                             nreps = nreps)
   },
   run_irace = function(exe, scenario_file, exec_dir, run, jobname) {
     self$do_run(exe, args = get_irace_cmdline(scenario_file, exec_dir, seed = 42 + run, ncpus = self$ncpus),
                 exec_dir = exec_dir, jobname = jobname)
   },
   
   run_irace_testing = function(exe, scenario_file, exec_dir, confs_file, jobname) {
     self$do_run(exe, args = get_irace_cmdline(scenario_file, exec_dir, seed = 42, test = confs_file, ncpus = self$ncpus),
                 exec_dir = exec_dir, jobname = jobname)
   },

   run_saved_setup = function() {
     if (is.null(self$saved_setup))
       cli_abort("No setup saved !")
     self$run_scenario(scenarios = self$saved_setup$scenarios,
                       tuners = self$saved_setup$tuners,
                       tuner_versions = self$saved_setup$tuner_versions,
                       nreps = self$saved_setup$nreps)
   },

   run_scenario = function(scenarios, tuners, tuner_versions, nreps) {
     if (length(reps) == 1)
       reps <- seq_len(nreps)
     
     exec_dir <- self$exec_dir
     install_dir <- self$install_dir
     for (scenario_name in scenarios) {
       scenario <- setup_scenario(scenario_name, install_dir)
       for (tuner in tuners) {
         for (tuner_version in tuner_versions) {
           exec_dirs <- sapply(reps, function(r) {
             d <- make_execdir_name(exec_dir, scenario_name, tuner, tuner_version, r)
             if (fs::file_exists(d)) {
               fs::dir_delete(d)
             }
             fs::dir_create(d)
             d
           })
           exe <- get_tuner_executable(install_dir, tuner, tuner_version)
           cli_inform("running irace {.file {exe}} on scenario {scenario_name}, exec_dir ={.file {exec_dir}}, reps = {paste0(collapse=',', reps)}")
           mapply(self$run_irace, exe = exe, scenario_file = scenario, exec_dir = exec_dirs, run = reps,
                  jobname = make_jobname(scenario_name, tuner, tuner_version, reps))
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
       }
     }
   },
   
   run_test = function(scenario_name) {
     exec_dir <- self$exec_dir
     install_dir <- self$install_dir
     if (!fs::file_exists(exec_dir))
       cli_abort("exec_dir {.filename {exec_dir}} not found")
     jobname <- paste0("test-", scenario_name)
     test_exec_dir <- file.path(exec_dir, jobname)
     if (!fs::file_exists(test_exec_dir))
       fs::dir_create(test_exec_dir)
     
     confs <- read_configurations(scenario_name, metadata = FALSE)
     confs_file <- file.path(test_exec_dir, paste0("confs-", scenario_name, ".txt"))
     write.table(confs, file = confs_file, row.names = FALSE)  
     
     exe <- get_tuner_executable(install_dir, "irace", "git")
     scenario <- find_scenario(scenario_name)
     self$run_irace_testing(exe = exe, scenario_file = scenario, exec_dir = test_exec_dir, confs_file = confs_file, jobname = jobname)
   }
   
 ))


read_setup_file <- function(file)
{
  source(file)
  scenarios <- scan(text=scenarios, what=character(), comment.char="#", quiet = TRUE)
  tuners <- scan(text=tuners, what=character(), comment.char="#", quiet = TRUE)
  tuner_versions <- scan(text=tuner_versions, what=character(), comment.char="#", quiet = TRUE)
  acbench <- ACBench$new(exec_dir = exec_dir, install_dir = install_dir,
                         cluster = cluster, ncpus = ncpus)
  
  acbench$save_setup(scenarios, tuners, tuner_versions, nreps)
  acbench
}

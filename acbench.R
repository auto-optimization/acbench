require_or_install <- function(package) {
  package <- as.character(substitute(package))
  if (!require(package, quiet = TRUE, character.only = TRUE)) {
    if (package == "remotes") {
      install.packages("remotes", repo = "https://cloud.r-project.org")
    } else {
      remotes::install_cran(package, upgrade = "never")
    }
    require(package, quiet = TRUE, character.only = TRUE)
  }
}
require_or_install(remotes)
require_or_install(R6)
require_or_install(fs)
require_or_install(cli)
require_or_install(processx)
require_or_install(withr)
require_or_install(stringr)
require_or_install(data.table)
setDTthreads(threads = 1)
## Debugging:
# require_or_install(debugme)
# Sys.setenv(DEBUGME = "batchtools")
# options(future.debug = TRUE)

# Same as !(x %in% table). Package data.table has %notin%.
"%notin%" <- function(x, table) match(x, table, nomatch = 0L) == 0L

source("setups.R")

check_output_files <- function(exec_dir, scenarios, tuners, reps) {
  check_file_nonzero <- function(f) {
    if (!fs::file_exists(f)) {
      cli_warn("{.file {f}} does not exist!")
    } else if (fs::file_size(f) == 0) {
      cli_warn("{.file {f}} has zero size!")
    }
  }
  check_file_zero <- function(f) {
    if (!fs::file_exists(f)) {
      cli_warn("{.file {f}} does not exist!")
    } else if (fs::file_size(f) > 0) {
      cli_warn("{.file {f}} has non-zero size!")
    }
  }

  if (length(reps) == 1L) {
    reps <- seq_len(reps)
  }

  for (scenario_name in scenarios) {
    for (tuner in tuners) {
      tuner <- strsplit(tuner, ":")[[1L]]
      tuner_version <- tuner[2L]
      tuner_name <- tuner[1L]
      for (r in reps) {
        d <- make_execdir_name(exec_dir, scenario_name, tuner_name, tuner_version, r)
        check_file_zero(file.path(d, "stderr.txt"))
        check_file_nonzero(file.path(d, "stdout.txt"))
        check_file_nonzero(file.path(d, "irace.Rdata"))
      }
    }
  }
}

find_scenario <- function(scenario_name, install_dir, tuner, version) {
  lib <- file.path(install_dir, paste0(tuner, "_", version))
  real_version <- packageVersion(tuner, lib.loc = lib)
  lowest_version <- "0.0.0"
  for (d in fs::dir_ls("scenarios/", glob = paste0("*/", tuner, "_*"))) {
    scenario_version <- strsplit(fs::path_file(d), "_")[[1L]][2L]
    if (real_version <= package_version(scenario_version) &&
      package_version(scenario_version) > package_version(lower_version)) {
      lowest_version <- scenario_version
    }
  }
  scenario <- if (lowest_version == "0.0.0") {
    file.path("scenarios", paste0(scenario_name, ".txt"))
  } else {
    file.path("scenarios", paste0(tuner, "_", lowest_version), paste0(scenario_name, ".txt"))
  }
  names(scenario) <- scenario_name
  if (!fs::file_exists(scenario)) {
    cli_abort("Unknown scenario {.var {scenario_name}}")
  }
  scenario
}

install_irace <- function(install_dir, version, reinstall = FALSE) {
  lib <- file.path(install_dir, paste0("irace_", version))
  if (reinstall || !fs::file_exists(file.path(lib, "irace", "bin", "irace"))) {
    tryCatch(
      {
        fs::dir_create(lib)
        if (version == "git") {
          install_github("MLopez-Ibanez/irace", upgrade = "never", lib = lib)
        } else if (startsWith(version, "git-")) {
          branch <- sub("git-", "", version, fixed = TRUE)
          # options(download.file.method = "libcurl")
          install_github("MLopez-Ibanez/irace", upgrade = "never", lib = lib, ref = branch)
        } else {
          install_version("irace", version = version, upgrade = "never", lib = lib)
        }
        real_version <- packageVersion("irace", lib.loc = lib)
        # We overwrite the irace launcher for versions that do not support relocation.
        if (real_version < package_version("3.5.1")) {
          fs::file_copy("./util/irace", file.path(lib, "irace", "bin", "irace"), overwrite = TRUE)
        }
      },
      error = function(c) {
        fs::dir_delete(lib)
        stop(c)
      }
    )
  }
  lib
}

get_installed_irace <- function(install_dir, version) {
  lib <- install_irace(install_dir, version = version)
  exe <- fs::path_abs(file.path(lib, "irace", "bin", "irace"))
  if (!fs::file_access(exe, mode = "execute")) {
    cli_abort("Executable {.filename {exe}} not found or not executable ")
  }
  exe
}

get_tuner_executable <- function(install_dir, tool, version) {
  switch(tool,
    "irace" = get_installed_irace(install_dir, version)
  )
}

file_safe_delete <- function(path) {
  if (fs::file_exists(path)) {
    fs::file_delete(path)
  }
}

get_irace_cmdline <- function(scenario_file, exec_dir, seed, ncpus = NULL, test = NULL) {
  args <- c("-s", scenario_file, "--exec-dir", exec_dir, "--seed", seed, "--test-num-elites", "0")
  if (!is.null(ncpus)) {
    args <- c(args, "--parallel", ncpus)
  }
  if (!is.null(test)) {
    args <- c(args, "--only-test", test)
  }
  args
}

sge_run <- function(ncpus) {
  .ncpus <- force(ncpus)
  function(exe, args, exec_dir, jobname, ncpus = .ncpus) {
    outfile <- fs::path_abs(file.path(exec_dir, "stdout.txt"))
    errfile <- fs::path_abs(file.path(exec_dir, "stderr.txt"))
    file_safe_delete(outfile)
    file_safe_delete(errfile)
    launch_file <- tempfile(pattern = "launch_sge", tmpdir = tempdir(), fileext = ".sh")
    brew::brew("launch_sge.tmpl", output = launch_file)
    fs::file_chmod(launch_file, "u+x")
    # cat("system2(", launch_file, ", args = c(", exe, paste0(collapse=",", args), "\n")
    # FIXME: Parse
    output <- processx::run(launch_file,
      args = c(exe, args), echo_cmd = FALSE, echo = TRUE,
      error_on_status = TRUE, cleanup_tree = TRUE
    )
    # print(output)
    jobID <- stringr::str_extract(output$stdout, "^Your job ([0-9]+) \\([^)]+\\) has been submitted", group = 1)
    # print(jobID)
    if (is.na(jobID)) {
      cli_abort("Cannot parse jobID from:\n", output$stdout)
    }
    fs::file_delete(launch_file)
    paste0(jobID, " ", jobname)
  }
}


local_run <- function(exe, args, exec_dir, jobname = NULL) {
  system2(exe,
    args = args,
    stdout = file.path(exec_dir, "stdout.txt"),
    stderr = file.path(exec_dir, "stderr.txt")
  )
}

invoke_make <- function(args) {
  system2("make", args = args)
}

download_uncompress <- function(url, exdir, flat = FALSE) {
  exts <- c(".tar.bz2", ".tar.gz", ".tgz", ".tar.xz", ".zip")
  fileext <- exts[endsWith(url, exts)]
  tf <- local_tempfile(fileext = fileext)
  download.file(url, destfile = tf)
  if (fileext == ".zip") {
    unzip(tf, exdir = exdir, junkpaths = flat)
  } else {
    untar(tf, exdir = exdir, restore_times = FALSE, verbose = TRUE)
    if (flat) {
      files <- untar(tf, list = TRUE)
      withr::with_dir(exdir, {
        for (p in files) {
          if (fs::is_file(p)) {
            fs::file_move(p, ".")
          }
        }
        paths <- fs::dir_ls(".", type = "directory")
        for (p in paths) {
          n <- length(fs::dir_ls(p, recurse = TRUE, type = "file"))
          if (n == 0L) fs::dir_delete(p)
        }
      })
    }
  }
}

make_jobname <- function(scenario_name, tuner, tuner_version, rep) {
  sprintf("%s_%s-%s-%02d", tuner, tuner_version, scenario_name, rep)
}

make_execdir_name <- function(exec_dir, scenario_name, tuner, tuner_version, rep) {
  file.path(exec_dir, scenario_name, sprintf("%s_%s-%02d", tuner, tuner_version, rep))
}

read_configurations <- function(scenario_name, file = "best_confs.rds", metadata = TRUE) {
  x <- readRDS(file)[[scenario_name]]
  if (metadata) {
    return(x)
  }
  drop_cols <- colnames(x)
  drop_cols <- drop_cols[startsWith(drop_cols, ".")]
  x[, (drop_cols) := NULL]
  unique(x) # Remove duplicated rows.
}

read_scenarios_file <- function(file = "", text) {
  if (missing(file) && !missing(text)) {
    return(scan(text = scenarios, what = character(), comment.char = "#", quiet = TRUE))
  }
  scan(file, what = character(), strip.white = TRUE, comment.char = "#", quiet = TRUE)
}

read_completed_logfile <- function(p, has_testing = FALSE) {
  p <- file.path(p, "irace.Rdata")
  if (!fs::file_exists(p)) cli_abort("File {.filename {p}} not found !")
  res <- irace::read_logfile(p)
  if (!is.null(res$state$completed) && res$state$completed == "Incomplete") {
    cli_abort("File {.filename {p}} does not contain a completed irace run !")
  }
  if (has_testing && !irace::has_testing_data(res)) {
    cli_abort("File {.filename {p}} does not contain testing data !")
  }
  res
}

get_best_configuration <- function(p) {
  require_or_install(irace)
  res <- read_completed_logfile(p)
  irace::getFinalElites(res, n = 1, drop.metadata = TRUE)
}

get_irace_test_results <- function(res) {
  require_or_install(irace)
  if (is.character(res)) {
    res <- read_completed_logfile(res, has_testing = TRUE)
  } else if (!irace::has_testing_data(res)) {
    cli_abort("log data does not contain testing data !")
  }
  instances <- sub("^/", "",
    perl = TRUE,
    sub(res$scenario$testInstancesDir, "", res$scenario$testInstances, fixed = TRUE)
  )
  experiments <- res$testing$experiments
  cbind(
    as.data.table(
      # Cartesian product of matrix row and column names
      # returns a data.frame with 2 column
      expand.grid(instance_id = rownames(experiments), configuration_id = as.integer(colnames(experiments)), stringsAsFactors = FALSE)
    ),
    # Store matrix value into a vector:
    # first all values from 1st column, then 2nd, and so on.
    cost = as.vector(experiments),
    seed = res$testing$seeds,
    instance = instances
  )
}

prune_test_results <- function(file = "test_results.rds", scenarios = NULL, tuners = NULL) {
  res <- readRDS(file = file)
  if (!is.null(tuners)) {
    ok <- FALSE
    for (s in names(res)) {
      x <- res[[s]]
      notfound <- tuners %notin% unique(x$tuner)
      if (any(notfound)) {
        cli_warn("tuner {tuners[notfound]} not found in scenario {s}")
      } else {
        ok <- TRUE
      }
    }
    if (ok) {
      res <- lapply(res, function(x) x[tuner %notin% tuners])
    }
  }
  saveRDS(res, file = file)
  cli_inform("Results saved to {.file {file}}")
}

collect_test_results <- function(exec_dir, scenarios, file = "test_results.rds", verbose = TRUE) {
  res <- list()
  if (fs::file_exists(file)) {
    res <- readRDS(file = file)
  }

  for (scenario_name in scenarios) {
    p <- file.path(exec_dir, paste0("test-", scenario_name))
    if (!fs::file_exists(file.path(p, "irace.Rdata"))) {
      cli_warn("File {.file {file.path(p, 'irace.Rdata')}} not found ! Skipping ...")
      next
    }
    log <- read_completed_logfile(p, has_testing = TRUE)
    results <- get_irace_test_results(log)
    allConfigurations <- as.data.table(log$allConfigurations)

    confs <- read_configurations(scenario_name)
    if (verbose) {
      cli_inform("Processing {scenario_name} ...\n")
    }
    # Assign each configuration to its corresponding .scenario, .tuner, .rep.
    confs <- allConfigurations[confs, on = colnames(confs)[!startsWith(colnames(confs), ".")]]
    # Keep only .ID., .scenario, .tuner, .rep.
    set(confs, j = c(".PARENT.", colnames(confs)[!startsWith(colnames(confs), ".")]), value = NULL)
    results <- merge(results, confs, by.x = "configuration_id", by.y = ".ID.", allow.cartesian = TRUE)
    colnames(results) <- sub(".", "", colnames(results), fixed = TRUE)
    old <- res[[scenario_name]]
    if (!is.null(old)) {
      results <- rbind(old[!results, on = c("scenario", "tuner", "rep")], results)
    }
    res[[scenario_name]] <- results
  }
  if (!is.null(file)) {
    saveRDS(res, file = file)
    cli_inform("Results saved to {.file {file}}")
  }
  invisible(res)
}

get_train_summary <- function(p) {
  res <- read_completed_logfile(p)
  res <- irace::irace_summarise(res)
  # cat(sep="", "Summarised '", p, "'\n")
  as.data.table(res[c(
    "n_iterations", "n_instances", "n_experiments", "time_targetrunner",
    "time_cpu_total", "time_wallclock"
  )])
}

prune_train_results <- function(exec_dir, scenarios, tuners, file = "train_results.rds", verbose = FALSE, invert = FALSE) {
  sapply(scenarios, function(scenario_name) {
    paths <- fs::dir_ls(file.path(exec_dir, scenario_name),
      regexp = paste0(tuners, "-[0-9]+$", collapse = "|"),
      invert = invert, type = "directory"
    )
    for (p in paths) {
      if (verbose) cli_inform("Deleting {.file {p}}")
      fs::dir_delete(p)
    }
  })
}

sge_list_jobs <- function() {
  jobs <- read.table(text = system2("qstat", "| tr -s ' ' | cut -d ' ' -f1,5 -s", stdout = TRUE), header = TRUE)
  jobs <- data.table(jobs)
  done <- setdiff(job_ids, jobs$job.ID)
  pending <- intersect(job_ids, jobs$job.ID)
  jobs <- jobs[job.ID %in% pending, ]
  running <- jobs[state == "r"][["job.ID"]]
  waiting <- jobs[state == "qw"][["job.ID"]]
  errored <- setdiff(jobs$job.ID, c(running, waiting))
  list(done = done, running = running, waiting = waiting, errored = errored)
}

# FIXME: We have to keep track of jobIDs/PIDs to be able to check status.
# check_status <- function(exec_dir, scenarios, tuners, reps) {
#  if (length(reps) == 1L)
#    reps <- seq_len(reps)
#  for (scenario_name in scenarios) {
#     for (tuner in tuners) {
#     	tuner <- strsplit(tuner, ":")[[1L]]
#  	tuner_version <- tuner[2L]
#  	tuner_name <- tuner[1L]
#         exec_dirs <- sapply(reps, function(r) {
#             d <- make_execdir_name(exec_dir, scenario_name, tuner_name, tuner_version, r)
#             if (!fs::file_exists(d)) {
#                cli_warn("{.file {d}} not found !")
# 	    } else {
#
# 	    }
#
#               fs::dir_delete(d)
#             }
#             fs::dir_create(d, recurse=TRUE)
#             d
#           })
#
# }
#

collect_train_results <- function(exec_dir, scenarios, file = "train_results.rds", verbose = FALSE) {
  results <- sapply(scenarios, function(scenario_name) {
    scenario_path <- file.path(exec_dir, scenario_name)
    paths <- fs::dir_ls(scenario_path, type = "directory")
    tuner <- sub("-[0-9]+$", "", fs::path_rel(paths, start = scenario_path), perl = TRUE)
    reps <- as.integer(sub("^.+-([0-9]+)$", "\\1", paths, perl = TRUE))
    if (verbose) {
      for (x in unique(tuner)) {
        cat(sprintf(
          "%s: %s: %s\n", scenario_name, x,
          paste0(collapse = ", ", reps[x == tuner])
        ))
      }
    }
    summaries <- lapply(paths, get_train_summary)
    res <- data.table::rbindlist(summaries)
    cbind(
      scenario = as.character(scenario_name), tuner = as.character(tuner), rep = reps,
      res
    )
  }, simplify = FALSE)
  results <- data.table::rbindlist(results, use.names = TRUE)

  old <- NULL
  if (!is.null(file) && fs::file_exists(file)) {
    old <- readRDS(file = file)
  }

  if (!is.null(old)) {
    if (verbose) cli_inform("Merging new results and old results from {.file {file}}")
    results <- rbind(old[!results, on = c("scenario", "tuner", "rep")], results)
  }
  if (!is.null(file)) {
    saveRDS(results, file = file)
    cli_inform("Results saved to {.file {file}}")
  }
  invisible(results)
}

collect_best_confs <- function(exec_dir, scenarios, file = "best_confs.rds", verbose = TRUE) {
  res <- list()
  if (fs::file_exists(file)) {
    res <- readRDS(file = file)
  }

  for (scenario_name in scenarios) {
    scenario_path <- file.path(exec_dir, scenario_name)
    paths <- fs::dir_ls(scenario_path, type = "directory")
    tuner <- sub("-[0-9]+$", "", fs::path_rel(paths, start = scenario_path), perl = TRUE)
    reps <- as.integer(sub("^.+-([0-9]+)$", "\\1", paths, perl = TRUE))
    if (verbose) {
      for (x in unique(tuner)) {
        cat(sprintf(
          "%s: %s: %s\n", scenario_name, x,
          paste0(collapse = ", ", reps[x == tuner])
        ))
      }
    }
    results <- lapply(paths, get_best_configuration)
    names(results) <- NULL
    results <- data.table(
      .scenario = as.character(scenario_name), .tuner = as.character(tuner),
      .rep = reps, rbindlist(results, use.names = TRUE)
    )
    old <- res[[scenario_name]]
    if (!is.null(old)) {
      results <- rbind(old[!results, on = c(".scenario", ".tuner", ".rep")], results)
    }
    res[[scenario_name]] <- results
  }

  if (!is.null(file)) {
    saveRDS(res, file = file)
    cli_inform("Results saved to {.file {file}}")
  }
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

read_setup_file_helper <- function(file) {
  source(file)
  scenarios <- read_scenarios_file(text = scenarios)
  tuners <- scan(text = tuners, what = character(), comment.char = "#", quiet = TRUE)
  list(
    scenarios = scenarios, tuners = tuners, reps = reps, exec_dir = exec_dir, install_dir = install_dir,
    cluster = cluster, ncpus = ncpus
  )
}

report <- function(path = "./", filename = "report",
                   interactive = base::interactive()) {
  require_or_install(rmarkdown)
  require_or_install(knitr)
  require_or_install(DT)
  # render() already checks this but the error is not clear enough.
  if (!rmarkdown::pandoc_available("1.12.3", error = FALSE)) {
    cli_abort(
      "pandoc version 1.12.3 or higher is required and was not found. ",
      "You can install the RStudio IDE, which has bundled a version of Pandoc. ",
      "Otherwise, follow the instructions at https://pandoc.org/installing.html ."
    )
  }

  path <- fs::path_abs(fs::path_expand(path))
  if (!fs::file_exists(path)) {
    cli_abort("path '{.file {path}}' does not exist")
  }

  filename <- fs::path_abs(fs::path_expand(fs::path_ext_set(filename, "html")))
  cli_alert_info("Creating file '{.file {filename}}'.\n")
  rmarkdown::render(
    input = file.path("templates", "report_html.Rmd"),
    output_file = filename, clean = TRUE
  )
  if (interactive) utils::browseURL(filename)
  filename
}

ACBench <- R6::R6Class("ACBench",
  cloneable = TRUE, lock_class = TRUE, portable = TRUE,
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
        Sys.setenv(PROCESSX_NOTIFY_OLD_SIGCHLD = "true") # https://github.com/r-lib/processx/issues/236
        future::plan(multicore, workers = ncpus)
        cli_inform("multicore plan setup completed with {ncpus} cpus")
        self$do_run <- local_run
      }
    },
    save_setup = function(scenarios, tuners, reps) {
      self$saved_setup <- list(
        scenarios = scenarios,
        tuners = tuners,
        reps = reps
      )
    },
    collect_train_results = function(scenarios, file = "train_results.rds", verbose = FALSE) {
      collect_train_results(self$exec_dir, scenarios = scenarios, file = file, verbose = verbose)
    },
    collect_test_results = function(scenarios, file = "test_results.rds", verbose = TRUE) {
      collect_test_results(self$exec_dir, scenarios = scenarios, file = file, verbose = verbose)
    },
    collect_best_confs = function(scenarios, file = "best_confs.rds", verbose = TRUE) {
      collect_best_confs(self$exec_dir, scenarios = scenarios, file = file, verbose = verbose)
    },
    run_irace = function(exe, scenario_file, exec_dir, run, jobname) {
      self$do_run(exe,
        args = get_irace_cmdline(scenario_file, exec_dir, seed = 42 + run, ncpus = self$ncpus),
        exec_dir = exec_dir, jobname = jobname
      )
    },
    run_irace_testing = function(exe, scenario_file, exec_dir, confs_file, jobname) {
      self$do_run(exe,
        args = get_irace_cmdline(scenario_file, exec_dir, seed = 42, test = confs_file, ncpus = self$ncpus),
        exec_dir = exec_dir, jobname = jobname
      )
    },
    run_saved_setup = function() {
      if (is.null(self$saved_setup)) {
        cli_abort("No setup saved !")
      }
      self$run_scenario(
        scenarios = self$saved_setup$scenarios,
        tuners = self$saved_setup$tuners,
        reps = self$saved_setup$reps
      )
    },
    run_scenario = function(scenarios, tuners, reps) {
      if (length(reps) == 1L) {
        reps <- seq_len(reps)
      }

      exec_dir <- self$exec_dir
      install_dir <- self$install_dir
      for (scenario_name in scenarios) {
        setup_scenario(scenario_name, install_dir)
        for (tuner in tuners) {
          tuner <- strsplit(tuner, ":")[[1L]]
          tuner_version <- tuner[2L]
          tuner_name <- tuner[1L]
          exec_dirs <- sapply(reps, function(r) {
            d <- make_execdir_name(exec_dir, scenario_name, tuner_name, tuner_version, r)
            if (fs::file_exists(d)) {
              fs::dir_delete(d)
            }
            fs::dir_create(d, recurse = TRUE)
            d
          })
          exe <- get_tuner_executable(install_dir, tuner_name, tuner_version)
          scenario <- find_scenario(scenario_name, install_dir, tuner_name, tuner_version)
          cli_inform("running irace {.file {exe}} on scenario {scenario_name}, exec_dir ={.file {exec_dir}}, reps = {paste0(collapse=',', reps)}")
          jobIDs <- mapply(self$run_irace,
            exe = exe, scenario_file = scenario, exec_dir = exec_dirs, run = reps,
            jobname = make_jobname(scenario_name, tuner_name, tuner_version, reps)
          )
          cat(jobIDs, sep = "\n", append = TRUE, file = file.path(exec_dir, "jobs_running"))

          #  future_mapply(run_irace, exe = exe, scenario_file = scenario, exec_dir = exec_dirs, run = reps,
          #        future.label = paste0(tuner_name, "_", tuner_version, "-", scenario_name, "-%d"),
          # 	  future.conditions = NULL)
          # ids <- batchtools::batchMap(run_irace, exe = exe, scenario_file = scenario, exec_dir = exec_dirs, run = reps)
          # batchtools::setJobNames(ids, names = paste0(tuner_name, "_", tuner_version, "-", scenario_name, "-", reps))
          #         ncpus <- 12
          #     	cpu <- "haswell"
          # batchtools::submitJobs(resources = list(cpu=cpu, ncpus=ncpus))
          # batchtools::waitForJobs()
        }
      }
    },
    run_test = function(scenarios) {
      exec_dir <- self$exec_dir
      install_dir <- self$install_dir
      if (!fs::file_exists(exec_dir)) {
        cli_abort("exec_dir {.file {exec_dir}} not found")
      }
      exe <- get_tuner_executable(install_dir, "irace", "git")
      if (missing(scenarios)) {
        scenarios <- self$saved_setup$scenarios
      } # FIXME: Error handling.

      jobIDs <- sapply(scenarios, function(scenario_name) {
        jobname <- paste0("test-", scenario_name)
        test_exec_dir <- file.path(exec_dir, jobname)
        if (!fs::file_exists(test_exec_dir))
          fs::dir_create(test_exec_dir)
        confs <- read_configurations(scenario_name, metadata = FALSE)
        confs_file <- file.path(test_exec_dir, paste0("confs-", scenario_name, ".txt"))
        write.table(confs, file = confs_file, row.names = FALSE)
        scenario <- find_scenario(scenario_name, install_dir, "irace", "git")
	self$run_irace_testing(exe = exe, scenario_file = scenario, exec_dir = test_exec_dir, confs_file = confs_file, jobname = jobname)
      })
      cat(jobIDs, sep = "\n", append = TRUE, file = file.path(exec_dir, "test_jobs_running"))
    }
  )
)

#' Check status of experiments.
#' 
do_status_check <- function(file) {
  s <- read_setup_file_helper(file)
  check_output_files(s$exec_dir, s$scenarios, s$tuners, s$reps)
}

#' Read a setup file and initialize acbench.
#' 
read_setup_file <- function(file) {
  x <- read_setup_file_helper(file)
  acbench <- ACBench$new(
    exec_dir = x$exec_dir, install_dir = x$install_dir,
    cluster = x$cluster, ncpus = x$ncpus
  )
  acbench$save_setup(x$scenarios, x$tuners, x$reps)
  acbench
}


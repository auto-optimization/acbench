# Algorithms
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
    if (fs::dir_exists(file.path(install_dir, "lib64")))
        fs::dir_delete(file.path(install_dir, "lib64"))
    if (fs::dir_exists(file.path(install_dir, "lib")))
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

# Problem instances
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
  exdir <- fs::path_abs("data/qap")
  if (!fs::file_exists(file.path(exdir, "RS"))) {
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

# Main function
setup_scenario <- function(scenario_name, install_dir)
{
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
}

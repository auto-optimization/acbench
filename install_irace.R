
path <- c("./irace")
version <- "3.5"
library(devtools)
lib <- file.path(path, version)
dir.create(lib, recursive = TRUE)
devtools::install_version("irace", version = version,
                          upgrade = "never", lib = lib)

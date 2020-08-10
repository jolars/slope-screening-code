# prepare system for running simulations by downloading packages
# if they are missing

cran_packages <- c("renv")

# configure R to use R_LIBS_USER folder for packages
path <- Sys.getenv("R_LIBS_USER")

# # use tar on path instead
# Sys.setenv(R_BUILD_TAR = "tar")

if (!dir.exists(path)) {
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
}

for (pkg in cran_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, lib = path, repos = "https://cran.rstudio.com/")
  }
}

# make sure data folder is available
if (!dir.exists("data"))
  dir.create("data")

# reinstate renv library
renv::consent(provided = TRUE)
renv::activate()
renv::restore()

# Use Posit Package Manager binaries on WSL/Ubuntu for speed
Sys.setenv(RENV_CONFIG_REPOS_OVERRIDE = "https://packagemanager.posit.co/cran/__linux__/noble/latest")

# Use pak as renv's installer backend to detect system dependencies 
options(renv.config.pak.enabled = TRUE)

# Ensure renv itself is available before trying to activate the project.
# This handles opening the project on a fresh machine where renv is not yet installed.
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

# Activate the project library.
source("renv/activate.R")

# Ensure critical packages are present in the project library.
# - yaml: required by renv to parse dependencies
# - knitr, rmarkdown: required by Quarto
# These may not be auto-detected by renv because they are not explicitly loaded
# in the Quarto source files.
ensure_installed <- function(pkgs) {
  missing <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
  if (length(missing)) {
    renv::install(missing)
  }
}

ensure_installed(c("yaml", "knitr", "rmarkdown"))

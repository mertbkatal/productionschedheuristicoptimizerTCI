# install_packages.R
options(repos = c(CRAN = "https://cran.rstudio.com"))

required_packages <- c(
  "shiny", "shinyjs", "openxlsx", "plotly", 
  "DT", "readxl", "dplyr", "parallel"
)

# Install with error handling
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

# Verify installations
if (!all(sapply(required_packages, requireNamespace, quietly = TRUE))) {
  stop("Failed to install required packages")
}

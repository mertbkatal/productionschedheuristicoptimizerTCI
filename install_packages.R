# List of required packages
required_packages <- c(
  "shiny",
  "shinyjs",
  "openxlsx",
  "plotly",
  "DT",
  "readxl",
  "dplyr",
  "parallel"
)

# Install missing packages
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) {
  install.packages(new_packages, repos = "https://cran.rstudio.com")
}

# Verify all packages are installed
if(!all(required_packages %in% installed.packages()[,"Package"])) {
  stop("Failed to install all required packages")
}

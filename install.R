packages <- c(
  "shiny", "ggplot2", "readxl", "dplyr", "tidyr", "lubridate", "plotly", "shinydashboard"
  # Add any other packages your app uses
)

install.packages(setdiff(packages, rownames(installed.packages())))

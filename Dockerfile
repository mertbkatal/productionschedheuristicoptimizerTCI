FROM rocker/shiny:latest

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    && apt-get clean

# Install R packages needed for the app
RUN R -e "install.packages(c('shiny', 'shinyjs', 'openxlsx', 'plotly', 'DT', 'readxl', 'dplyr', 'ggplot2'), repos='https://cloud.r-project.org')"

# Copy app files to the container
COPY . /srv/shiny-server/

# Set correct permissions
RUN chown -R shiny:shiny /srv/shiny-server

# Expose default shiny port
EXPOSE 3838

# Run the app
CMD ["/usr/bin/shiny-server"]

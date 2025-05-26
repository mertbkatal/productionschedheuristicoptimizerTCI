FROM rocker/shiny:latest

# Install system libraries needed by some R packages
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

# Install R packages
RUN R -e "install.packages(c('shiny', 'shinyjs', 'openxlsx', 'plotly', 'DT', 'readxl', 'dplyr', 'parallel'), repos='https://cloud.r-project.org')"

# Copy the app to the container
COPY . /srv/shiny-server/

# Set permissions
RUN chown -R shiny:shiny /srv/shiny-server

# Expose port
EXPOSE 3838

# Start the Shiny app
CMD ["/usr/bin/shiny-server"]

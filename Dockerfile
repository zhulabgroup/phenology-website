# Use a base image with R and Shiny Server pre-installed
FROM rocker/shiny-verse:4.2.2

# Install system libraries for geospatial analysis
RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    libgdal-dev \
    libgeos-dev \
    libproj-dev

# Install R packages
RUN R -e "install.packages(c('aws.s3', 'imputeTS','ptw','geosphere', 'ggnewscale','ggridges', 'maps','mapproj', 'shinyjs', 'shinyscreenshot', 'digest', 'sp', 'gstat'), dependencies=TRUE)"

# Copy Shiny Server configuration with worker process settings
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

# Copy your Shiny app directory into the image
COPY phenowatch /srv/shiny-server/phenowatch

# Default CMD from rocker/shiny-verse starts Shiny Server
# This will use the configuration from shiny-server.conf which specifies 4 worker processes
# Shiny Server listens on 0.0.0.0:3838 by default
# Use a base image with R and Shiny Server pre-installed
FROM rocker/geospatial:4.4.0

# Install system libraries for geospatial analysis
RUN apt-get update

# Install R packages
RUN R -e "install.packages(c('shiny','tidyverse','aws.s3', 'imputeTS','ptw','geosphere', 'ggnewscale','ggridges', 'maps', 'shinyjs', 'shinyscreenshot', 'digest'), dependencies=TRUE)"

# Create the directory for Shiny apps
RUN mkdir -p /srv/shiny-server/

# Copy your Shiny app directory into the image
COPY app.R /srv/shiny-server/app.R
# COPY phenoinfo /srv/shiny-server/phenoinfo
COPY phenowatch /srv/shiny-server/phenowatch
# COPY phenoforecast /srv/shiny-server/phenoforecast
# RUN chmod 777 /srv/shiny-server/phenoforecast

# Expose the default Shiny Server port (optional if not changed)
EXPOSE 3838

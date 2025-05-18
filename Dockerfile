# Use a base image
FROM rocker/geospatial:4.4.0

# Install system libraries for geospatial analysis
RUN apt-get update

# Install R packages
RUN R -e "install.packages(c('shiny','tidyverse','aws.s3', 'imputeTS','ptw','geosphere', 'ggnewscale','ggridges', 'maps','mapproj', 'shinyjs', 'shinyscreenshot', 'digest'), dependencies=TRUE)"

# Create app directory and set correct permissions
RUN mkdir -p /srv/shiny-server
COPY phenowatch/app.R /srv/shiny-server/app.R

# Set permissions for Shiny to read and execute everything
RUN chmod -R 755 /srv/shiny-server

# Expose the default Shiny Server port (optional if not changed)
EXPOSE 3838

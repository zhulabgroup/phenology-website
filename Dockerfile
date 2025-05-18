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

# Copy your Shiny app directory into the image
COPY app.R /srv/shiny-server/app.R
COPY phenowatch /srv/shiny-server/phenowatch

# Expose the default Shiny Server port (optional if not changed)
EXPOSE 3838
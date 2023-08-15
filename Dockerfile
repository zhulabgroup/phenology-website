# Use a base image with R and Shiny Server pre-installed
FROM rocker/shiny-verse:latest

# Install system libraries for geospatial analysis
RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libmysqlclient-dev

# Install R packages
RUN R -e "install.packages(c( 'shinythemes','shinyjs', 'shinyscreenshot', 'geosphere', 'raster', 'gstat', 'ggpubr', 'gridExtra', 'maps', 'rnpn','leaflet', 'terra','colorRamps', 'lubridate','digest','aws.s3','ptw','doSNOW'), dependencies=TRUE)"

# Copy your Shiny app directory into the image
# COPY sample /srv/shiny-server/sample
COPY phenology-website/phenoinfo /srv/shiny-server/phenoinfo
COPY phenology-website/phenowatch /srv/shiny-server/phenowatch
COPY phenology-website/phenoforecast /srv/shiny-server/phenoforecast
RUN mkdir /srv/shiny-server/phenoforecast/data && chmod 777 /srv/shiny-server/phenoforecast/data

# Expose the default Shiny Server port (optional if not changed)
EXPOSE 3838
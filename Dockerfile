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
RUN R -e "install.packages(c('shinyjs', 'shinyscreenshot', 'geosphere', 'imputeTS','gstat', 'ggpubr', 'gridExtra', 'maps', 'rnpn','leaflet', 'terra','colorRamps', 'lubridate','digest','aws.s3','ptw','doSNOW','svglite','ggnewscale','ggridges'), dependencies=TRUE)"

# Copy your Shiny app directory into the image
# COPY phenoinfo /srv/shiny-server/phenoinfo
COPY phenowatch /srv/shiny-server/phenowatch
# COPY phenoforecast /srv/shiny-server/phenoforecast
# RUN chmod 777 /srv/shiny-server/phenoforecast

# Expose the default Shiny Server port (optional if not changed)
EXPOSE 3838

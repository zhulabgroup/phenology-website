# Phenoforecast Dockerfile
# Use a base image with R, Shiny, Tidyverse and Geospatial libraries installed
FROM rocker/geospatial:latest

WORKDIR /app

# Copy your local repo into the Docker container
COPY . .

# Install necessary R packages
RUN R -e "install.packages(c('leaflet', 'terra', 'raster', 'colorRamps', 'lubridate', 'shinyscreenshot', 'digest', 'shinyjs'), dependencies=TRUE, repos='http://cran.rstudio.com/')"

# Start shiny server when container is run
EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('/app', host='0.0.0.0', port=3838)"]

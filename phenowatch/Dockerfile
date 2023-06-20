# Phenowatch Dockerfile
# Use a base image with R installed
#FROM rocker/r-base:latest
#FROM rocker/tidyverse:latest
FROM rocker/shiny-verse:latest
# runs on 3838 by default


WORKDIR /app

# Ensures that the system's package repository is updated and all system packages are upgraded
RUN apt-get update && apt-get upgrade -y

# Install necessary R packages
RUN R -e "install.packages('tidyverse', repos = 'http://cran.us.r-project.org', dependencies=TRUE)"
RUN R -e "install.packages(c('shiny', 'shinythemes', 'shinyjs', 'shinyscreenshot',  'raster', 'gstat', 'ggpubr', 'gridExtra', 'maps', 'rnpn'), repos = 'http://cran.us.r-project.org', dependencies=TRUE)"

# Copy your local repo into the Docker container
COPY . .

# Start shiny server when container is run
EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('/app', host='0.0.0.0', port=3838)"]

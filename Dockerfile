# Use a base image
FROM rocker/geospatial:4.4.0

# Install dependencies for shiny-server
RUN apt-get update

# Download and install Shiny Server
RUN wget https://download3.rstudio.org/centos8/x86_64/shiny-server-1.5.23.1030-x86_64.rpm \
    && sudo yum install --nogpgcheck shiny-server-1.5.23.1030-x86_64.rpm \
    && rm shiny-server-1.5.23.1030-x86_64.rpm

# Install R packages
RUN R -e "install.packages(c('shiny','tidyverse','aws.s3', 'imputeTS','ptw','geosphere', 'ggnewscale','ggridges', 'maps','mapproj', 'shinyjs', 'shinyscreenshot', 'digest'), dependencies=TRUE)"

# Copy your Shiny app to the shiny-server directory
COPY app.R /srv/shiny-server/app.R
COPY phenowatch /srv/shiny-server/phenowatch

# Expose the default Shiny Server port (optional if not changed)
EXPOSE 3838

# Run Shiny Server
CMD ["/usr/bin/shiny-server"]

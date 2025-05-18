# Use a base image
FROM rocker/geospatial:4.4.0

# Install system libraries for geospatial analysis
RUN apt-get update

# Install R packages
RUN R -e "install.packages(c('shiny','tidyverse','aws.s3', 'imputeTS','ptw','geosphere', 'ggnewscale','ggridges', 'maps','mapproj', 'shinyjs', 'shinyscreenshot', 'digest'), dependencies=TRUE)"

COPY phenowatch /phenowatch
WORKDIR /phenowatch

# Expose the default Shiny Server port (optional if not changed)
EXPOSE 3838

# Copy the Shiny app files into the image
CMD ["Rscript", "app.R"]
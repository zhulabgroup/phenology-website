# Use a base image with R and Shiny Server pre-installed
FROM rocker/geospatial:4.4.0

# Install system libraries for geospatial analysis
RUN apt-get update

# Install R packages
RUN R -e "install.packages(c('shiny','tidyverse','aws.s3', 'imputeTS','ptw','geosphere', 'ggnewscale','ggridges', 'maps','mapproj', 'shinyjs', 'shinyscreenshot', 'digest'), dependencies=TRUE)"

# Copy your Shiny app directory into the image
COPY app.R app.R

# Expose the default Shiny Server port (optional if not changed)
EXPOSE 3838

# Run the Shiny app
CMD ["Rscript", "app.R"]
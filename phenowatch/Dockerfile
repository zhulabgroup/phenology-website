# Phenowatch Dockerfile
# Use a base image with R installed
FROM rocker/r-base:latest

WORKDIR /app

# Install necessary R packages --> Stuff for app
RUN R -e "install.packages(c('shiny', 'shinythemes'), dependencies=TRUE)"

# Copy your local repo into the Docker container
COPY . .

# Start shiny server when container is run
EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('/app', host='0.0.0.0', port=3838)"]

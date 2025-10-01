# Use the official Rocker Shiny image as base
FROM rocker/shiny:latest
# Install system dependencies for spatial packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libv8-dev \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libmagick++-dev \
    cmake \
    wget \
    libfontconfig1 \
    libfreetype6 \
    && rm -rf /var/lib/apt/lists/*
# Install required R packages (including spatial packages)
RUN R -e "options(repos = c(CRAN = 'https://p3m.dev/cran/__linux__/noble/latest')); \
    install.packages(c('shiny', 'leaflet', 'dplyr', 'readr', 'sf', 'DT', 'shinythemes', 'lwgeom', 'rnaturalearth', 'rnaturalearthdata', 'RColorBrewer', 'webshot', 'writexl', 'plotly', 'shinyjs', 'viridisLite', 'ggplot2', 'htmlwidgets', 'purrr'))"
# Install PhantomJS manually for webshot functionality
RUN R -e "webshot::install_phantomjs()"
RUN cp /root/bin/phantomjs /usr/local/bin/ && chmod +x /usr/local/bin/phantomjs
# Remove default shiny apps
RUN rm -rf /srv/shiny-server/*
# Copy your app files
COPY map.R /srv/shiny-server/app.R
COPY final_geo_table.csv /srv/shiny-server/
COPY variable_abbreviations.csv /srv/shiny-server/
COPY styles.css /srv/shiny-server/
# Set working directory
WORKDIR /srv/shiny-server
# Expose port 3838
EXPOSE 3838
# Run the shiny server
CMD ["/usr/bin/shiny-server"]
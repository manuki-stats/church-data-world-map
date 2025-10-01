# Use the official Rocker Shiny image as base
FROM rocker/shiny:latest
# Install system dependencies for spatial packages and headless Chrome (for webshot2)
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libnode-dev \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libmagick++-dev \
    cmake \
    wget \
    libfontconfig1 \
    libfreetype6 \
    chromium \
    libatk-bridge2.0-0 \
    libatk1.0-0 \
    libatspi2.0-0 \
    libcairo2 \
    libcups2 \
    libdbus-1-3 \
    libdrm2 \
    libgbm1 \
    libglib2.0-0 \
    libgtk-3-0 \
    libnspr4 \
    libnss3 \
    libpango-1.0-0 \
    libx11-6 \
    libxcb1 \
    libxcomposite1 \
    libxdamage1 \
    libxext6 \
    libxfixes3 \
    libxrandr2 \
    libxrender1 \
    fonts-liberation \
    libayatana-appindicator3-1 \
    libasound2 \
    libxshmfence1 \
    lsb-release \
    xdg-utils \
    && rm -rf /var/lib/apt/lists/*
ENV CHROMOTE_CHROME=/usr/bin/chromium
# Install required R packages (including spatial packages), replace webshot with webshot2
RUN R -e "options(repos = c(CRAN = 'https://p3m.dev/cran/__linux__/bookworm/latest')); \
    install.packages(c('shiny', 'leaflet', 'dplyr', 'readr', 'sf', 'DT', 'shinythemes', 'lwgeom', 'rnaturalearth', 'rnaturalearthdata', 'RColorBrewer', 'webshot2', 'writexl', 'plotly', 'shinyjs', 'viridisLite', 'ggplot2', 'htmlwidgets', 'purrr'))"
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
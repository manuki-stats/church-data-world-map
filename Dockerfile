# Use the official Rocker Shiny image as base
FROM rocker/shiny:latest

# Set environment variables
ENV DEBIAN_FRONTEND=noninteractive
ENV CHROME_BIN=/usr/bin/google-chrome
ENV CHROMOTE_CHROME=/usr/bin/google-chrome

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
    gnupg \
    libfontconfig1 \
    libfreetype6 \
    fonts-liberation \
    libnss3 \
    libxss1 \
    libappindicator3-1 \
    libasound2t64 \
    libatk-bridge2.0-0 \
    libatk1.0-0 \
    libcups2 \
    libdrm2 \
    libgbm1 \
    libgtk-3-0 \
    libnspr4 \
    libpango-1.0-0 \
    libxcomposite1 \
    libxdamage1 \
    libxfixes3 \
    libxkbcommon0 \
    libxrandr2 \
    xdg-utils \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Install Google Chrome properly
RUN wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | gpg --dearmor -o /usr/share/keyrings/google-chrome-keyring.gpg \
    && echo "deb [arch=amd64 signed-by=/usr/share/keyrings/google-chrome-keyring.gpg] http://dl.google.com/linux/chrome/deb/ stable main" > /etc/apt/sources.list.d/google-chrome.list \
    && apt-get update \
    && apt-get install -y google-chrome-stable \
    && rm -rf /var/lib/apt/lists/* \
    && google-chrome --version

# Install R packages in separate steps
# Install basic packages first
RUN R -e "options(repos = c(CRAN = 'https://p3m.dev/cran/__linux__/noble/latest')); \
    install.packages(c('shiny', 'dplyr', 'readr', 'DT', 'shinythemes'))"

# Install sf separately (it's often problematic)
RUN R -e "options(repos = c(CRAN = 'https://p3m.dev/cran/__linux__/noble/latest')); \
    install.packages('sf')" && \
    R -e "if (!require('sf')) stop('sf package failed to install')"

# Install other spatial packages
RUN R -e "options(repos = c(CRAN = 'https://p3m.dev/cran/__linux__/noble/latest')); \
    install.packages(c('lwgeom', 'rnaturalearth', 'rnaturalearthdata'))"

# Install leaflet and related packages
RUN R -e "options(repos = c(CRAN = 'https://p3m.dev/cran/__linux__/noble/latest')); \
    install.packages(c('leaflet', 'RColorBrewer', 'htmlwidgets'))"

# Install webshot2 and mapview for screenshots
RUN R -e "options(repos = c(CRAN = 'https://p3m.dev/cran/__linux__/noble/latest')); \
    install.packages(c('webshot2', 'chromote', 'processx', 'websocket', 'mapview'))"

# Install remaining packages including tmap for static maps
RUN R -e "options(repos = c(CRAN = 'https://p3m.dev/cran/__linux__/noble/latest')); \
    install.packages(c('writexl', 'plotly', 'shinyjs', 'viridisLite', 'ggplot2', 'purrr', 'tmap', 'tmaptools'))"

# Create and set proper permissions for tmp directories
RUN mkdir -p /tmp/chromium /tmp/shiny && \
    chmod -R 1777 /tmp && \
    chown -R shiny:shiny /tmp/chromium /tmp/shiny

# Remove default shiny apps
RUN rm -rf /srv/shiny-server/*

# Copy your app files
COPY map.R /srv/shiny-server/app.R
COPY final_geo_table.csv /srv/shiny-server/
COPY variable_abbreviations.csv /srv/shiny-server/
COPY styles.css /srv/shiny-server/

# Set working directory
WORKDIR /srv/shiny-server

# Ensure shiny user can write to necessary directories
RUN chown -R shiny:shiny /srv/shiny-server

# Expose port 3838
EXPOSE 3838

# Run the shiny server
CMD ["/usr/bin/shiny-server"]
# Base image
FROM rocker/shiny:4.5.1

# General updates
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get install -y git libxml2-dev libmagick++-dev libssl-dev libharfbuzz-dev libfribidi-dev && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# Install the required packages
# Option 1: Recreate the R environment using renv package
RUN Rscript -e 'install.packages(c("renv"))'
COPY /renv.lock /srv/shiny-server/renv.lock
RUN Rscript -e 'setwd("/srv/shiny-server/");renv::restore();'


# Option 2: Install R packages manually
# Command to install standard R packages from CRAN; enter the list of required packages for your app here
#RUN Rscript -e 'install.packages(c("shiny","tidyverse","BiocManager"), dependencies = TRUE)'
# Command to install packages from Bioconductor; enter the list of required Bioconductor packages for your app here
#RUN Rscript -e 'BiocManager::install(c("Biostrings"),ask = F)'

# Copy the app files (scripts, data, etc.)
RUN rm -rf /srv/shiny-server/*
COPY /app/ /srv/shiny-server/

# Ensure that the expected user is present in the container
RUN if id shiny &>/dev/null && [ "$(id -u shiny)" -ne 999 ]; then \
        userdel -r shiny; \
        id -u 999 &>/dev/null && userdel -r "$(id -un 999)"; \
    fi; \
    useradd -u 999 -m -s /bin/bash shiny; \
    chown -R shiny:shiny /srv/shiny-server/ /var/lib/shiny-server/ /var/log/shiny-server/

# Other settings
USER shiny
EXPOSE 3838

CMD ["/usr/bin/shiny-server"]

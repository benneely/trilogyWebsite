FROM rocker/shiny

MAINTAINER Ben Neely "ben.neely@duke.edu"

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libxml2-dev

RUN R -e "install.packages(c('XML', 'scales', 'ggplot2'), repos='http://cran.rstudio.com/')"

EXPOSE 3838


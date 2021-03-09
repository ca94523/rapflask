#!/bin/bash

set -e

#remove/add packages as necessary
#apt-get update -qq && apt-get -y --no-install-recommends install \
#    libxml2-dev \
#    libcairo2-dev \
#    libgit2-dev \
#    default-libmysqlclient-dev \
#    libpq-dev \
#    libsasl2-dev \
#    libsqlite3-dev \
#    libssh2-1-dev \
#    zlib1g-dev \
#    make \
#    unixodbc-dev && \
#  rm -rf /var/lib/apt/lists/*


#remove/add packages as necessary
#check here to check if binary, or source compiled, and check dependency
#https://packagemanager.rstudio.com/client/#/repos/1/packages
R_LIBS_SITE=/opt/R/4.0.4/lib/R/library
install2.r -l ${R_LIBS_SITE} --error --skipinstalled -r https://packagemanager.rstudio.com/all/latest -n 1 \
  dplyr \
  jsonlite \
  lubridate \
  reshape \
  timeDate \
  xgboost \
  zoo \
  stringr \
  data.table

# rm -rf /tmp/downloaded_packages

#!/bin/bash
set -e
apt-get update

apt-get -y install --no-install-recommends \
      ca-certificates \
      less \
      libopenblas-base \
      locales \
      vim-tiny \
      wget \
      dirmngr \
      gpg \
      gpg-agent \
      software-properties-common \
      apt-transport-https

#apt-key adv --keyserver keys.gnupg.net --recv-key 'E19F5F87128899B192B1A2C2AD5F960A256A04AF'
apt-key adv --keyserver keyserver.ubuntu.com --recv-key 'E19F5F87128899B192B1A2C2AD5F960A256A04AF'
add-apt-repository 'deb http://cloud.r-project.org/bin/linux/debian buster-cran40/'
apt-get update && apt-get -y install --no-install-recommends r-base r-base-dev
rm -rf /var/lib/apt/lists/*

#default install to /usr/lib/R
R_HOME=${R_HOME:-/usr/lib/R}


echo "options(repos = c(CRAN = 'https://packagemanager.rstudio.com/all/__linux__/bionic/latest'), download.file.method = 'libcurl')" >> /usr/lib/R/etc/Rprofile.site
echo "options(HTTPUserAgent = sprintf('R/%s R (%s)', getRversion(), paste(getRversion(), R.version$platform, R.version$arch, R.version$os)))" >> /usr/lib/R/etc/Rprofile.site
Rscript -e "install.packages(c('littler', 'docopt'), repos='https://packagemanager.rstudio.com/all/__linux__/bionic/latest')"

## By default R_LIBS_SITE is unset, and defaults to this, so this is where `littler` will be.
## We set it here for symlinks, but don't make the env var persist (since it's already the default)
R_LIBS_SITE=/usr/local/lib/R/site-library
ln -s ${R_LIBS_SITE}/littler/examples/install.r /usr/local/bin/install.r
ln -s ${R_LIBS_SITE}/littler/examples/install2.r /usr/local/bin/install2.r
ln -s ${R_LIBS_SITE}/littler/examples/installGithub.r /usr/local/bin/installGithub.r
ln -s ${R_LIBS_SITE}/littler/bin/r /usr/local/bin/r


#!/bin/bash
set -e
#refer to https://docs.rstudio.com/resources/install-r/
yum install -y https://dl.fedoraproject.org/pub/epel/epel-release-latest-8.noarch.rpm
export R_VERSION=4.0.4
curl -O https://cdn.rstudio.com/r/centos-8/pkgs/R-${R_VERSION}-1-1.x86_64.rpm
yum install -y R-${R_VERSION}-1-1.x86_64.rpm
/opt/R/${R_VERSION}/bin/R --version
ln -s /opt/R/${R_VERSION}/bin/R /usr/local/bin/R
ln -s /opt/R/${R_VERSION}/bin/Rscript /usr/local/bin/Rscript

R_HOME=${R_HOME:-/usr/local/bin/R}


Rscript -e "install.packages(c('littler', 'docopt'), repos='https://packagemanager.rstudio.com/all/latest')"
#saved to /opt/R/4.0.4/lib/R/library

## By default R_LIBS_SITE is unset, and defaults to this, so this is where `littler` will be.
## We set it here for symlinks, but don't make the env var persist (since it's already the default)
R_LIBS_SITE=/opt/R/4.0.4/lib/R/library
ln -s ${R_LIBS_SITE}/littler/examples/install.r /usr/local/bin/install.r
ln -s ${R_LIBS_SITE}/littler/examples/install2.r /usr/local/bin/install2.r
ln -s ${R_LIBS_SITE}/littler/examples/installGithub.r /usr/local/bin/installGithub.r
ln -s ${R_LIBS_SITE}/littler/bin/r /usr/local/bin/r

#export LD_LIBRARY_PATH="/opt/R/4.0.4/lib/R/lib:$LD_LIBRARY_PATH" #needed by rpy2

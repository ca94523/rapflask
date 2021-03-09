#!/usr/bin/env sh
set -e

export LD_LIBRARY_PATH="/opt/R/4.0.4/lib/R/lib:$LD_LIBRARY_PATH" #needed by rpy2
python -m rpy2.situation
exec gunicorn -k egg:meinheld#gunicorn_worker -c "/opt/app-root/gunicorn_conf.py" app.main:app --worker-tmp-dir /dev/shm
#/bin/bash
#FROM python:3.8
FROM registry.access.redhat.com/ubi8/python-38:latest
USER 0
WORKDIR /opt/app-root/
COPY r_scripts /opt/app-root/r_scripts
RUN chmod -R +x /opt/app-root/r_scripts
RUN /opt/app-root/r_scripts/install_r.sh
COPY ./requirements.txt /opt/app-root/requirements.txt
RUN pip install -r /opt/app-root/requirements.txt
RUN pip install meinheld gunicorn flask
RUN /opt/app-root/r_scripts/install_rpackage.sh
COPY ./app /opt/app-root/app
RUN chgrp -R 0 /opt/app-root/app && chmod -R g+rwX /opt/app-root/app
COPY ./gunicorn_conf.py /opt/app-root/gunicorn_conf.py
COPY ./start.sh /opt/app-root/start.sh
RUN chmod +x /opt/app-root/start.sh
COPY ./config.py /opt/app-root/config.py
RUN chown -R 1001:1001 /opt/app-root
USER 1001
EXPOSE 8000
CMD ["/opt/app-root/start.sh"]
#ENTRYPOINT /bin/bash

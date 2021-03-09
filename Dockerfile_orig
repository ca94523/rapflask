FROM python:3.8
RUN pip install meinheld gunicorn flask
#WORKDIR /app/
#ENV PYTHONPATH=/app
COPY r_scripts /r_scripts
RUN chmod -R +x /r_scripts
RUN /r_scripts/install_r.sh
COPY ./requirements.txt /requirements.txt
RUN pip install -r /requirements.txt
RUN /r_scripts/install_rpackage.sh
COPY ./app /app
RUN chgrp -R 0 /app && chmod -R g+rwX /app
COPY ./gunicorn_conf.py /gunicorn_conf.py
COPY ./entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh
COPY ./start.sh /start.sh
RUN chmod +x /start.sh
COPY ./config.py /config.py
EXPOSE 8000
ENTRYPOINT ["/entrypoint.sh"]
CMD ["/start.sh"]
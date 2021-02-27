import os
from app import create_app,db
from config import *
from flask import current_app
from app.models.cb_d.models import *

app = create_app(DevelopmentConfig)
app.app_context().push()

@app.shell_context_processor
def make_shell_context():
     return {'db': db, 'req' : Scorem1Model}

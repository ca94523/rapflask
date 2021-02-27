from flask import Flask
from flask_sqlalchemy import SQLAlchemy
from flask_migrate import Migrate
from flask_restx import Api
from config import *

db = SQLAlchemy()
migrate = Migrate()

def create_app(config_class=DevelopmentConfig):
    app = Flask(__name__)
    app.config.from_object(config_class)

    db.init_app(app)
    migrate.init_app(app,db)

    from app.controller.pdmodel import bp as pdmodel_bp
    from app.controller.pdmodel.pdmodel_controller import pd

    rap = Api(app, title='PD Model RAP', version='2.0', description='RAP untuk LOS digital')
    rap.add_namespace(pd)
    app.register_blueprint(pdmodel_bp)

    from app.controller.cb1 import bp as cb1_bp
    from app.controller.cb1.cb_controller import tot

    rap2 = Api(cb1_bp, title='CB1', version='1.0', description='CB1')
    rap2.add_namespace(tot)
    app.register_blueprint(cb1_bp)

    from app.controller.scorem1 import bp as scorem1_bp
    from app.controller.scorem1.m_controller import scorem1

    rap3 = Api(scorem1_bp,title='Scorem1', version='1.0',description='Scorem1')
    rap3.add_namespace(scorem1)
    app.register_blueprint(scorem1_bp)

    return app

from app import models
from app.models.cb_d import models
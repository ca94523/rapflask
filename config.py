import os

basedir = os.path.abspath(os.path.dirname(__file__))

username = os.getenv('USERNAME') or 'postgres'
password = os.getenv('PASSWORD') or 'postgres'
dbname = 'spark_beyond'

class Config(object):
    SECRET_KEY = os.getenv('SECRET_KEY') or 'kunci_rahasia_saya'
    DEBUG = False
    SQLALCHEMY_TRACK_MODIFICATIONS = False

class DevelopmentConfig(Config):
    # TODO : Delete before deployment to UAT
    username = "postgres"
    password = "postgres"
    SQLALCHEMY_DATABASE_URI = os.environ.get('DATABASE_URL') or \
        'sqlite:///' + os.path.join(basedir, 'app', 'app.db')
    DATABASE_URL = os.environ.get('DATABASE_URL') or 'postgresql://' + username + ":" \
                                    + password + "@10.255.4.8:5432" + "/" + dbname
    DEBUG = True

class TestingConfig(Config):
    DEBUG = True
    TESTING = True
    # SQLALCHEMY_DATABASE_URI = os.environ.get('DATABASE_URL') or 'postgresql://' + username + ":" \
    #                          + password + "@10.255.4.8:5432" + "/" + dbname

class ProductionConfig(Config):
    SQLALCHEMY_DATABASE_URI = os.environ.get('DATABASE_URL') or \
                              'sqlite:///' + os.path.join(basedir, 'app', 'app.db')
    DATABASE_URL = os.environ.get('DATABASE_URL') or 'postgresql://' + username + ":" \
                   + password + "@172.30.36.173:5432" + "/" + dbname
    DEBUG = False

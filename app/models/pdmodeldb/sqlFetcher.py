import psycopg2
import pandas as pd
import numpy as np
from flask import current_app

def check():
    return current_app.config['DATABASE_URL']

class sqlFetcher:
    def __init__(self, query, input):
        self.query  = query
        self.input = input
        self.conn = current_app.config['DATABASE_URL']

    def sql_result(self):
        con_string = self.conn
        # con_string = "host= 10.255.4.8 dbname='spark_beyond' user='postgres' password='postgres'"
        con = psycopg2.connect(con_string)
        cur = con.cursor()
        cur.execute(self.query % (self.input))
        hasil = pd.DataFrame(np.array(cur.fetchall()))
        columnname = [desc[0] for desc in cur.description]
        ncol = len(columnname)
        if hasil.empty:
            hasil = pd.DataFrame(np.zeros([0, ncol])*np.nan)
            hasil.columns = columnname
        else:
            hasil.columns = columnname
        cur.close()
        con.close()
        return hasil

    def null_helper(self,data ,column ,type ):
        if len(data)==0:
            data[column] = data[column]
        elif type == 'int':
            data[column] = int(data[column][0])
        elif type == 'string':
            data[column] = str(data[column][0])
        return data[column]

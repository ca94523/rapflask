import os
import pandas as pd
import json
from app.models.pdmodeldb.cfmodel import get_cf
from app.models.pdmodeldb.cdmodel import get_cd
from app.models.pdmodeldb.ebwmodel import get_ebw
from app.models.pdmodeldb.ddmodel import get_dd
# TODO coment both R_HOME before deployment
# R_HOME='/usr/lib/R'
# os.environ['R_HOME'] = 'C:/Users/david.sirait/Documents/R/R-4.0.3'
filepath = os.path.dirname(__file__)

import rpy2.robjects as robj
from rpy2.robjects import pandas2ri

r_source = robj.r['source']
r_source(filepath + "/functions.R")
pandas2ri.activate()

def empty_df(input_data,bureau):
    pd_bureau = pd.DataFrame({"CRN": input_data['DemographicData']['IDNumber'],
                              "xgb_bureau": None, "xgb_bureau_recode": None,
                              "bureau": bureau},index=[0])
    return pd_bureau

class pdScoringHelper:
    def __init__(self, json_input):
        self.json_input = json_input
        self.demog_data = json_input['DemographicData']
        self.input_data = robj.r['fromJSON'](json.dumps(self.json_input))

    def calculate_bureau(self):
        hasil_bureau_df = []
        if 'ResultFlag' in self.json_input:
            bureau = "SLIK"
            if not 'ResultIdeb' in self.json_input or len(self.json_input['ResultIdeb']) == 0:
                hasil_bureau_df = empty_df(self.json_input,bureau)
            else:
                pd_bureau = robj.globalenv['pdscore_bureau_slik'](self.input_data)
                hasil_bureau_df = pd.DataFrame(robj.conversion.rpy2py(pd_bureau))
                hasil_bureau_df['bureau'] = bureau
        elif 'ReportInfo' in self.json_input:
            bureau = "pefindo"
            if self.json_input['ReportInfo']['ReportStatus'] != 'ReportGenerated' or len(self.json_input['Contracts']) == 0:
                hasil_bureau_df = empty_df(self.json_input,bureau)
            else:
                a, b, c, d = robj.globalenv['pefindo_tables'](self.input_data)
                pd_bureau = robj.globalenv['pdscore_bureau_pefindo'](a, b, c, d)
                hasil_bureau_df = pd.DataFrame(robj.conversion.rpy2py(pd_bureau))
                hasil_bureau_df['bureau'] = bureau

        return hasil_bureau_df

    def calculate_demog(self):
        pd_demog = robj.globalenv['feature_engineering_demog'](self.input_data)
        hasil_demog = robj.conversion.rpy2py(pd_demog)
        return hasil_demog

    def calculate_banking_digital(self):
        cif = int(self.demog_data['CIF'])
        cfmast = get_cf(cif)
        ddmast, ddhist = get_dd(cif)
        cdmast, cdhist = get_cd(cif)
        ebwtr,ebwth = get_ebw(cif)
        pd_banking = robj.globalenv['pdscore_banking'](self.input_data,cfmast,ddmast,ddhist,cdmast,cdhist)
        pd_digital = robj.globalenv['pdscore_digital'](self.input_data,ddmast,ddhist,ebwtr,ebwth)
        return pd_banking,pd_digital

    def calculate_ntb(self):
        pd_bureau = self.calculate_bureau()
        pd_bureau_R = pandas2ri.py2rpy_pandasdataframe(pd_bureau)
        pd_demog = self.calculate_demog()

        pd_final = robj.globalenv['NTB_PDScore_final'](pd_bureau_R, pd_demog)
        hasil_ntb = robj.conversion.rpy2py(pd_final)

        return hasil_ntb
    
    def calculate_etb(self):
        pd_bureau = self.calculate_bureau()
        pd_bureau_R = pandas2ri.py2rpy_pandasdataframe(pd_bureau)
        pd_demog = self.calculate_demog()
        pd_banking, pd_digital = self.calculate_banking_digital()
        pd_final = robj.globalenv['ETB_PDScore_final'](pd_bureau_R, pd_demog,pd_banking,pd_digital)
        hasil_ntb = robj.conversion.rpy2py(pd_final)

        return hasil_ntb
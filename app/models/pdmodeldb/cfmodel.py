from .sqlFetcher import sqlFetcher
import pandas as pd

def get_cf(cif):
    cfmast = sqlFetcher("SELECT cfcif_2, cforgd, scd_start from sb.cfmast where cfcif_2 = '%s'", cif)
    cfmast_data = cfmast.sql_result()
    cfmast_data['scd_start'] = pd.to_datetime(cfmast_data['scd_start'])
    # cfmast_data_R = robj.conversion.py2rpy(cfmast_data)
    return cfmast_data
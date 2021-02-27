from .sqlFetcher import sqlFetcher
import pandas as pd

def get_cd(cif):
    cdmast = sqlFetcher("SELECT distinct acctno,cifno from sb.cdmast where cifno = '%s'",cif)
    cdmast_data = cdmast.sql_result()
    cdmast_data['cifno'] = cdmast.null_helper(cdmast_data,'cifno','string')
    cdmast_data['acctno'] = cdmast.null_helper(cdmast_data, 'acctno','string')
    # cdmast_data = robj.conversion.py2rpy(cdmast_data)
    ### PENGECEKAN DI CDHIST ###
    if len(cdmast_data) == 0:
        cdhist_data = pd.DataFrame()
        # cdhist_data = robj.conversion.py2rpy(cdhist_data)
    else:
        ## Ambil data acctno yang distinct ##
        acctno = cdmast_data['acctno']
        if len(acctno) > 1:
            acct = tuple(acctno.astype(str))
        else:
            acct = (acctno[0], "")
        cdhist= sqlFetcher(""" SELECT chacct, chdorc, trloca, chefdt FROM sb.cdhist WHERE chacct in %s; """, (acct,))
        cdhist_data = cdhist.sql_result()
        cdhist_data['trloca'] = cdhist.null_helper(cdhist_data,'trloca','int')
        cdhist_data['chefdt'] = pd.to_datetime(cdhist_data['chefdt'])
        #  cdhist_data = robj.conversion.py2rpy(ddhist_data)

    return cdmast_data,cdhist_data


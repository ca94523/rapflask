from .sqlFetcher import sqlFetcher
import pandas as pd

def get_dd(cif):
    ddmast = sqlFetcher("SELECT acctno, cifno, cbal, scd_start from sb.ddmast where cifno =  '%s'",cif)
    ddmast_data = ddmast.sql_result()
    ddmast_data['cbal'] = ddmast.null_helper(ddmast_data,'cbal','int')
    ddmast_data['scd_start'] = pd.to_datetime(ddmast_data['scd_start'])
    ddmast_data_n = ddmast_data.drop_duplicates('acctno')
    # ddmast_data = ro.conversion.py2rpy(ddmast_data)
    ### PENGECEKAN DI DDHIST ###
    if len(ddmast_data) == 0:
        ddhist_data = pd.DataFrame()
        # ddhist_data = ro.conversion.py2rpy(ddhist_data)
    else:
        ## Ambil data acctno yang distinct ##
        acctno = ddmast_data_n['acctno']
        if len(acctno) > 1:
            acct = tuple(acctno.astype(str))
        else:
            acct = (acctno[0], "")
        ddhist= sqlFetcher(""" SELECT tracct, trdorc, trloca, treffd,trbr from sb.ddhist 
                                where tracct in %s; """, (acct,))
        ddhist_data = ddhist.sql_result()
        ddhist_data['trloca'] = ddhist.null_helper(ddhist_data,'trloca','int')
        ddhist_data['treffd'] = pd.to_datetime(ddhist_data['treffd'])
        #  ddhist_data = ro.conversion.py2rpy(ddhist_data)

    return ddmast_data, ddhist_data

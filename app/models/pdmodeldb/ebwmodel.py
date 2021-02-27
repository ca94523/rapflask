from .sqlFetcher import sqlFetcher

def get_ebw(cif):
    ebwtr = sqlFetcher(
        "SELECT distinct on (cifnumber) cifnumber,membersince from sb.ebwmemberaccountchannel_tr "
        "where cifnumber = '%s' order by cifnumber ,membersince", cif)
    ebwtr_data = ebwtr.sql_result()
    # ebwmemberaccountchannel_tr3 = ro.conversion.py2rpy(ebwmemberaccountchannel_tr3)
    ebwth = sqlFetcher(
        "select distinct on (cifnumber) cifnumber ,membersince from sb.ebwmemberaccountchannel_th \
        where cifnumber = '%s' order by cifnumber ,membersince",cif)
    ebwth_data = ebwth.sql_result()
    # ebwmemberaccountchannel_th3 = ro.conversion.py2rpy(ebwmemberaccountchannel_th3)

    return ebwtr_data,ebwth_data

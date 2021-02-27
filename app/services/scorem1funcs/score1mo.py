import json
import pandas as pd
import numpy as np

""" 
 Feature Engineering Function
"""

def derive_cc_util(facdf):  # return a dict
    if facdf.empty or len(facdf) == 0:
        outputd = dict()
        outputd['ccutil'] = None
        outputd['numccactive'] = 0
        return outputd
    else:
        is_active = facdf['kondisiKet'].isin(['Fasilitas Aktif'])
        is_cc = facdf['jenisKreditPembiayaanKet'].isin(['Kartu Kredit atau Kartu Pembiayaan Syariah'])
        ccfac = facdf[is_active & is_cc]
        outputd = dict()
        if ccfac.shape[0] > 0:
            ccbalance = sum(filter(None, ccfac['bakiDebet'].astype(int)))  ##local value itu apa?
            cclimit = sum(filter(None, ccfac['plafonAwal'].astype(int)))  # TotalFacilityAmount.Value ???
            if cclimit > 0:
                ccutil = float(ccbalance) / float(cclimit)
            else:
                ccutil = 1.0
            outputd['ccutil'] = ccutil
            outputd['numccactive'] = ccfac.shape[0]
            return outputd
        else:
            outputd['ccutil'] = None
            outputd['numccactive'] = 0
            return outputd

def derive_all_dpd(facdf):
    outputd = dict()
    if facdf.empty:
        outputd['havecc'] = 'no'
        outputd['dpdfreq'] = None
        return outputd
    else:
        if facdf.shape[0] > 0:
            ccpayment = facdf
            # ccpayment = pmtdf[pmtdf['ContractCode'].isin(ccfac['ContractCode'])]
            dpd_per_month = []
            new = pd.DataFrame()
            for i in range(13, 25):
                i = '%02d' % i
                text = "tahunBulan%sHt" % (i)
                dpd_per_month.append(facdf[text])
            dpd_per_month = pd.DataFrame(np.array(dpd_per_month))
            dpd_per_month = dpd_per_month[dpd_per_month != ''].fillna(0).applymap(int)
            dpd_per_month = (dpd_per_month >= 30).replace(True, 1)
            dpd_per_month = dpd_per_month.replace(False, 0)
            dpd_freq = dpd_per_month.values.sum()
            outputd['havecc'] = 'yes'
            outputd['dpdfreq'] = dpd_freq
            return outputd
        else:
            outputd['havecc'] = 'no'
            outputd['dpdfreq'] = None
            return outputd


def derive_oldest_fac(facdf):  # return a dict
    if facdf.empty:
        outputd = dict()
        outputd['oldestmob'] = None
        return outputd
    else:
        outputd = dict()
        is_noncc = ~facdf['jenisKreditPembiayaanKet'].isin(['Kartu Kredit atau Kartu Pembiayaan Syariah'])
        facdf = facdf[is_noncc]
        if facdf.shape[0] > 0:
            oldestagreement = facdf.tanggalMulai.min()
            if pd.isnull(oldestagreement):
                oldestagreement = pd.to_datetime("today")
            oldestmob = pd.to_datetime("today").to_period('M') - pd.to_datetime(oldestagreement).to_period('M')
            outputd['oldestmob'] = oldestmob.n
            return outputd
        else:
            outputd['oldestmob'] = None
            return outputd

"""
    Scoring Function
"""

def cc_active_util_scoring(output_cc):
    cc_util = output_cc['ccutil']
    if cc_util == None:
        cc_util_score = 16
    elif cc_util < 0.15:
        cc_util_score = 46
    elif cc_util < 0.45:
        cc_util_score = 26
    elif cc_util < 0.6:
        cc_util_score = 15
    else:
        cc_util_score = 7
    return cc_util_score

def coverage_ratio(demog_data):
    job_type = demog_data['occupation']
    if job_type == "2" or job_type == "3":
        cov_ratio = 45
    else:
        cov_ratio = 17
    return cov_ratio

def all_dpd_freq_scoring(output_dpd):
    data= output_dpd
    dpd_freq_score = 0
    if data['havecc'] == 'no':
        dpd_freq_score = 16
    elif data['havecc'] == 'yes':
        if data['dpdfreq'] == 0:
            dpd_freq_score = 35
        elif data['dpdfreq'] >= 1:
            dpd_freq_score = 23
    return dpd_freq_score

def job_type(demog_data):
    job_type = demog_data['occupation']
    if job_type == "1" :
        job_score = 27
    else:
        job_score = 38
    return job_score

def proposed_tenor(demog_data):
    req_tnr = demog_data['requestTenor']
    if req_tnr < 48:
        tenor_score = 66
    elif req_tnr < 96:
        tenor_score = 43
    elif req_tnr < 156:
        tenor_score = 31
    else:
        tenor_score = 27
    return tenor_score

def oldest_fac_scoring(output_fac):
    """ 2. oldest length of credit history of all bank facilities
    """
    oldestmob = output_fac['oldestmob']
    if oldestmob == None or oldestmob < 12 :
        fac_score = 30
    elif oldestmob < 48:
        fac_score = 35
    else:
        fac_score = 37
    return fac_score

def num_active_cc(output_cc):
    data = output_cc['numccactive']
    if data == None or data == 0:
        cc_active_score = 18
    elif data < 5:
        cc_active_score = 34
    else:
        cc_active_score = 36
    return cc_active_score

"""
    Get score and result
"""

def get_scorem1(data, demog_data):
    slikdf = pd.DataFrame()
    if data['ResultIdeb'] is None or data['ResultIdeb'] == []:
        slikdf = pd.DataFrame()
    else :
        for fac in data['ResultIdeb']:
            fac_df = pd.json_normalize(fac['Ideb']['IdebIndividu']['individual']['fasilitas']['kreditPembiayan'])
            slikdf = slikdf.append(fac_df)

    cc_util_score = derive_cc_util(slikdf)
    dpd_freq_score = derive_all_dpd(slikdf)
    faclength_score = derive_oldest_fac(slikdf)

    score = (cc_active_util_scoring(cc_util_score) + coverage_ratio(demog_data) +
                 all_dpd_freq_scoring(dpd_freq_score) + job_type(demog_data) +
                 num_active_cc(cc_util_score) + proposed_tenor(demog_data) +
                 oldest_fac_scoring(faclength_score))
    if score >= 255:
        result = "Preapproved"
    elif score < 200:
        result = "Reject"
    else:
        result = "Normal Process"

    return score,result

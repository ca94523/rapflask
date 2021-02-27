import time
import json
from threading import Thread
import requests
from requests.api import get
from ..cbfunc.cbconn import get_cb_report
from ..scorem1funcs.score1mo import get_scorem1
from app import db
from app.models.cb_d.models import Scorem1Model
from flask import current_app

def occ_decode(data):
    if data == 'Self-Employed':
        occupation = '1'
    elif data == 'Employee':
        occupation = '2'
    else:
        occupation = '3'
    return occupation

class GetScorem1Result(Thread):
    def __init__(self, request):
        Thread.__init__(self)
        self.request = request
        self.app = current_app.app_context()

    def run(self):
        self.app.push()
        ref_id = self.request.json['request_reff_id']
        slik_result = self.request.json['slik_result']
        # TODO : turn on during deployment to UAT
        dataset = get_cb_report(ref_id) # get report data
        req_query = Scorem1Model.query.filter(Scorem1Model.reference_id == ref_id).first()
        demog_data = {"requestTenor": req_query.req_tnr, "occupation": occ_decode(req_query.occupation)}

        # do calculation
        score, result = get_scorem1(dataset, demog_data)

        # insert to db
        req_query.slik_result = slik_result
        req_query.score = score
        req_query.result = result
        
        # send to client
        # payload = {"fullName" : req_query.name, "referenceID": ref_id, "aipResult": aipresult}
        db.session.commit()
        # requests.post(url = "client/endpoint/TBD", data=json.dumps(payload), \
        #    headers={"Content-Type" : "application/json"})
        print("sending to client")
        self.app.pop()

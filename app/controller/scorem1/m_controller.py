from flask import request,abort,render_template
from flask_restx import Resource
from sqlalchemy.exc import IntegrityError 
from app.controller.rap_utils.dto import Scorem1
from app.controller.rap_utils.parser import *
from app.models.cb_d.models import *
from app import db
from app.services.cbfunc.cbinput import cbInputData
from app.services.cbfunc.cbconn import send_cb_request,get_cb_report
from ..scorem1 import bp
from app.services.scorem1funcs.score1mo import get_scorem1

scorem1 = Scorem1.rap
resp = Scorem1.res_1
result = Scorem1.res_2

def occ_helper(inputs):
    if inputs['occupation'] == "1":
        occupation = 'Self-Employed'
    elif inputs['occupation'] == "2":
        occupation = 'Employee'
    else:
        occupation = 'Professional'
    return occupation

def occ_decode(data):
    if data == 'Self-Employed':
        occupation = '1'
    elif data == 'Employee':
        occupation = '2'
    else:
        occupation = '3'
    return occupation

@scorem1.route('/mort/request')
class aScoreMort(Resource):
    @scorem1.marshal_with(resp)
    @scorem1.response(200, 'SUCCESS')
    def post(self):
        try:
            # validate inputs
            inputs = Scorem1Input().load(request.json)
            occupation = occ_helper(inputs)

            # insert input to database and get referenceID
            req = Scorem1Model(name=inputs['fullName'], req_tnr=inputs['requestTenor'], occupation=occupation)
            db.session.add(req)
            db.session.commit()
            req.create_ref()
            db.session.commit()

            # get report from cb
            inputs['custDob'] = inputs['custDob'].strftime("%Y-%m-%d")
            inputs['reference_id'] = req.reference_id
            cb_input = cbInputData().create_form(inputs)
            # TODO turn on during deployment
            send_cb_request(cb_input) # TODO TESTING
            return inputs # send to client
        except ValidationError as err: 
            db.session.rollback()
            return abort(400, err.messages)
        except IntegrityError as e:
            db.session.rollback()
            return abort(500, "Reference ID already exist")

@scorem1.route('/mort/getResult/')
class get_result(Resource):
    '''
    Trigger from clients
    '''
    @scorem1.response(200, 'SUCCESS')
    @scorem1.marshal_with(result)
    def post(self):
        try:
            ref_id = checkScoreResult().load(request.json) # check the inputs
            req_query = Scorem1Model.query.filter(Scorem1Model.reference_id == ref_id['referenceID']).first()
            if not req_query:
                return abort(400, "Reference ID not found")
            result = get_cb_report(ref_id) # retrieve the report
            if result['ResultFlag'] == False:
                return abort(400,"Result Not Ready")
            else:
                # insert to db
                demog_data = {"requestTenor": req_query.req_tnr, "occupation": occ_decode(req_query.occupation)}
                score, result = get_scorem1(result, demog_data)  # do calculation
                if result['ResultIdeb'] != []:
                    req_query.slik_result = 'FOUND'
                else:
                    req_query.slik_result = 'NOT FOUND'
                req_query.score = score
                req_query.result = result
                db.session.commit()
                payload = {"fullName": req_query.name, "referenceID": ref_id['ref_id'], "aipResult": result}
                return payload
        except ValidationError as err:
            return abort(400, err.messages)


@bp.route('/mort/checking', methods=['GET','POST'])
def get_result():
    score_result = Scorem1Model.query.order_by(Scorem1Model.id.desc()).limit(10)
    return render_template('result_page.html', title="A Score result", result = score_result)

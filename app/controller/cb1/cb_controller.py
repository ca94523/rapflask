from flask.json import jsonify
from flask import request,abort
from flask_restx import  Resource
from app.controller.rap_utils.dto import cb1
from ...models.cb_d.models import Scorem1Model
from sqlalchemy.exc import IntegrityError
from app import db
from ...services.cbfunc.cbhelper import GetScorem1Result

tot = cb1.rap
res = cb1.res

@tot.route('/SetSlikDone')
@tot.route('/setSlikDone')
class getResult(Resource):
    @tot.response(200,'SUCCESS')
    def post(self):
        if not request.json or not 'request_reff_id' in request.json:
            abort(400,description= "Invalid Request")
        req_query = Scorem1Model.query.filter(Scorem1Model.reference_id == \
                                              request.json['request_reff_id']).first()
        if req_query is None :
            abort(400, description="Reference Id not Found")
        else :
            try:
                db.session.rollback()
                thread_a = GetScorem1Result(request.__copy__())
                thread_a.start()
                return jsonify({"ResultFlag" : "True"})
            except IntegrityError :
                db.session.rollback()
                return jsonify({"ERROR" : "Result already exist for this reference id"}), 400

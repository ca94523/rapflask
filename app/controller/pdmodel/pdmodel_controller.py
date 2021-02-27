from flask import request,abort
from flask_restx import Resource
from app.controller.rap_utils.dto import Pdmodel
from app.services.pdfunctions.Rfunctions import *
# from app.models.sqlFetcher import check

pd = Pdmodel.rap
res = Pdmodel.res

## healthcheck
@pd.route('/healthy')
@pd.route('/healthz/ready')
@pd.route('/health')
class healthCheck(Resource):
    @pd.doc({"description":'HealthCheck'})
    @pd.response(200,'SUCCESS')
    def get(self):
        return "OK"
    def head(self):
        return "OK"


@pd.route('/slik')
@pd.route('/pefindo')
class CheckResult(Resource):
    @pd.doc({"description": 'Return pd score for LOS data'})
    @pd.marshal_with(res)
    @pd.response(200,'SUCCESS')
    def post(self):
        data = request.json
        if 'ReportInfo' not in data and 'ResultFlag' not in data:
            abort(400, "Invalid Bureau Data")
        else:
            # do PD calculation
            calc = pdScoringHelper(data)
            if data['DemographicData']['CustomerType'] == 'NTB':
                pd_final = calc.calculate_ntb().to_dict('records')
            else:
                pd_final = calc.calculate_etb().to_dict('records')
            return pd_final[0]
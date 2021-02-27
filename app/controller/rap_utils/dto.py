from flask_restx import Namespace,fields

class cb1:
    rap = Namespace('/', description='CB1')
    res = rap.model('result', {
        'ResultFlag': fields.String(description= "ResultFlag")
    })

class Pdmodel:
    rap = Namespace('/', description='PD Scoring for Digital Loans')
    res = rap.model('PD Output', {
        "ApplicationNumber": fields.String(required=True,description= "application number from LOS"),
        "GUID": fields.String(description= "GUID from LOS"),
        "Name": fields.String(description= "Customer Full Name"),
        "MarketingCode": fields.String(description= "Marketing Code from LOS"),
        "CustomerType": fields.String(required=True, description= "Is new or existing Customer"),
        "CIF": fields.String(required=True, description= "CIF data for existing customer"),
        "KTP": fields.String(required=True, description= "Customer's KTP", attribute='IDNumber'),
        "PD_Original": fields.Fixed(required=True, description="Original PD Score",
                                    attribute='pd',decimals=3),
        "PD_Recodification": fields.Fixed(required=True, description="Recodification Score",
                                          attribute='pd_recode',decimals=3)
    })

class Scorem1:
    rap = Namespace('v1', description='Scorem1')
    res_1 = rap.model('Response', {
        "fullName" : fields.String(required=True,description="Full Name"),
        "referenceID" : fields.String(required=True,description="Reference ID",attribute = 'reference_id'),
        "resultFlag" : fields.String(required=True,description="Request ResultFlag",default='OK')
    })
    res_2 = rap.model('Results', {
        "fullName" : fields.String(required=True,description="Full Name"),
        "referenceID" : fields.String(required=True,description="Reference ID"),
        "aipResult" : fields.String(required=True,description="Request ResultFlag")
    })
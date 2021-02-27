from pprint import pprint
from marshmallow import Schema,fields, ValidationError, validate

# Create mort input parser

class Scorem1Input(Schema):
    fullName = fields.String(required=True)
    custDob = fields.DateTime("%Y-%m-%d",required=True)
    custEktp = fields.String(required=True)
    gender = fields.Str(validate=validate.OneOf(['M','F']))
    occupation = fields.Str(validate=validate.OneOf(['1','2','3']))
    requestTenor = fields.Int(validate=validate.Range(min = 0, max = 400))
    monthlyIncome = fields.Int(required=True)
    spouseIncome = fields.Int()
    monthlySales = fields.Int(required =True)
    propertyValue = fields.Int(required=True)

    def __repr__(self):
        return "<Customer (name = {fullname}) > ".format(self=self)

class checkScoreResult(Schema):
    referenceId = fields.String(required=True)
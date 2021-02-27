from app import db

class Scorem1Model(db.Model):
    __tablename__ = "scorem1table"
    id = db.Column(db.Integer,primary_key=True)
    reference_id = db.Column(db.String(64), index=True, unique=True)
    name = db.Column(db.String(120), index=True)
    req_tnr = db.Column(db.Integer)
    occupation = db.Column(db.String(30))
    slik_result = db.Column(db.String(20))
    score = db.Column(db.Integer)
    result= db.Column(db.String(20))

    def create_ref(self):
        self.reference_id = "PROS0000" + str(self.id)
    
    def __repr__(self):
        return '<Reference_id {}>'.format(self.reference_id)

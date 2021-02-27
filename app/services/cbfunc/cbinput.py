
class cbInputData:
    def __init__(self,**kwargs):
        self.npwp = kwargs.get('npwp', None)
        self.pob = kwargs.get('pob',None)
        self.address = kwargs.get('address', None)
        self.mother_name = kwargs.get('mother_name',None)
        self.office_name = kwargs.get('office_name',None)
        self.custtype = "1"
        self.product = "CA"
        self.channel = "pdmodel" # TODO might need to change to 'pdmodel' during deployment
        
    def create_form(self,input_data):
        dynamic_data = {"request_reff_id" : input_data['reference_id'], 
                    "application_reff_id" : str(input_data['reference_id'] + "-1"),
                    "name" : input_data['fullName'] , "dob": input_data['custDob'],
                    "ktp" : input_data['custEktp'], "gender" : input_data['gender']}
        form_data = {**self.__dict__, **dynamic_data}
        return form_data
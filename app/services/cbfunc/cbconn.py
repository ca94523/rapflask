import requests
import json

headers = {"Content-Type" : "application/json", "Accept": "application/json"}
def send_cb_request(cbas_input):
    url = "http://10.255.7.250/CBASAPI/Request"
    payload = json.dumps(cbas_input)
    response = requests.post(url=url, data=payload, headers=headers)
    return response.json()

def get_cb_report(input_id):
    url = "http://10.255.7.250/CBASAPI/getResult"
    payload = {"request_reff_id": input_id}
    response = requests.post(url=url, data=json.dumps(payload), headers=headers)
    return response.json()

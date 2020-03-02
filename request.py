import requests



url = 'http://localhost:5000/predict_api'

r = requests.post(url,json={'zip_code_3_digits':147,'Code_illness':3,'ratio_of_total_costs_to_total_charges':1.3847943130000002,'Age':'30 to 49', 'Gender':'F','Cultural_group':'White', 'ethnicity':'Not Span/Hispanic', 'Admission_Type':'Elective', 'Home_or_self_care':'Expired','apr_mdc_description':'Diseases and Disorders of the Respiratory System','Description_illness':'Moderate', 'Mortality_Risk':'Minor','Surg_Description':'Medical', 'Payment_typology_1':'Medicare', 'Emergency_dept_yes_no':'N'})


print(r.json())
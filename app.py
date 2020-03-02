import numpy as np

from flask import Flask, request, jsonify, render_template

import pickle

from sklearn.preprocessing import LabelEncoder



app = Flask(__name__)

model = pickle.load(open('model2.pkl', 'rb'))



@app.route('/')

def home():

    return render_template('index.html')



@app.route('/predict',methods=['POST'])

def predict():

    '''

    For rendering results on HTML GUI

    '''

    int_features = [(x) for x in request.form.values()]
    
    LE=LabelEncoder()
    int_features=LE.fit_transform(int_features)

    final_features = [np.array(int_features)]

    prediction = model.predict(final_features)
    
    output = prediction

    return render_template('index.html', prediction_text='The insurance is {}'.format(output))



@app.route('/predict_api',methods=['POST'])

def predict_api():

    '''

    For direct API calls trought request

    '''

    data = request.get_json(force=True)

    prediction = model.predict([np.array(list(data.values()))])



    output = prediction

    return jsonify(output)



if __name__ == "__main__":

    app.run(debug=True)
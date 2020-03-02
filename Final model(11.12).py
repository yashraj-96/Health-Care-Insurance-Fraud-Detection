#Including the required libraries

import pandas as pd
import numpy as np
from numpy import array
import seaborn as sns
import matplotlib.pyplot as plt
from sklearn.linear_model import RidgeCV, LassoCV, Ridge, Lasso
from sklearn import preprocessing

from imblearn.over_sampling import SMOTE
from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import train_test_split
from sklearn.metrics import confusion_matrix
import itertools
from sklearn.metrics import classification_report, confusion_matrix

#Importing the dataset
missing_values = ["n/a", "na", "--"]
ins = pd.read_csv("G://Data Science Project//ins.csv", na_values = missing_values)
#ins=pd.read_csv("G://Data Science Project//ins.csv")
df=pd.DataFrame(ins)
print(df)

#Renaming the columns
df=df.rename(columns={'Home or self care':'Home_Selfcare','Mortality risk':'Mortality_Risk','Emergency dept_yes/No':'Emergency_dept_yes/No'})
print(df)


#Replacing the columns with NA values with their mode/median
df.columns
modeb1=df['Area_Service'].mode()
print(modeb1)

modeb2=df['Hospital County'].mode()
print(modeb2)

modeb22=df['Description_illness'].mode()
print(modeb22)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    

modeb23=df['Mortality_Risk'].mode()                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
print(modeb23)     

modeb3=df['Certificate_num'].mode()                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
print(modeb3)

medianb4=df['Hospital Id'].median()
print(medianb4)

#The following two columns should be dropped as they have MCAR ratio beyond the threshold (>0.5)
df.drop(['payment_typology_2','payment_typology_3'],inplace= True,axis=1)

#Replacing a row name in zip_code column
df['zip_code_3_digits']=df['zip_code_3_digits'].replace({'OOS':'0'})
df=df.replace({'nan':'na'})


medianb7=df['zip_code_3_digits'].median()
print(medianb7)


#Dropping the columns from the dataframe whose mode and median is being calculated
#df.drop(['Area_Service','Hospital County','Description_illness','Mortality_Risk','Certificate_num','Hospital Id','zip_code_3_digits'],inplace=True,axis=1)
#Replacing the NA values
df['Area_Service'].fillna('Hudson Valley',inplace=True)
df['Hospital County'].fillna('Erie',inplace=True,axis=0)
df['Description_illness'].fillna('Moderate',inplace=True,axis=0)
df['Mortality_Risk'].fillna('Minor',inplace=True)
df['Certificate_num'].fillna('1401014',inplace=True)
df['Hospital Id'].fillna(630,inplace=True)
df['zip_code_3_digits'].fillna(130,inplace=True)

df.isnull().sum()
####################################################################################################
###############################################################################################
#Feature Engineering
#Label Encoder for converting the features with string to numeric
x1=df.iloc[:,0]
x2=df.iloc[:,1]
x3=df.iloc[:,4]
x4=df.iloc[:,5]
x5=df.iloc[:,7]
x6=df.iloc[:,8]
x7=df.iloc[:,9]
x8=df.iloc[:,11]
x9=df.iloc[:,12]
x10=df.iloc[:,15]
x11=df.iloc[:,17]
x12=df.iloc[:,18]
x13=df.iloc[:,19]
x14=df.iloc[:,21]
x15=df.iloc[:,22]
x16=df.iloc[:,23]
x17=df.iloc[:,24]
x18=df.iloc[:,26]
x19=df.iloc[:,27]
#x20=df.iloc[:,31]

y1=array(x1)
y2=array(x2)
y3=array(x3)
y4=array(x4)
y5=array(x5)
y6=array(x6)
y7=array(x7)
y8=array(x8)
y9=array(x9)
y10=array(x10)
y11=array(x11)
y12=array(x12)
y13=array(x13)
y14=array(x14)
y15=array(x15)
y16=array(x16)
y17=array(x17)
y18=array(x18)
y19=array(x19)
#y20=array(x20)

from sklearn.preprocessing import LabelEncoder
LE=LabelEncoder()
Area_Service=LE.fit_transform(y1)
print(Area_Service)
Hospital_County=LE.fit_transform(y2)
print(Hospital_County)
Hospital_Name=LE.fit_transform(y3)
print(Hospital_Name)
Age=LE.fit_transform(y4)
Gender=LE.fit_transform(y5)
Cultural_group=LE.fit_transform(y6)
ethnicity=LE.fit_transform(y7)
Admission_Type=LE.fit_transform(y8)
Home_or_self_care=LE.fit_transform(y9)
ccs_diagnosis_description=LE.fit_transform(y10)
ccs_procedure_description=LE.fit_transform(y11)
apr_drg_description=LE.fit_transform(y12)
apr_mdc_description=LE.fit_transform(y13)
Description_illness=LE.fit_transform(y14)
Mortality_Risk=LE.fit_transform(y15)
Surg_Description=LE.fit_transform(y16)
Payment_typology_1=LE.fit_transform(y17)
Abortion=LE.fit_transform(y18)
Emergency_dept_yes_no=LE.fit_transform(y19)
#Result=LE.fit_transform(y20)


df_test=df
df_test.columns
df_test.drop(['Area_Service','Hospital County','Hospital Name','Age','Gender','Cultural_group','ethnicity','Admission_type','Home or self care,'],inplace=True,axis=1)
df_test.drop(['ccs_diagnosis_description','ccs_procedure_description','apr_drg_description','apr_mdc_description','Description_illness','Mortality_Risk','Surg_Description','Payment_typology_1','Abortion','Emergency_dept_yes/No'],inplace=True,axis=1)


#Adding the integer encoded columns to the dataframe
df_test['Area_Service']=Area_Service
df_test['Hospital County']=Hospital_County
df_test['Hospital Name']= Hospital_Name
df_test['Age']=Age
df_test['Gender']=Gender
df_test['Cultural_group']=Cultural_group
df_test['ethnicity']=ethnicity
df_test['Admission_Type']=Admission_Type
df_test['Home_or_self_care']=Home_or_self_care
df_test['ccs_diagnosis_description']=ccs_diagnosis_description
df_test['ccs_procedure_description']=ccs_procedure_description
df_test['apr_drg_description']=apr_drg_description
df_test['apr_mdc_description']=apr_mdc_description
df_test['Description_illness']=Description_illness
df_test['Mortality_Risk']=Mortality_Risk
df_test['Surg_Description']=Surg_Description
df_test['Payment_typology_1']=Payment_typology_1
df_test['Abortion']=Abortion
df_test['Emergency_dept_yes_no']=Emergency_dept_yes_no
#df_test['Result']=Result

######################################################################
#Days_spend_hpstl has an issue while conversion and building model
#Deleting the features based on the Exploratary Data Analysis results
df_test.drop(['Days_spend_hsptl'],inplace=True,axis=1)
df_test.drop(['Certificate_num'],inplace=True,axis=1)
df_test.drop(['ccs_diagnosis_description','ccs_procedure_description','Weight_baby','year_discharge'],inplace=True,axis=1)
#df_26vr=df_test
###########################################################################
#Converting the data type of the features of other type
print(df_test.dtypes)
df_test['zip_code_3_digits']=df_test['zip_code_3_digits'].astype(float)

#df_test['Result']=ins['Result']
################################################################################
#Re-arranging the order of columns
df_test.columns
df_test=df_test[['Hospital Id', 'zip_code_3_digits', 'ccs_diagnosis_code',
       'ccs_procedure_code', 'Code_illness', 'Tot_charg', 'Tot_cost',
       'ratio_of_total_costs_to_total_charges', 'Area_Service',
       'Hospital County', 'Hospital Name', 'Age', 'Gender', 'Cultural_group',
       'ethnicity', 'Admission_Type', 'Home_or_self_care',
       'apr_drg_description', 'apr_mdc_description', 'Description_illness',
       'Mortality_Risk', 'Surg_Description', 'Payment_typology_1', 'Abortion',
       'Emergency_dept_yes_no','Result']]
################################################################################
#RFE method for feature selection
from sklearn.feature_selection import RFE
from sklearn.linear_model import LogisticRegression
#dataframe = read_csv(url, names=names)
array = df_test.values
X_FS = array[:,0:25]
Y_FS = array[:,25]
# feature extraction
model = LogisticRegression(solver='lbfgs')
rfe = RFE(model, 15)
fit = rfe.fit(X_FS,Y_FS)
print("Num Features: %d" % fit.n_features_)
print("Selected Features: %s" % fit.support_)
print("Feature Ranking: %s" % fit.ranking_)

#Creating a dataframe for selected 15 columns

rfe=pd.DataFrame(df_test)
rfe.drop(['Hospital Id','ccs_diagnosis_code','ccs_procedure_code','Tot_charg', 'Tot_cost','Area_Service', 'Hospital County', 'Hospital Name', 'apr_drg_description','Abortion',],inplace=True,axis=1)
rfe.columns
rfe.shape
X_rfe=rfe.iloc[:,0:15]
Y_rfe= rfe.iloc[:,15]

#################################################################################
#Function used to plot confusion matrix
def plot_confusion_matrix(cm, classes,
                          normalize=False,
                          title='Confusion matrix',
                          cmap=plt.cm.Blues):
    """
    This function prints and plots the confusion matrix.
    Normalization can be applied by setting `normalize=True`.
    """
    if normalize:
        cm = cm.astype('float') / cm.sum(axis=1)[:, np.newaxis]
        print("Normalized confusion matrix")
    else:
        print('Confusion matrix, without normalization')

    print(cm)

    plt.imshow(cm, interpolation='nearest', cmap=cmap)
    plt.title(title)
    plt.colorbar()
    tick_marks = np.arange(len(classes))
    plt.xticks(tick_marks, classes, rotation=45)
    plt.yticks(tick_marks, classes)

    fmt = '.2f' if normalize else 'd'
    thresh = cm.max() / 2.
    for i, j in itertools.product(range(cm.shape[0]), range(cm.shape[1])):
        plt.text(j, i, format(cm[i, j], fmt),
                 horizontalalignment="center",
                 color="white" if cm[i, j] > thresh else "black")

    plt.ylabel('True label')
    plt.xlabel('Predicted label')
    plt.tight_layout()
    
    

# To check the count of fraudulent and normal transactions
sns.countplot(df_test['Result'],facecolor=(0, 0, 0, 0),linewidth=5,edgecolor=sns.color_palette("dark", 3), label = "Count")

# Now Checking actual number of fraudulent transactions
fraud_indices=np.array(df_test[df_test.Result==0].index)
no_records_fraud=len(fraud_indices)
normal_indices=np.array(df_test[df_test.Result==1].index)
no_records_normal=len(normal_indices)

print("No. of Fraudulent Transaction is {} and No. of Normal Transaction is {}".format(no_records_fraud, no_records_normal))


#Finding correlation with target variable
data = rfe.drop(columns = ['Result'])
data.corrwith(rfe.Result).plot.bar(
        figsize = (20, 10), title = "Correlation with Class", fontsize = 20,
        rot = 45, grid = True)


##########################################################################################
from sklearn import preprocessing
min_max_scaler = preprocessing.MinMaxScaler()
X_scale = min_max_scaler.fit_transform(X_rfe)


##########################################################################################

#SMOTE (Over-sampling)
X_resample,y_resample=SMOTE().fit_sample(X_rfe,Y_rfe.values.ravel())

y_resample=pd.DataFrame(y_resample)
X_resample=pd.DataFrame(X_resample)

X_train, X_test, y_train, y_test = train_test_split(X_resample, y_resample, test_size = 0.3, random_state=0)
X_train.shape
Y_rfe.shape
from sklearn.ensemble import RandomForestClassifier
random_forest=RandomForestClassifier(n_estimators=100)

random_forest.fit(X_train,y_train)


y_pred=random_forest.predict(X_test)
    random_forest.score(X_test,y_test)


#Confusion matrix

cnf_matrix=confusion_matrix(y_test,y_pred)
plot_confusion_matrix(cnf_matrix,classes=[0,1])
plt.show()
    


#Accuracy
from sklearn.metrics import accuracy_score
accuracy = accuracy_score(y_test, y_pred)
print("Accuracy: %.2f%%" % (accuracy * 100.0))


#Classification report
print(classification_report(y_test, y_pred))


#Specificity  and Sensitivity


from imblearn.metrics import sensitivity_specificity_support

s=sensitivity_specificity_support
print(s)

s(y_test, y_pred, average='weighted')


###############################################################################################################
#Deploying the model
import pickle
# Saving model to disk
pickle.dump(random_forest, open('model2.pkl','wb'))

# Loading model to compare the results
model = pickle.load(open('model2.pkl','rb'))

score = model.score(X_test, y_test)
print("Test score: {0:.2f} %".format(100 * score))
Ypredict = model.predict(X_test)


###########################################################################################################



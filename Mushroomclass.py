'''Brittany Hayes
   Artificial Intelligence
   classify mushrooms as either poisonous (1) or edible (0)'''

import numpy as np
import pandas as pd
from sklearn.cross_validation import train_test_split
from sknn.mlp import  Layer, Classifier
from sklearn.metrics import confusion_matrix
import datetime
# Load data
df = pd.read_csv('mushroomwNames.csv', sep = ',', header = 0)
#separate the target variable
y = df['edibility']
#convert the levels to be binary for the target variable
y = y.replace(['p','e'],[1,0])
# drop the target variable from the predictors
X = df.drop('edibility', 1) 
#print pd.DataFrame.head(X,n=5)
#get dummy variables for all of the categorical variables
X = pd.get_dummies(X)
X = np.array(X)
# Split the data into a test set and a training set
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, random_state=42)

#create the neural net
fit1 = Classifier(layers= [Layer("Sigmoid",units=7),
                  Layer("Softmax")],
                  learning_momentum=0.00,
                  learning_rate=0.80,
                  random_state=42,
                  valid_size=0.25,
                  n_iter=10)


#fit the model
start_time = datetime.datetime.now()
fit1.fit(X_train,y_train)
stop_time = datetime.datetime.now()
print "Time required for training:",stop_time - start_time

#get results for the training set
pred1_train=fit1.predict(X_train)
#train confusion matrix
confu1=confusion_matrix(y_train,pred1_train)
print "confusion matrix for the training set:"
print confu1
#train accuracy
score1=fit1.score(X_train, y_train)
print "training success rate: ", score1

#get results for the test set
pred2_test=fit1.predict(X_test)
#test confusion matrix
confu2=confusion_matrix(y_test,pred2_test)
print "confusion matrix for the test set:"
print confu2
#test accuracy
score2=fit1.score(X_test, y_test)
print "test success rate: ", score2
rm(list=ls())
#Brittany Hayes
#####################
#### SECTION 1 ####
#####################
X<-matrix(c(1,0,1,3,3,1,0,11,2,1,1,7), ncol = 3,nrow = 4)
I<-matrix(c(1,1,1,1), ncol = 1)
#center the columns of X
n<-nrow(X)
X<-X-I%*%t(I)%*%X*(1/n)
#check the mean of each column.
mean(X[,1])
mean(X[,2])
mean(X[,3])
#scale each column
X<-apply(X,MARGIN = 2, FUN = function(m) (m)/sd(m))
#make sure the standard deviation of each column is 1
sd(X[,1])
sd(X[,2])
sd(X[,3])
#calculate the covariance matrix
sigma<-(t(X)%*%X)/(n-1)
#check the result
cov(X)
#extract the eigenvectors for sigma
eigen(sigma)
#check the calculations
X<-matrix(c(1,0,1,3,3,1,0,11,2,1,1,7), ncol = 3,nrow = 4)
X<-scale(X, center = TRUE, scale = TRUE)
components<-prcomp(X)
#the calculations check out, so now we want to project the 3D values onto a 2D plane
U<-components[2]$rotation[,1:2]
Z<-X%*%U
#Display Z
Z

#####################
#### SECTION 2 ####
#####################
rm(list=ls())
library(pls)
library(ISLR)
set.seed(2)
par(mfrow=c(1,1))
#prepare the data by excluding blank values
Hitters=na.omit(Hitters)
x<-model.matrix(Salary~.,Hitters)[,-1]
y<-Hitters$Salary
#now fit with pcr
pcr.fit=pcr(Salary~.,data=Hitters, scale=TRUE,validation = "CV")
summary(pcr.fit)
#plot the cross-validation scores
validationplot(pcr.fit,val.type="MSEP")
#perform pcr on the training data
set.seed(1)
train = sample(1:nrow(x),nrow(x)/2)
test =(-train)
pcr.fit=pcr(Salary~.,data=Hitters,subset = train, scale =TRUE,validation = "CV")
validationplot(pcr.fit,val.type="MSEP")
#the best cross-validation error is at M=7
pcr.pred=predict(pcr.fit,x[test,],ncomp=7)
#calculate the test MSE
mean((pcr.pred-y[test])^2)
#fit pcr on the full data set with 7 components
pcr.fit=pcr(y~x,scale=TRUE,ncomp=7)
summary(pcr.fit)
#Now for PLS
set.seed(1)
pls.fit=plsr(Salary~.,data=Hitters,subset=train,scale=TRUE,validation="CV")
summary(pls.fit)
validationplot(pls.fit,val.type="MSEP")
#so we want M=2. Now calculate the test MSE
pls.pred=predict(pls.fit,x[test,],ncomp=2)
mean((pls.pred-y[test])^2)
#now use pls with the whole data set
pls.fit=plsr(Salary~.,data=Hitters,scale=TRUE,ncomp=2)
summary(pls.fit)

#####################
#### SECTION 3 ####
#####################
house=read.csv("kc_house_data.csv",header = TRUE)
#house=read.csv("https://raw.githubusercontent.com/Jonasyao/Machine-Learning-SpecializationUniversity-of-Washington-/master/Regression/Assignment_four/kc_house_data.csv")
house<-as.data.frame(na.omit(house))
house<-house[,3:21]
#perhaps don't scale price
house<-scale(house,center = TRUE, scale = TRUE)
house<-as.data.frame(house)
n<-nrow(house)
set.seed(1)

train_ind <- sample(seq_len(nrow(house)), size = 0.8*n)
train <- house[train_ind, ]
test <- house[-train_ind, ]

#create a linear regression model with the training set
lm.fit<-lm(price~.,data=train)
summary(lm.fit)
alias(lm.fit)
#from the results of alias() we can see that there is multicollinearity
#remove sqft_living
house<-house[,-4]
train<-train[,-4]
lm.fit<-lm(price~.,data=train)
sm<-summary(lm.fit)
#now the error is gone, so we will calculate the test MSE
lm.pred<-predict(lm.fit,test)
testMSE<-mean((lm.pred-test$price)^2)
#create a PCR model
set.seed(1)
pcr.fit=pcr(price~.,data=train, scale=FALSE,validation = "CV")
par(mfrow=c(1,1))
validationplot(pcr.fit,val.type="MSEP")
#I will choose to use 11 components 
pcr.pred=predict(pcr.fit,test,ncomp=11)
#calculate the test MSE
testMSE2<-mean((pcr.pred-test$price)^2)

#####################
#### SECTION 4 ####
#####################
set.seed(1)
#create a pls model
pls.fit=plsr(price~.,data=train,scale=FALSE,validation="CV")
summary(pls.fit)
#plot the cv scores
validationplot(pls.fit,val.type="MSEP")
#I chose to use 5 components
#compute the test MSE
pls.pred=predict(pls.fit,test,ncomp=5)
testMSE3<-mean((pls.pred-test$price)^2)
#The linear, PCR, and PLS models had similar test MSE's
#The linear model had the lowest MSE at 0.287, while PCR had the highest at 0.325
#In this case, the linear model may be the best choice since it had the lowest MSE


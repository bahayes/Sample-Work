#Machine Learning Project I Script
#Brittany Hayes, Zachary Fallon, Aaron Fernandez, Hillary Yarosh
rm(list=ls())
require(boot)
#install.packages("knncat")
require(knncat)
#Read in and prep the data for analysis
data <- read.csv("UCI_Credit_Card.csv")
names(data[13:24])
data_final <- cbind(data[,1:12],data$default.payment.next.month)
data_final <- cbind(data_final, data$BILL_AMT1 - data$PAY_AMT1, data$BILL_AMT2 - data$PAY_AMT2, 
                    data$BILL_AMT3 - data$PAY_AMT3, data$BILL_AMT4 - data$PAY_AMT4,
                    data$BILL_AMT5 - data$PAY_AMT5, data$BILL_AMT6 - data$PAY_AMT6)
data_final <- cbind(data_final[-13],data$default.payment.next.month)
colnames(data_final)[13:19] = c("bal_0","bal_2","bal_3","bal_4","bal_5","bal_6","default")
colnames(data_final) = tolower(colnames(data_final))
colnames(data_final)
data_final <- data_final[-1]
data_final$sex <- as.factor(data_final$sex)
data_final$education <- as.factor(data_final$education)
data_final$marriage <- as.factor(data_final$marriage)
data_final$pay_0 <- as.factor(data_final$pay_0)
data_final$pay_2 <- as.factor(data_final$pay_2)
data_final$pay_3 <- as.factor(data_final$pay_3)
data_final$pay_4 <- as.factor(data_final$pay_4)
data_final$pay_5 <- as.factor(data_final$pay_5)
data_final$pay_6 <- as.factor(data_final$pay_6)
data_final$default <- as.factor(data_final$default)

data_final$education[data_final$education==6]=5
data_final$education[data_final$education==0]=5
data_final$education=factor(data_final$education)

str(data_final)


###########################
### Logistic Regression ###
###########################
#check correlations between numeric predictors
require(corrplot)
cor(data_final[,c(1,12:17)])
corrplot(cor(data_final[,c(1,12:17)]))
#the balance predictors are highly correlated

#Fitting a few different models
glm.fit1<-glm(default~limit_bal,data=data_final,family=binomial)
glm.fit2<- glm(default~sex, data=data_final,family=binomial)
glm.fit3<- glm(default~education, data=data_final,family=binomial)
glm.fit4<- glm(default~marriage, data=data_final,family=binomial)
glm.fit5<- glm(default~limit_bal+age, data=data_final,family=binomial)
glm.fit6<-glm(default~limit_bal+sex, data=data_final, family=binomial)
glm.fit7<-glm(default~limit_bal+I(limit_bal^2), data=data_final, family=binomial)
glm.fit8<-glm(default~limit_bal+I(limit_bal^2)+I(limit_bal^3), data=data_final, family=binomial)
glm.fit9<-glm(default~limit_bal*age,data=data_final, family=binomial)
glm.fit10<-glm(default~limit_bal*sex,data=data_final, family=binomial)
glm.fit11<-glm(default~age+I(age^2),data=data_final, family=binomial)
glm.fit12<-glm(default~age+I(age^2)+I(age^3),data=data_final, family=binomial)
glm.fit13<-glm(default~limit_bal*bal_0,data=data_final, family=binomial)
glm.fit14<-glm(default~limit_bal+age+bal_0,data=data_final, family=binomial)
glm.fit15<-glm(default~limit_bal+age+pay_0+bal_0, data=data_final, family= binomial)
glm.fit16<-glm(default~age+poly(limit_bal,2)+pay_0+bal_0, data=data_final, family= binomial)
glm.fit17<-glm(default~poly(limit_bal,3)+marriage+sex+education+pay_0+pay_4+pay_3,data=data_final, family=binomial)

#Test Error Rate (Attained via Cross Validation)
set.seed(1234)
cv.error<-c()
cv.error[1]=cv.glm(data_final,glm.fit1,K=10)$delta[1]
cv.error[2]=cv.glm(data_final,glm.fit2,K=10)$delta[1]
cv.error[3]=cv.glm(data_final,glm.fit3,K=10)$delta[1]
cv.error[4]=cv.glm(data_final,glm.fit4,K=10)$delta[1]
cv.error[5]=cv.glm(data_final,glm.fit5,K=10)$delta[1]
cv.error[6]=cv.glm(data_final,glm.fit6,K=10)$delta[1]
cv.error[7]=cv.glm(data_final,glm.fit7,K=10)$delta[1]
cv.error[8]=cv.glm(data_final,glm.fit8,K=10)$delta[1]
cv.error[9]=cv.glm(data_final,glm.fit9,K=10)$delta[1]
cv.error[10]=cv.glm(data_final,glm.fit10,K=10)$delta[1]
cv.error[11]=cv.glm(data_final,glm.fit11,K=10)$delta[1]
cv.error[12]=cv.glm(data_final,glm.fit12,K=10)$delta[1]
cv.error[13]=cv.glm(data_final,glm.fit13,K=10)$delta[1]
cv.error[14]=cv.glm(data_final,glm.fit14,K=10)$delta[1]
cv.error[15]=cv.glm(data_final,glm.fit15,K=10)$delta[1]
cv.error[16]=cv.glm(data_final,glm.fit16,K=10)$delta[1]
cv.error[17]=cv.glm(data_final,glm.fit17,K=10)$delta[1]
cv.error 

plot(cv.error,type="b",xlab='Model',ylab='10-Fold Cross Validation Error',main='k-Fold Cross Validation ')

#SETTING UP THE CONFUSION MATRIX....FOR THE BEST MODEL BY CROSS VALIDATION
#We find that by changing the threshold we can reduce our type II error
glm.probs<-predict(glm.fit17,type="response")  
glm.pred<-rep(0, nrow(data_final))
glm.pred[glm.probs>.5]<-1

glm.pred1<-rep(0, nrow(data_final))
glm.pred1[glm.probs>.3]<-1

glm.pred2<-rep(0, nrow(data_final))
glm.pred2[glm.probs>.1]<-1
#for consistency, we will always put the actuals on the left and the predictions on the top of the table
mytable<-table(data_final$default,glm.pred)
mytable

mytable1<-table(data_final$default,glm.pred1)
mytable1

mytable2<-table(data_final$default,glm.pred2)
mytable2

#PERFORMANCE STATISTICS for first threshold
#Overall fraction of correct predictions
(mytable[1,1]+mytable[2,2])/sum(mytable)
#Overall error rate/training error rate
(mytable[2,1]+mytable[1,2])/sum(mytable)
#Type I Error Rate
(mytable[1,2]/sum(mytable[1,]))
#Type II Error Rate
(mytable[2,1]/sum(mytable[2,]))
#Power of the Model
1-(mytable[2,1]/sum(mytable[2,]))
#Precision of the Model
mytable[2,2]/sum(mytable[,2])

#PERFORMANCE STATISTICS for second threshold
#Overall fraction of correct predictions
(mytable1[1,1]+mytable1[2,2])/sum(mytable1)
#Overall error rate/training error rate
(mytable1[2,1]+mytable[1,2])/sum(mytable1)
#Type I Error Rate
(mytable1[1,2]/sum(mytable1[1,]))
#Type II Error Rate
(mytable1[2,1]/sum(mytable1[2,]))
#Power of the Model
1-(mytable1[2,1]/sum(mytable1[2,]))
#Precision of the Model
mytable1[2,2]/sum(mytable1[,2])

#PERFORMANCE STATISTICS for third threshold
#Overall fraction of correct predictions
(mytable2[1,1]+mytable2[2,2])/sum(mytable2)
#Overall error rate/training error rate
(mytable2[2,1]+mytable2[1,2])/sum(mytable2)
#Type I Error Rate
(mytable2[1,2]/sum(mytable2[1,]))
#Type II Error Rate
(mytable2[2,1]/sum(mytable2[2,]))
#Power of the Model
1-(mytable2[2,1]/sum(mytable2[2,]))
#Precision of the Model
mytable2[2,2]/sum(mytable2[,2])

#We can reduce the type II error by quite a bit but 
#at a steep cost

###########################
########## KNN ############
###########################

#Knn with categorical predictors
set.seed(5067)
data_cat <- cbind(data_final[2:4],data_final[6:11],data_final$default)
colnames(data_cat)[10] = "default"
n=nrow(data_cat)
trainprop.cat<-0.90  
trainindices.cat <- sample(1:n, trainprop.cat*n)
trset.cat<-data_cat[trainindices.cat,]
teset.cat<-data_cat[-trainindices.cat,]

#10 fold cross validation to compare k= (1,3,5,7,9) and picks the best one
#This model takes about 5 minutes to run since it is doing so much
knn.cat.full <- knncat(trset.cat, teset.cat, k=c(13,15,17), classcol = 10, xvals=10)
#Misclassification rate
knn.cat.full
#Confusion Matrix
#Columns are the correct classification and rows are the estimates
knn.cat.full$misclass.mat
knn.cat.full$vars
knn.cat.full$best.k
#Interestingly this identifies pay_0 and pay_4 and pay_6 as the most predictive attributes. It does
#not use any other predictors for the best model

mytable<-knn.cat.full$misclass.mat
#PERFORMANCE STATISTICS for third threshold
#Overall fraction of correct predictions
(mytable[1,1]+mytable[2,2])/sum(mytable)
#Overall error rate/training error rate
(mytable[2,1]+mytable[1,2])/sum(mytable)
#Type I Error Rate
(mytable[2,1]/sum(mytable[2,]))
#Type II Error Rate
(mytable[1,2]/sum(mytable[1,]))
#Power of the Model
1-(mytable[1,2]/sum(mytable[1,]))
#Precision of the Model
mytable[2,2]/sum(mytable[,2])


#Knn regression with numeric predictors
data_finalrow2<- (cbind(data_final[1],data_final[12:18]))
colnames(data_finalrow2)[8]='default'
trainindices<-sample(1:nrow(data_finalrow2),.8*nrow(data_finalrow2))
trset<-data_finalrow2[trainindices,]
teset<-data_finalrow2[-trainindices,]
y.name='default'
train.xvals<-scale(trset[setdiff(names(trset),y.name)])
test.xvals<-scale(teset[setdiff(names(teset),y.name)])
train.yvals<-trset$default
test.yvals<-teset$default
library(class)
require(class)

testerror<-numeric(0)
trainerror<-numeric(0)
biggestk=19
kset<-seq(1,biggestk,by=2) 
#This loop takes some time to run
for(k in kset) {
  knn.class.model<-knn(train.xvals,test.xvals,train.yvals,k=k)
  testerror[k%/%2 +1]<-mean(test.yvals!=knn.class.model)
  knn.class.model<-knn(train.xvals,train.xvals,train.yvals,k=k)
  trainerror[k%/%2 +1]<-mean(train.yvals!=knn.class.model)
}
#windows()
par(mfrow=c(2,1))
plot(kset,testerror,type='n',xlim=c(biggestk,1),xlab='Increasing Flexibility (Decreasing k)',ylab='Test Error',main='Test Error as a function of Flexibility for KNN Prediction')
lines(seq(biggestk,1,by=-2),testerror[order(length(testerror):1)],type='b')
plot(kset,trainerror,type='n',xlim=c(biggestk,1),xlab='Increasing Flexibility (Decreasing k)',ylab='Training Error',main='Training Error as a function of Flexibility for KNN Prediction')
lines(seq(biggestk,1,by=-2),trainerror[order(length(trainerror):1)],type='b')
par(mfrow=c(1,1))
print(paste("Best test k is",kset[which.min(testerror)],"with a test error of",testerror[which.min(testerror)]))
print(paste("Best training k is",kset[which.min(trainerror)],"with training error of",trainerror[which.min(trainerror)]))
detach(package:class)

###########################
####### LDA & QDA #########
###########################
set.seed(1234)
library(MASS) 
n<-nrow(data_final)
train<-sample(n,.8*n)
test<-data_final[-train,]
testresult<-data_final$default[-train]
ldaerrors<-c()
#LDA
lda.fit1<-lda(default~limit_bal, data=data_final, family= binomial, subset = train)
lda.pred<-predict(lda.fit1,test)
mytesttable<-table(testresult,lda.pred$class)
#Overall error rate/training error rate
ldaerrors[1]<-(mytesttable[2,1]+mytesttable[1,2])/sum(mytesttable)

lda.fit2<-lda(default~sex, data=data_final, family= binomial, subset = train)
lda.pred<-predict(lda.fit2,test)
mytesttable<-table(testresult,lda.pred$class)
#Overall error rate/training error rate
ldaerrors[2]<-(mytesttable[2,1]+mytesttable[1,2])/sum(mytesttable)

lda.fit3<-lda(default~limit_bal+age, data=data_final, family= binomial, subset = train)
lda.pred<-predict(lda.fit3,test)
mytesttable<-table(testresult,lda.pred$class)
#Overall error rate/training error rate
ldaerrors[3]<-(mytesttable[2,1]+mytesttable[1,2])/sum(mytesttable)

lda.fit4<-lda(default~limit_bal+I(limit_bal^2), data=data_final, family= binomial, subset = train)
lda.pred<-predict(lda.fit4,test)
mytesttable<-table(testresult,lda.pred$class)
#Overall error rate/training error rate
ldaerrors[4]<-(mytesttable[2,1]+mytesttable[1,2])/sum(mytesttable)

lda.fit5<-lda(default~limit_bal+age+pay_0+bal_0, data=data_final, family= binomial, subset = train)
lda.pred<-predict(lda.fit5,test)
mytesttable<-table(testresult,lda.pred$class)
mytesttable
#Overall error rate/training error rate
ldaerrors[5]<-(mytesttable[2,1]+mytesttable[1,2])/sum(mytesttable)


lda.fit6<-lda(default~age+poly(limit_bal,2)+pay_0+bal_0, data=data_final, family= binomial, subset = train)
lda.pred<-predict(lda.fit6,test)
mytesttable<-table(testresult,lda.pred$class)
#Overall error rate/training error rate
ldaerrors[6]<-(mytesttable[2,1]+mytesttable[1,2])/sum(mytesttable)

lda.fit7<-lda(default~poly(limit_bal,3)+marriage+sex+education+pay_0+pay_4+pay_3, data=data_final, family= binomial, subset = train)
lda.pred<-predict(lda.fit7,test)
mytesttable<-table(testresult,lda.pred$class)
mytesttable
#Overall error rate/training error rate
ldaerrors[7]<-(mytesttable[2,1]+mytesttable[1,2])/sum(mytesttable)
#calculate all of the performance statistics for model 7, which had the lowest overall error rate
#Overall fraction of correct predictions
(mytesttable[1,1]+mytesttable[2,2])/sum(mytesttable)
#Overall error rate/training error rate
(mytesttable[2,1]+mytesttable[1,2])/sum(mytesttable)
#Type I Error Rate
(mytesttable[1,2]/sum(mytesttable[1,]))
#Type II Error Rate
(mytesttable[2,1]/sum(mytesttable[2,]))
#Power of the Model
1-(mytesttable[2,1]/sum(mytesttable[2,]))
#Precision of the Model
mytesttable[2,2]/sum(mytesttable[,2])

#QDA
qdaerrors<-c()
qda.fit1<-qda(default~limit_bal, data=data_final, family= binomial, subset = train)
qda.pred<-predict(qda.fit1,test)
mytesttable<-table(testresult,qda.pred$class)
qdaerrors[1]<-(mytesttable[2,1]+mytesttable[1,2])/sum(mytesttable)

qda.fit2<-qda(default~sex, data=data_final, family= binomial, subset = train)
qda.pred<-predict(qda.fit2,test)
mytesttable<-table(testresult,qda.pred$class)
qdaerrors[2]<-(mytesttable[2,1]+mytesttable[1,2])/sum(mytesttable)

qda.fit3<-qda(default~limit_bal+age, data=data_final, family= binomial, subset = train)
qda.pred<-predict(qda.fit3,test)
mytesttable<-table(testresult,qda.pred$class)
qdaerrors[3]<-(mytesttable[2,1]+mytesttable[1,2])/sum(mytesttable)

qda.fit4<-qda(default~limit_bal+I(limit_bal^2), data=data_final, family= binomial, subset = train)
qda.pred<-predict(qda.fit4,test)
mytesttable<-table(testresult,qda.pred$class)
qdaerrors[4]<-(mytesttable[2,1]+mytesttable[1,2])/sum(mytesttable)

qda.fit5<-qda(default~limit_bal+age+pay_0+bal_0, data=data_final, family= binomial, subset = train)
qda.pred<-predict(qda.fit5,test)
mytesttable<-table(testresult,qda.pred$class)
qdaerrors[5]<-(mytesttable[2,1]+mytesttable[1,2])/sum(mytesttable)

qda.fit6<-qda(default~age+poly(limit_bal,2)+pay_0+bal_0, data=data_final, family= binomial, subset = train)
qda.pred<-predict(qda.fit6,test)
mytesttable<-table(testresult,qda.pred$class)
qdaerrors[6]<-(mytesttable[2,1]+mytesttable[1,2])/sum(mytesttable)

#calculate all of the performance statistics for model 5, which had the lowest overall error rate
qda.pred<-predict(qda.fit5,test)
mytesttable<-table(testresult,qda.pred$class)
mytesttable
#Overall fraction of correct predictions
(mytesttable[1,1]+mytesttable[2,2])/sum(mytesttable)
#Overall error rate/training error rate
(mytesttable[2,1]+mytesttable[1,2])/sum(mytesttable)
#Type I Error Rate
(mytesttable[1,2]/sum(mytesttable[1,]))
#Type II Error Rate
(mytesttable[2,1]/sum(mytesttable[2,]))
#Power of the Model
1-(mytesttable[2,1]/sum(mytesttable[2,]))
#Precision of the Model
mytesttable[2,2]/sum(mytesttable[,2])




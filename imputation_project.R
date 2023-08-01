# 6 different statistical imputationa methods (Hotdeck, MIIC, Stochastic regression, Complete case analysis, Regression)
# applying to Boston House Price data

download.packages("DMwR")
library(DMwR)


test<-read.csv("C:/missingdata1/test.csv")
train<-read.csv("C:/missingdata1/train.csv")

#ptratio- when continous variable
train3<-train
model3<-lm(MEDV~.,data=train3)
summary(model3)




#prtatio in2<-train
train2[,6]<-as.factor(train[,6])
str(train2)   #linear regressionel<-lm(MEDV~.,data=train2) 
summary(model) ??t2<-test
test2[,6]<-as.factor(test2[,6])
test_predict<-predict(model, test2) 








#hotdeck simple within cells
hd<-read.csv("C:/missingdata1/hotdeck_sim.csv")
hd<-hd[,-c(10,11,12)]
model.hd<-lm(MEDV~.,data=hd)
summary(model.hd)
test_predict<-predict(model.hd, test)
eval1<-regr.eval(test$MEDV, test_predict)


#hotdeck with imputation cells
hdc<-read.csv("C:/missingdata1/hotdeck_imp.csv")
hdc<-hdc[,-c(10,11,12)]
model.hdc<-lm(MEDV~.,data=hdc)
summary(model.hdc)
test_predict<-predict(model.hdc, test)
eval2<-regr.eval(test$MEDV, test_predict)

#miic
mi<-read.csv("C:/missingdata1/miic.csv")
mi<-mi[,-c(10,11,12)]
model.mi<-lm(MEDV~.,data=mi)
summary(model.mi)
test_predict<-predict(model.mi, test)
eval3<-regr.eval(test$MEDV, test_predict)


#stochastic regression imputation
sr<-read.csv("C:/missingdata1/sri.csv")
sr<-sr[,-c(10,11,12)]
model.sr<-lm(MEDV~.,data=sr)
summary(model.sr)
test_predict<-predict(model.sr, test)
eval4<-regr.eval(test$MEDV, test_predict)


#complete case analysis
train5<-train
train5<-train5[!(is.na(missing$RM)), ]
model.cc<-lm(MEDV~.,data=train5)
summary(model.cc)
test_predict<-predict(model.cc, test)
eval5<-regr.eval(test$MEDV, test_predict)



#missing data imputation with regression
missing<-read.csv("C:/missingdata1/missing.csv")
train4<-train4[is.na(missing$RM), -c(2,5,8)]
model.rm<-lm(MEDV~.,data=train4)
summary(model.rm)
test_predict<-predict(model.rm, test)
eval6<-regr.eval(test$MEDV, test_predict)


ls(pattern = "eval")
list_eval <- mget(ls(pattern = "eval"))
list_eval 

set.seed(1)
install.packages("caret")
install.packages("NbClust")



#split 7:3 in test/train set 
#data<-read.csv(file="C:/data/boston.csv",header = T)
library(caret)
inTrain <- createDataPartition(y=rawdata$MEDV, p=0.7, list=FALSE)
training <- rawdata[inTrain,]
testing <- rawdata[-inTrain,]
write.table(testing,"C:/data/testset_full.csv", sep=",", row.names = FALSE)
write.table(training,"C:/data/trainset_full.csv", sep=",", row.names = FALSE)



testset<-testing[,c(5,8,14,13,9,11)]
write.table(testset,"C:/data/testset.csv", sep=",", row.names = FALSE)

#NOX, DIS, MEDV <- training[,c(5,8,14)]
head(data1)



#ui generating-rnorm(356, mean=0, sd=1)
u<-z3 + data1$NOX-mean(data1$NOX) + data1$DIS-mean(data1$DIS) + data1$MEDV-mean(data1$MEDV)
data1$LSTAT<-training[,13]
data1$RAD<-training[,13]



#MAR (Missing at Random) imputation 
for (i in 1:356){
  if (u[i]<0) {data
    1$LSTAT[i]<-NA 
  }else
  {
    data1$LSTAT[i] = data1$LSTAT[i]
  }i=  +1
}

#data1$<-training[,13]





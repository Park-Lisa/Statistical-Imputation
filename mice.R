setwd("C:/data")
data1<-read.table(file="boston_nan.txt", header= TRUE, sep=",")

library(mice)
(imp.mice = mice(data1, m=5)) 

fit.MDEV <- with(imp.mice, lm(MEDV ~ RM+ LSTAT+ RAD+ CRIM+ PTRATIO+ DIS+ ZN+ NOX+ CHAS+ B+ TAX+ INDUS+ AGE))
pool(fit.MDEV)
summary(pool(fit.MDEV))
install.packages("mi")
library(mi)
install.packages("betareg")
library(betareg)

setwd("C:/data")
data1<-read.table(file="boston_nan.txt", header= TRUE, sep=",")

data0<-read.table(file="boston_transform.txt", header= TRUE, sep=",")


imp.mi <- mi(data1)
round(mipply(imp.mi, mean, to.matrix = TRUE), 3)
lm.mi.out <- lm.mi(MEDV ~ RM+ LSTAT+ RAD+ CRIM+ PTRATIO+ DIS+ ZN+ NOX+ CHAS+ B+ TAX+ INDUS+ AGE, imp.mi)
Rhats(imp.mi)
imputations <- mi(imp.mi, n.iter = 5)
plot(imputations)
hist(imputations)

image(imputations)
summary(imputations)

imp.mi.out <-complete(imputations)
mi.out<-imp.mi.out[[4]]

plot(x=data0$LSTAT,y=mi.out$LSTAT)
plot(x=data0$RAD,y=mi.out$RAD)
plot(x=data0$CRIM,y=mi.out$CRIM)
plot(x=data0$PTRATIO,y=mi.out$PTRATIO)
plot(x=data0$RM,y=mi.out$RM)



write.csv(mi.out, "C:/data/miimputed.csv", sep = ",", row.names = FALSE, quote = FALSE, append = TRUE, na = "NA") 

write.table(mi.out, "C:/data/miimputed.txt", sep = ",", row.names = FALSE, quote = FALSE, append = TRUE, na = "NA") 










################################
imputations <- mi(data1, n.iter = 30, n.chains = 4, max.minutes = 20)
round(mipply(imputations, mean, to.matrix = TRUE), 3)

analysis <- pool(MEDV ~ RM+ LSTAT+ RAD+ CRIM+ PTRATIO+ DIS+ ZN+ NOX+ CHAS+ B+ TAX+ INDUS+ AGE,data = imputations, m = 5)
display(analysis)
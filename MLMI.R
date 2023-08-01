install.packages("mlmi")
library(mlmi)



### EM by MLMI 
mlmi.em<-mixImp(boston_nan_1, nCat, M = 5, pd = FALSE, marginsType = 1, margins = NULL, designType = 1, design = NULL, steps = 100)
### DA by MLMI
mlmi.da<-mixImp(boston_nan_1, nCat, M = 5, pd = TRUE, marginsType = 1, margins = NULL, designType = 1, design = NULL, steps = 100)


## Data Simulation
getwd()
setwd("C:/datafile/nan")
dir = "C:/datafile/nan"
src_file = list.files(dir)
src_file_cnt = length(src_file)
dataset=data.frame()
for(i in 1:src_file_cnt){
  data = read.csv(paste(dir, "/", src_file[i], sep=""), header=TRUE, sep=",")
  group=c(rep(i,nrow(data)))
  print(group)
  data_tmp=cbind(data,group)
  dataset=rbind(dataset,data_tmp)
}
dataset

#ITERATION
mean_em=data.frame()
var_em=data.frame()
mean_da=data.frame()
var_da=data.frame()



for(i in 1:src_file_cnt){
  data_tmp=subset(dataset, group==i)

  #group_idx ¹Ù²ãÁÖ±â
  group_idx=i
  
  ###Data Augmentation
  data_da_miss =data_tmp
  data_numeric=as.data.frame(lapply(data_da_miss, as.numeric))
  rngseed(25672)
  #getparam.norm(s, theta) #Print out those results
  impdata<-mixImp(data_numeric, nCat=0, M = 5, pd = TRUE, marginsType = 1, margins = NULL, designType = 1, design = NULL, steps = 100,rseed=NULL)
  data_da = data.frame(impdata)
  
  #Mean
  mean_tmp=data_da %>% summarise_if(is.numeric, mean)
  mean_temp=cbind(mean_tmp,group_idx)
  mean_da =rbind(mean_da,mean_temp)
  
  #Var
  var_tmp=data_da %>% summarise_if(is.numeric, var)
  var_temp = cbind(var_tmp,group_idx)
  var_da=rbind(var_da,var_temp)
  
  ###EM
  data_em_miss =data_tmp
  data_numeric=as.data.frame(lapply(data_em_miss, as.numeric))
  #getparam.norm(s, theta) #Print out those results
  impdata<-mixImp(data_numeric, nCat=0, M = 5, pd = FALSE, marginsType = 1, margins = NULL, designType = 1, design = NULL, steps = 100, rseed=NULL)
  data_em = data.frame(impdata)
  
  #Mean
  mean_tmp=data_em %>% summarise_if(is.numeric, mean)
  mean_temp=cbind(mean_tmp,group_idx)
  mean_em =rbind(mean_em,mean_temp)
  
  #Var
  var_tmp=data_em %>% summarise_if(is.numeric, var)
  var_temp = cbind(var_tmp,group_idx)
  var_em=rbind(var_em,var_temp)
  
  
  

}



write.csv(mean_da, file="mean_mlmi_da.csv", row.names = F)
write.csv(var_da, file="var_mimli_da.csv", row.names = F)
write.csv(mean_em, file="mean_mlmi_em.csv", row.names = F)
write.csv(var_em, file="var_mlmi_em.csv", row.names = F)



write.csv(colMeans(mean_da), file="mean_mlmi_da final.csv", row.names = TRUE)
write.csv(colMeans(var_da), file="var_mlmi_da final.csv", row.names = TRUE)
write.csv(colMeans(mean_em), file="mean_mlmi_em final.csv", row.names = TRUE)
write.csv(colMeans(var_em), file="var_mlmi_em final.csv", row.names = TRUE)







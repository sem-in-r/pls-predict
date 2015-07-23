
#Clear environment
rm(list=ls())

#Create the baseline for the values
base<-c(1,2,3,4,5,6,7)

#Set seed 
set.seed(123)

#Generate values for first construct
i11<-sample(base,183,replace=TRUE)
i12<-sample(base,183,replace=TRUE)
i13<-sample(base,183,replace=TRUE)
i14<-sample(base,183,replace=TRUE)

#Generate values for second construct
i21<-sample(base,183,replace=TRUE)
i22<-sample(base,183,replace=TRUE)
i23<-sample(base,183,replace=TRUE)
i24<-sample(base,183,replace=TRUE)

#Generate containers for third construct
i31<-rep(0,183)
i32<-rep(0,183)
i33<-rep(0,183)
i34<-rep(0,183)

#Combine as data matrix
data=cbind(i11,i12,i13,i14,i21,i22,i23,i24,i31,i32,i33,i34)

w1<-c(0.7,0.5,0.6,0.8)
w2<-c(0.3,0.5,0.9,0.4)

l1<-data[,c("i11","i12","i13","i14")]%*%w1

l2<-data[,c("i21","i22","i23","i24")]%*%w2

l3<-l1*0.3+l2*0.7

w3<-c(0.1,0.3,0.8,0.4)

o3<-l3%*%w3

data[,"i31"]<-o3[,1]
data[,"i32"]<-o3[,2]
data[,"i33"]<-o3[,3]
data[,"i34"]<-o3[,4]

write.csv(data[1:100,], "SimulationJM.csv")

write.csv(mobi[1:100,], "mobi100.csv")


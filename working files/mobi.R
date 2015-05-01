#-----------------------------------------semPLS--------------------------------

#Load semPLS package for Data
library("semPLS")

#Load data
data("mobi")

#Filter data 
data<-mobi[1:250,]

#Load Structural and Measurement Models
data("ECSIsm")
data("ECSImm")

#Create Model
ECSI <- plsm(data = data, strucmod = ECSIsm, measuremod = ECSImm)

#Run Model
ecsi <- sempls(model = ECSI, data = data, wscheme = "factorial")

#Path Coefficients
pathCoeff(ecsi)

#Outter Weights
plsWeights(ecsi)

#Loadings
plsLoadings(ecsi)


#-----------------------------------------simplePLS--------------------------------


#Load our Algorithm
source("./lib/simplePLS.R")

#Transform models to simplePLS
smMatrix<-ECSIsm
mmMatrix<-cbind(ECSImm,rep("R",nrow(ECSImm)))
colnames(mmMatrix)<-c("latent","measurement","type")

#Call PLS-PM Function
plsModel<-simplePLS(data,smMatrix,mmMatrix)

#Path Coefficients
plsModel$path_coef

plsModel$weightDiff

plsModel$outer_weights

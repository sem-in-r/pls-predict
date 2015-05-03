#-----------------------------------------semPLS--------------------------------

#Load semPLS package for Data
library("semPLS")

#Load data
data("mobi")

#Prepare the random sampling
set.seed(123)
index=sample.int(dim(mobi)[1],250,replace=F)
trainData=mobi[index[1:200],]
holdData=mobi[index[201:250],]

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
source("./lib/graphUtils.R")

#Transform models to simplePLS
smMatrix<-ECSIsm
mmMatrix<-cbind(ECSImm,rep("R",nrow(ECSImm)))
colnames(mmMatrix)<-c("latent","measurement","type")

#Call PLS-PM Function
plsModel<-simplePLS(trainData,smMatrix,mmMatrix)

#Call Prediction Function
predHold <- PLSpredict(plsModel,holdData)

#Set the panels
par(mfrow=c(2,2))

#Find scales
xmax<-ceiling(max(c(holdData[,"CUSL1"],
                    holdData[,"CUSL1"],
                    holdData[,"CUSL1"])))

ymax<-ceiling(max(c(predHold$predictedMeasurements[,"CUSL1"],
                    predHold$predictedMeasurements[,"CUSL1"],
                    predHold$predictedMeasurements[,"CUSL1"])))

#PLS: Actual vs Predicted (CUSL1)
y<-predHold$predictedMeasurements[,"CUSL1"]
x<-holdData[,"CUSL1"]
z<-holdData[,"CUSL1"]#nnPredData[,"mv11"]
title="Actual vs Predicted (CUSL1)"
xlabel=paste("Mean: Act=",
             signif(mean(x),digits=4),
             "PLS=",
             signif(mean(y),digits=4),
             "NN=",
             signif(mean(z),digits=4),
             "\n SD: Act=",
             signif(sd(x),digits=4),
             "PLS=",
             signif(sd(y),digits=4),
             "NN=",
             signif(sd(z),digits=4))
ylabel="Predicted"
graphScatterplot(x,y,z,title,xlabel,ylabel,xmax=xmax,ymax=ymax)
points(x,z, col= "black")





rm(list=ls())
data(simdata)
# Calculate plspm
sim_inner = matrix(c(0,0,0,0,0,0,1,1,0), 3, 3, byrow=TRUE)
dimnames(sim_inner) = list(c("Price", "Quality", "Satisfaction"),
                           c("Price", "Quality", "Satisfaction"))
sim_outer = list(c(1,2,3,4,5), c(6,7,8,9,10), c(11,12,13))
sim_mod = c("A", "A", "A") # reflective indicators
sim_global = plspm(simdata,sim_inner,sim_outer, modes=sim_mod)
sim_global
plot (sim_global)
sim_outer
sim_inner
simdata


#Load our Algorithm
source("./lib/simplePLS.R")
source("./lib/graphUtils.R")

#Load library for simulated Data
library("plspm")

#Load Data
data(simdata)

#Prepare the random sampling
set.seed(123)
index=sample.int(dim(simdata)[1],400,replace=F)
trainData=simdata[index[1:300],]
holdData=simdata[index[301:400],]

#Create the Matrix of the Structural Model
smMatrix <- matrix(c("Price", "Satisfaction",
                     "Quality","Satisfaction"),nrow=2,ncol=2,byrow =TRUE,
                   dimnames = list(1:2,c("source","target")))

#Create the Matrix of the Measurement Model
mmMatrix <- matrix(c("Price","mv1","R",
                     "Price","mv2","R",
                     "Price","mv3","R",
                     "Price","mv4","R",
                     "Price","mv5","R",
                     "Quality","mv6","R",
                     "Quality","mv7","R",
                     "Quality","mv8","R",
                     "Quality","mv9","R",
                     "Quality","mv10","R",
                     "Satisfaction","mv11","R",
                     "Satisfaction","mv12","R",
                     "Satisfaction","mv13","R"),nrow=13,ncol=3,byrow =TRUE,
                   dimnames = list(1:13,c("latent","measurement","type")))

#Call PLS-PM Function
plsModel<-simplePLS(trainData,smMatrix,mmMatrix)

#Call Prediction Function
predHold <- PLSpredict(plsModel,holdData)
  

## fitting neural net model
require(nnet)

X=trainData[,c(1:10)]
Y=trainData[,c(11:13)]

# Model with two hidden nodes
net1=nnet(X,Y,size=2,linout =T)
nnPredData=data.frame(predict(net1,holdData))

#Calculate nn prediction residuals
nnPredResiduals<-holdData[,c("mv11","mv12","mv13")]-nnPredData[,c("mv11","mv12","mv13")]

#Set the panels
par(mfrow=c(2,2))

#Find scales
xmax<-ceiling(max(c(holdData[,"mv11"],
                    holdData[,"mv12"],
                    holdData[,"mv13"])))

ymax<-ceiling(max(c(predHold$predictedMeasurements[,"mv11"],
                    predHold$predictedMeasurements[,"mv12"],
                    predHold$predictedMeasurements[,"mv13"])))

#PLS: Actual vs Predicted (mv11)
y<-predHold$predictedMeasurements[,"mv11"]
x<-holdData[,"mv11"]
z<-nnPredData[,"mv11"]
title="Actual vs Predicted (mv11)"
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

#PLS: Actual vs Predicted (mv12)
y<-predHold$predictedMeasurements[,"mv12"]
x<-holdData[,"mv12"]
z<-nnPredData[,"mv12"]
title="Actual vs Predicted (mv12)"
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

#PLS: Actual vs Predicted (mv13)
y<-predHold$predictedMeasurements[,"mv13"]
x<-holdData[,"mv13"]
z<-nnPredData[,"mv13"]
title="Actual vs Predicted (mv13)"
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


## Residuals Histogram PLS vs NN


#Set the panels
par(mfrow=c(2,2))

title<-"PLS vs NN Residuals"

graphCombinedResiduals("mv11",nnPredResiduals,predHold$residuals,title,c(-6,6),c(0,0.5),10,"NN","PLS")

graphCombinedResiduals("mv12",nnPredResiduals,predHold$residuals,title,c(-6,6),c(0,0.5),10,"NN","PLS")

graphCombinedResiduals("mv13",nnPredResiduals,predHold$residuals,title,c(-6,6),c(0,0.5),10,"NN","PLS")


#Clear Environment
rm (list=ls())

#load libraries
source("./lib/simplePLS.R")
source("./lib/graphUtils.R")

#load datafile

data<-read.csv(file = "./data/data_meeting_11_simsem.csv",header = T)

#Prepare the random sampling
set.seed(123)
index=sample.int(dim(data)[1],300,replace=F)
trainData=data[index[1:200],]
holdData=data[index[201:300],]

#Scenario 1: Four Constructs using one mediator

#Create the Matrix of the Structural Model
smMatrix <- matrix(c("Latent Variable 1", "Latent Variable 3",
                     "Latent Variable 2","Latent Variable 3",
                     "Latent Variable 3","Latent Variable 4"),nrow=3,ncol=2,byrow =TRUE,
                   dimnames = list(1:3,c("source","target")))

#Create the Matrix of the Measurement Model
mmMatrix <- matrix(c("Latent Variable 1","x11","R",
                     "Latent Variable 1","x12","R",
                     "Latent Variable 1","x13","R",
                     "Latent Variable 1","x14","R",
                     "Latent Variable 2","x21","R",
                     "Latent Variable 2","x22","R",
                     "Latent Variable 2","x23","R",
                     "Latent Variable 2","x24","R",
                     "Latent Variable 3","x31","R",
                     "Latent Variable 3","x32","R",
                     "Latent Variable 3","x33","R",
                     "Latent Variable 3","x34","R",
                     "Latent Variable 4","y1","R",
                     "Latent Variable 4","y2","R",
                     "Latent Variable 4","y3","R",
                     "Latent Variable 4","y4","R"),nrow=16,ncol=3,byrow =TRUE,
                   dimnames = list(1:16,c("latent","measurement","type")))

#Call PLS-PM Function
plsModel<-simplePLS(trainData,smMatrix,mmMatrix)

#Predicting from the exogenous measurements

#Call Prediction Function
predHold <- PLSpredict(plsModel,holdData)

## fitting neural net model
require(nnet)

X=trainData[,c(2:13)]
Y=trainData[,c(14:17)]

# Model with two hidden nodes
net1=nnet(X,Y,size=2,linout =T)
nnPredData=data.frame(predict(net1,holdData))

#Calculate NN Residuals
nnPredResiduals<-holdData[,c("y1","y2","y3","y4")]-nnPredData[,c("y1","y2","y3","y4")]

#Set the panels
par(mfrow=c(2,2))

#Find scales
xmax<-ceiling(max(c(holdData[,"y1"],
                    holdData[,"y2"],
                    holdData[,"y3"],
                    holdData[,"y4"])))

ymax<-ceiling(max(c(predHold$predictedMeasurements[,"y1"],
                    predHold$predictedMeasurements[,"y2"],
                    predHold$predictedMeasurements[,"y3"],
                    predHold$predictedMeasurements[,"y4"],
                    nnPredData[,"y1"],
                    nnPredData[,"y2"],
                    nnPredData[,"y3"],
                    nnPredData[,"y4"])))

xmin<-floor(min(c(holdData[,"y1"],
                  holdData[,"y2"],
                  holdData[,"y3"],
                  holdData[,"y4"])))

ymin<-floor(min(c(predHold$predictedMeasurements[,"y1"],
                  predHold$predictedMeasurements[,"y2"],
                  predHold$predictedMeasurements[,"y3"],
                  predHold$predictedMeasurements[,"y4"],
                  nnPredData[,"y1"],
                  nnPredData[,"y2"],
                  nnPredData[,"y3"],
                  nnPredData[,"y4"])))

#PLS: Actual vs Predicted (y1)
y<-predHold$predictedMeasurements[,"y1"]
x<-holdData[,"y1"]
z<-nnPredData[,"y1"]
title="Actual vs Predicted (y1)"
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
graphScatterplot(x,y,z,title,xlabel,ylabel,xmax=xmax,ymax=ymax,xmin=xmin,ymin=ymin)
points(x,z, col= "black")

#PLS: Actual vs Predicted (y2)
y<-predHold$predictedMeasurements[,"y2"]
x<-holdData[,"y2"]
z<-nnPredData[,"y2"]
title="Actual vs Predicted (y2)"
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
graphScatterplot(x,y,z,title,xlabel,ylabel,xmax=xmax,ymax=ymax,xmin=xmin,ymin=ymin)
points(x,z, col= "black")

#PLS: Actual vs Predicted (y3)
y<-predHold$predictedMeasurements[,"y3"]
x<-holdData[,"y3"]
z<-nnPredData[,"y3"]
title="Actual vs Predicted (y3)"
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
graphScatterplot(x,y,z,title,xlabel,ylabel,xmax=xmax,ymax=ymax,xmin=xmin,ymin=ymin)
points(x,z, col= "black")

#PLS: Actual vs Predicted (y3)
y<-predHold$predictedMeasurements[,"y4"]
x<-holdData[,"y4"]
z<-nnPredData[,"y4"]
title="Actual vs Predicted (y4)"
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
graphScatterplot(x,y,z,title,xlabel,ylabel,xmax=xmax,ymax=ymax,xmin=xmin,ymin=ymin)
points(x,z, col= "black")

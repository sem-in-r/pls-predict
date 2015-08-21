#Clear Environment
rm (list=ls())

#load libraries
source("./lib/simplePLS.R")
source("./lib/graphUtils.R")

#load datafile

data<-read.csv(file = "./data/data_meeting_11_jm.csv",header = T)

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

#Get results from model
smMatrix <- plsModel$smMatrix
mmMatrix <- plsModel$mmMatrix
ltVariables <- plsModel$ltVariables
mmVariables <- plsModel$mmVariables
outer_weights <- plsModel$outer_weights
outer_loadings <- plsModel$outer_loadings
meanData<-plsModel$meanData
sdData <- plsModel$sdData
path_coef<-plsModel$path_coef


# Subscenario 1: Predicting from the most exogenous variables

pMeasurements<-c("x11","x12","x13","x14","x21","x22","x23","x24")

#Extract Measurements needed for Predictions
normData <- holdData[,pMeasurements]

#Normalize data
for (i in pMeasurements)
{
  normData[,i] <-(holdData[,i] - meanData[i])/sdData[i]
}  

#Convert dataset to matrix
normData<-data.matrix(normData)

#Estimate Factor Scores from Outter Path
fscores <- normData%*%outer_weights[pMeasurements,c("Latent Variable 1","Latent Variable 2")]

fscores3<-fscores%*%path_coef[c("Latent Variable 1","Latent Variable 2"),c("Latent Variable 3")]

fscores4<-fscores3%*%path_coef[c("Latent Variable 3"),c("Latent Variable 4")]

#Predict Measurements with loadings
predictedMeasurements<-fscores4%*% t(outer_loadings[13:16,4])

#Denormalize data
for (i in c("y1","y2","y3","y4"))
{
  predictedMeasurements[,i]<-(predictedMeasurements[,i] * sdData[i])+meanData[i]
}  

#Calculating the residuals
residuals <- holdData[,c("y1","y2","y3","y4")] - predictedMeasurements[,c("y1","y2","y3","y4")]

predHold <- list(predictedMeasurements = predictedMeasurements[,c("y1","y2","y3","y4")],
                 residuals = residuals)

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
                    predHold$predictedMeasurements[,"y4"])))

xmin<-floor(min(c(holdData[,"y1"],
                  holdData[,"y2"],
                  holdData[,"y3"],
                  holdData[,"y4"])))

ymin<-floor(min(c(predHold$predictedMeasurements[,"y1"],
                  predHold$predictedMeasurements[,"y2"],
                  predHold$predictedMeasurements[,"y3"],
                  predHold$predictedMeasurements[,"y4"])))

#PLS: Actual vs Predicted (y1)
y<-predHold$predictedMeasurements[,"y1"]
x<-holdData[,"y1"]
z<-holdData[,"y1"]
title="Actual vs Predicted (y1)"
xlabel=paste("Mean: Act=",
             signif(mean(x),digits=4),
             "PLS=",
             signif(mean(y),digits=4),
             "\n SD: Act=",
             signif(sd(x),digits=4),
             "PLS=",
             signif(sd(y),digits=4))
ylabel="Predicted"
graphScatterplot(x,y,z,title,xlabel,ylabel,xmax=xmax,ymax=ymax,xmin=xmin,ymin=ymin)

#PLS: Actual vs Predicted (y2)
y<-predHold$predictedMeasurements[,"y2"]
x<-holdData[,"y2"]
z<-holdData[,"y2"]
title="Actual vs Predicted (y2)"
xlabel=paste("Mean: Act=",
             signif(mean(x),digits=4),
             "PLS=",
             signif(mean(y),digits=4),
             "\n SD: Act=",
             signif(sd(x),digits=4),
             "PLS=",
             signif(sd(y),digits=4))
ylabel="Predicted"
graphScatterplot(x,y,z,title,xlabel,ylabel,xmax=xmax,ymax=ymax,xmin=xmin,ymin=ymin)

#PLS: Actual vs Predicted (y3)
y<-predHold$predictedMeasurements[,"y3"]
x<-holdData[,"y3"]
z<-holdData[,"y3"]
title="Actual vs Predicted (y3)"
xlabel=paste("Mean: Act=",
             signif(mean(x),digits=4),
             "PLS=",
             signif(mean(y),digits=4),
             "\n SD: Act=",
             signif(sd(x),digits=4),
             "PLS=",
             signif(sd(y),digits=4))
ylabel="Predicted"
graphScatterplot(x,y,z,title,xlabel,ylabel,xmax=xmax,ymax=ymax,xmin=xmin,ymin=ymin)

#PLS: Actual vs Predicted (y3)
y<-predHold$predictedMeasurements[,"y4"]
x<-holdData[,"y4"]
z<-holdData[,"y4"]
title="Actual vs Predicted (y4)"
xlabel=paste("Mean: Act=",
             signif(mean(x),digits=4),
             "PLS=",
             signif(mean(y),digits=4),
             "\n SD: Act=",
             signif(sd(x),digits=4),
             "PLS=",
             signif(sd(y),digits=4))
ylabel="Predicted"
graphScatterplot(x,y,z,title,xlabel,ylabel,xmax=xmax,ymax=ymax,xmin=xmin,ymin=ymin)

#Set the panels
par(mfrow=c(2,2))

title<-"Prediction Residuals"

xrange<-c(-10,10)
yrange<-c(0,2)

newgraphResiduals("y1",predHold$residuals,title,xrange,yrange)

newgraphResiduals("y2",predHold$residuals,title,xrange,yrange)

newgraphResiduals("y3",predHold$residuals,title,xrange,yrange)

newgraphResiduals("y4",predHold$residuals,title,xrange,yrange)





# Subscenario 2: Predicting from the mediator

pMeasurements<-c("x31","x32","x33","x34")

#Extract Measurements needed for Predictions
normData <- holdData[,pMeasurements]

#Normalize data
for (i in pMeasurements)
{
  normData[,i] <-(holdData[,i] - meanData[i])/sdData[i]
}  

#Convert dataset to matrix
normData<-data.matrix(normData)

#Estimate Factor Scores from Outter Path
fscores <- normData%*%outer_weights[pMeasurements,c("Latent Variable 3")]

fscores4<-fscores%*%path_coef[c("Latent Variable 3"),c("Latent Variable 4")]

#Predict Measurements with loadings
predictedMeasurements<-fscores4%*% t(outer_loadings[13:16,4])

#Denormalize data
for (i in c("y1","y2","y3","y4"))
{
  predictedMeasurements[,i]<-(predictedMeasurements[,i] * sdData[i])+meanData[i]
}  

#Calculating the residuals
residuals <- holdData[,c("y1","y2","y3","y4")] - predictedMeasurements[,c("y1","y2","y3","y4")]

predHold <- list(predictedMeasurements = predictedMeasurements[,c("y1","y2","y3","y4")],
                 residuals = residuals)

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
                    predHold$predictedMeasurements[,"y4"])))

xmin<-floor(min(c(holdData[,"y1"],
                  holdData[,"y2"],
                  holdData[,"y3"],
                  holdData[,"y4"])))

ymin<-floor(min(c(predHold$predictedMeasurements[,"y1"],
                  predHold$predictedMeasurements[,"y2"],
                  predHold$predictedMeasurements[,"y3"],
                  predHold$predictedMeasurements[,"y4"])))

#PLS: Actual vs Predicted (y1)
y<-predHold$predictedMeasurements[,"y1"]
x<-holdData[,"y1"]
z<-holdData[,"y1"]
title="Actual vs Predicted (y1)"
xlabel=paste("Mean: Act=",
             signif(mean(x),digits=4),
             "PLS=",
             signif(mean(y),digits=4),
             "\n SD: Act=",
             signif(sd(x),digits=4),
             "PLS=",
             signif(sd(y),digits=4))
ylabel="Predicted"
graphScatterplot(x,y,z,title,xlabel,ylabel,xmax=xmax,ymax=ymax,xmin=xmin,ymin=ymin)

#PLS: Actual vs Predicted (y2)
y<-predHold$predictedMeasurements[,"y2"]
x<-holdData[,"y2"]
z<-holdData[,"y2"]
title="Actual vs Predicted (y2)"
xlabel=paste("Mean: Act=",
             signif(mean(x),digits=4),
             "PLS=",
             signif(mean(y),digits=4),
             "\n SD: Act=",
             signif(sd(x),digits=4),
             "PLS=",
             signif(sd(y),digits=4))
ylabel="Predicted"
graphScatterplot(x,y,z,title,xlabel,ylabel,xmax=xmax,ymax=ymax,xmin=xmin,ymin=ymin)

#PLS: Actual vs Predicted (y3)
y<-predHold$predictedMeasurements[,"y3"]
x<-holdData[,"y3"]
z<-holdData[,"y3"]
title="Actual vs Predicted (y3)"
xlabel=paste("Mean: Act=",
             signif(mean(x),digits=4),
             "PLS=",
             signif(mean(y),digits=4),
             "\n SD: Act=",
             signif(sd(x),digits=4),
             "PLS=",
             signif(sd(y),digits=4))
ylabel="Predicted"
graphScatterplot(x,y,z,title,xlabel,ylabel,xmax=xmax,ymax=ymax,xmin=xmin,ymin=ymin)

#PLS: Actual vs Predicted (y3)
y<-predHold$predictedMeasurements[,"y4"]
x<-holdData[,"y4"]
z<-holdData[,"y4"]
title="Actual vs Predicted (y4)"
xlabel=paste("Mean: Act=",
             signif(mean(x),digits=4),
             "PLS=",
             signif(mean(y),digits=4),
             "\n SD: Act=",
             signif(sd(x),digits=4),
             "PLS=",
             signif(sd(y),digits=4))
ylabel="Predicted"
graphScatterplot(x,y,z,title,xlabel,ylabel,xmax=xmax,ymax=ymax,xmin=xmin,ymin=ymin)

#Set the panels
par(mfrow=c(2,2))

title<-"Prediction Residuals"

xrange<-c(-4,4)
yrange<-c(0,4)

newgraphResiduals("y1",predHold$residuals,title,xrange,yrange)

newgraphResiduals("y2",predHold$residuals,title,xrange,yrange)

newgraphResiduals("y3",predHold$residuals,title,xrange,yrange)

newgraphResiduals("y4",predHold$residuals,title,xrange,yrange)

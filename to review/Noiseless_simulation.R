
#Clear Environment
rm (list=ls())

#Load library to get random samples of multivariate normal distribution
require(mnormt)

#Seed to the random process
set.seed(123)

#Four Variables Mean 2 SD =2
Sigma <- matrix(c(4,2,2,2,2,4,2,2,2,2,4,2,2,2,2,4),4,4)
mu <- c(2, 2, 2, 2)
x1<-rmnorm(300,mu,Sigma)
w1<-c(1,1,1,1)
fc1<-(x1%*%w1)
fc1<-scale(fc1,center=TRUE,scale=TRUE)

#Four Variables Mean 0, SD =1
Sigma <- matrix(c(1,0.5,0.5,0.5,0.5,1,0.5,0.5,0.5,0.5,1,0.5,0.5,0.5,0.5,1),4,4)
mu <- c(0, 0, 0, 0)
x2<-rmnorm(300,mu,Sigma)
w2<-c(1,1,1,1)
#Create an error term of 1/4 the sd of the variables and add all 
#the values to creat latent variable 1
fc2<-(x2%*%w2)
fc2<-scale(fc2,center=TRUE,scale=TRUE)

fc3<-fc1+fc2
fc3<-scale(fc3,center=TRUE,scale=TRUE)

#produce some weight out of the randomness
w3<-rnorm(4,mean=0.5,sd=(2))

#Create the variables
y3<-fc3%*%w3

#Denormalization
y3[,1] <- y3[,1]*0.5+5
y3[,2] <- y3[,2]*0.25+3
y3[,3] <- y3[,3]*0.75+8
y3[,4] <- y3[,4]*2+2


#Create the data
data<-cbind(x1,x2,y3)
colnames(data)<-c("x11","x12","x13","x14","x21","x22","x23","x24","y1","y2","y3","y4")

head(data)

source("./lib/simplePLS.R")
source("./lib/graphUtils.R")

#Prepare the random sampling
set.seed(123)
index=sample.int(dim(data)[1],300,replace=F)
trainData=data[index[1:200],]
holdData=data[index[201:300],]

smMatrix <- matrix(c("Latent Variable 1", "Latent Variable 3",
                     "Latent Variable 2","Latent Variable 3"),nrow=2,ncol=2,byrow =TRUE,
                   dimnames = list(1:2,c("source","target")))

#Create the Matrix of the Measurement Model
mmMatrix <- matrix(c("Latent Variable 1","x11","R",
                     "Latent Variable 1","x12","R",
                     "Latent Variable 1","x13","R",
                     "Latent Variable 1","x14","R",
                     "Latent Variable 2","x21","R",
                     "Latent Variable 2","x22","R",
                     "Latent Variable 2","x23","R",
                     "Latent Variable 2","x24","R",
                     "Latent Variable 3","y1","R",
                     "Latent Variable 3","y2","R",
                     "Latent Variable 3","y3","R",
                     "Latent Variable 3","y4","R"),nrow=12,ncol=3,byrow =TRUE,
                   dimnames = list(1:12,c("latent","measurement","type")))

#Call PLS-PM Function
plsModel<-simplePLS(trainData,smMatrix,mmMatrix)

#Call Prediction Function
predHold <- PLSpredict(plsModel,holdData)



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

#Standarizing
fscores[,1]<-fscores[,1]/sd(fscores[,1])
fscores[,2]<-fscores[,2]/sd(fscores[,2])

fscores3<-fscores%*%path_coef[c("Latent Variable 1","Latent Variable 2"),c("Latent Variable 3")]

fscores3 [,1]<-fscores3[,1]/sd(fscores3[,1])

#Predict Measurements with loadings
predictedMeasurements<-fscores3%*% t(outer_loadings[9:12,3])

predictedMeasurements[,1]<-predictedMeasurements[,1]*sd(trainData[,"y1"]) + mean(trainData[,"y1"])
predictedMeasurements[,2]<-predictedMeasurements[,2]*sd(trainData[,"y2"]) + mean(trainData[,"y2"])
predictedMeasurements[,3]<-predictedMeasurements[,3]*sd(trainData[,"y3"]) + mean(trainData[,"y3"])
predictedMeasurements[,4]<-predictedMeasurements[,4]*sd(trainData[,"y4"]) + mean(trainData[,"y4"])

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

xrange<-c(-0.2,0.2)
yrange<-c(0,50)

newgraphResiduals("y1",predHold$residuals,title,xrange,yrange)

newgraphResiduals("y2",predHold$residuals,title,xrange,yrange)

newgraphResiduals("y3",predHold$residuals,title,xrange,yrange)

newgraphResiduals("y4",predHold$residuals,title,xrange,yrange)




#Clear Environment
rm(list=ls())

#Creating Simulated Data
source("./thesis/simsem.R")

#Creating Model

#Load our Algorithms
source("./lib/simplePLS.R")
source("./lib/graphUtils.R")

#Seed to the random process
set.seed(123)

#Get the index for the Training and the test data
index=sample.int(dim(semData)[1],100,replace=F)

#Get the training and the test data
trainData=semData[-index,]
testData=semData[index,]

#Create the Matrix of the Structural Model
smMatrix <- matrix(c("LV1", "LV3",
                     "LV2","LV3"),nrow=2,ncol=2,byrow =TRUE,
                     dimnames = list(1:2,c("source","target")))

#Create the Matrix of the Measurement Model
mmMatrix <- matrix(c("LV1","x11","F",
                     "LV1","x12","F",
                     "LV1","x13","F",
                     "LV2","x21","F",
                     "LV2","x22","F",
                     "LV2","x23","F",
                     "LV3","x31","R",
                     "LV3","x32","R",
                     "LV3","x33","R",
                     "LV3","x34","R"),nrow=10,ncol=3,byrow =TRUE,
                   dimnames = list(1:10,c("latent","measurement","type")))

#Call PLS-PM Function
plsModel<-simplePLS(trainData,smMatrix,mmMatrix,9)

#Call Prediction Function
predTest <- PLSpredict(plsModel,testData)

#Set the panels
par(mfrow=c(2,2))

#Find scales
xmax<-ceiling(max(c(testData[,"x31"],
                    testData[,"x32"],
                    testData[,"x33"],
                    testData[,"x34"])))

ymax<-ceiling(max(c(predTest$predictedMeasurements[,"x31"],
                    predTest$predictedMeasurements[,"x32"],
                    predTest$predictedMeasurements[,"x33"],
                    predTest$predictedMeasurements[,"x34"])))

xmin<-floor(min(c(testData[,"x31"],
                  testData[,"x32"],
                  testData[,"x33"],
                  testData[,"x34"])))

ymin<-floor(min(c(predTest$predictedMeasurements[,"x31"],
                  predTest$predictedMeasurements[,"x32"],
                  predTest$predictedMeasurements[,"x33"],
                  predTest$predictedMeasurements[,"x34"])))

#PLS: Actual vs Predicted (x31)
y<-predTest$predictedMeasurements[,"x31"]
x<-testData[,"x31"]
title="Actual vs Predicted (x31)"
xlabel=paste("Mean: Act=",
             signif(mean(x),digits=4),
             "PLS=",
             signif(mean(y),digits=4),
             "\n SD: Act=",
             signif(sd(x),digits=4),
             "PLS=",
             signif(sd(y),digits=4))
ylabel="Predicted"
graphScatterplot42(x,y,title,xlabel,ylabel,xmax=xmax,ymax=ymax,xmin=xmin,ymin=ymin)

#PLS: Actual vs Predicted (x32)
y<-predTest$predictedMeasurements[,"x32"]
x<-testData[,"x32"]
title="Actual vs Predicted (x32)"
xlabel=paste("Mean: Act=",
             signif(mean(x),digits=4),
             "PLS=",
             signif(mean(y),digits=4),
             "\n SD: Act=",
             signif(sd(x),digits=4),
             "PLS=",
             signif(sd(y),digits=4))
ylabel="Predicted"
graphScatterplot42(x,y,title,xlabel,ylabel,xmax=xmax,ymax=ymax,xmin=xmin,ymin=ymin)

#PLS: Actual vs Predicted (x33)
y<-predTest$predictedMeasurements[,"x33"]
x<-testData[,"x33"]
title="Actual vs Predicted (x33)"
xlabel=paste("Mean: Act=",
             signif(mean(x),digits=4),
             "PLS=",
             signif(mean(y),digits=4),
             "\n SD: Act=",
             signif(sd(x),digits=4),
             "PLS=",
             signif(sd(y),digits=4))
ylabel="Predicted"
graphScatterplot42(x,y,title,xlabel,ylabel,xmax=xmax,ymax=ymax,xmin=xmin,ymin=ymin)

#PLS: Actual vs Predicted (x34)
y<-predTest$predictedMeasurements[,"x34"]
x<-testData[,"x34"]
title="Actual vs Predicted (x34)"
xlabel=paste("Mean: Act=",
             signif(mean(x),digits=4),
             "PLS=",
             signif(mean(y),digits=4),
             "\n SD: Act=",
             signif(sd(x),digits=4),
             "PLS=",
             signif(sd(y),digits=4))
ylabel="Predicted"
graphScatterplot42(x,y,title,xlabel,ylabel,xmax=xmax,ymax=ymax,xmin=xmin,ymin=ymin)


#Set the panels
par(mfrow=c(2,2))

title<-"Prediction Residuals"

xrange<-c(-4,4)
yrange<-c(0,0.8)

newgraphResiduals("x31",predTest$residuals,title,xrange,yrange)

newgraphResiduals("x32",predTest$residuals,title,xrange,yrange)

newgraphResiduals("x33",predTest$residuals,title,xrange,yrange)

newgraphResiduals("x34",predTest$residuals,title,xrange,yrange)

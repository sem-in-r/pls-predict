
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


#Multiple LInear Regeresion for each output variable
lmX31 <- with(trainData, lm(x31 ~ x11+x12+x13+x21+x22+x23))
lmX32 <- with(trainData, lm(x32 ~ x11+x12+x13+x21+x22+x23))
lmX33 <- with(trainData, lm(x33 ~ x11+x12+x13+x21+x22+x23))
lmX34 <- with(trainData, lm(x34 ~ x11+x12+x13+x21+x22+x23))

#Use our custom function to predict each value for training data
predTestX31 <- predictlm (lmX31,testData)
predTestX32 <- predictlm (lmX32,testData)
predTestX33 <- predictlm (lmX33,testData)
predTestX34 <- predictlm (lmX34,testData)

#Join the Predictions in vector
predTestlm <- cbind(predTestX31,predTestX32,predTestX33,predTestX34)
names(predTestlm)<-c("x31","x32","x33","x34")

#Join the Residuals in vector
residualsTestlm <- testData[,c("x31","x32","x33","x34")]-cbind(predTestX31,predTestX32,predTestX33,predTestX34)

#Prepare Object
lm <- list(predTest = list(predictedMeasurements = predTestlm),
           residualsTest= residualsTestlm)


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
                    predTest$predictedMeasurements[,"x34"],
                    lm$predTest$predictedMeasurements[,"x31"],
                    lm$predTest$predictedMeasurements[,"x32"],
                    lm$predTest$predictedMeasurements[,"x33"],
                    lm$predTest$predictedMeasurements[,"x34"])))

xmin<-floor(min(c(testData[,"x31"],
                  testData[,"x32"],
                  testData[,"x33"],
                  testData[,"x34"])))

ymin<-floor(min(c(predTest$predictedMeasurements[,"x31"],
                  predTest$predictedMeasurements[,"x32"],
                  predTest$predictedMeasurements[,"x33"],
                  predTest$predictedMeasurements[,"x34"],
                  lm$predTest$predictedMeasurements[,"x31"],
                  lm$predTest$predictedMeasurements[,"x32"],
                  lm$predTest$predictedMeasurements[,"x33"],
                  lm$predTest$predictedMeasurements[,"x34"])))


#Actual vs Predicted (x31)
y<-predTest$predictedMeasurements[,"x31"]
x<-testData[,"x31"]
z<-lm$predTest$predictedMeasurements[,"x31"]
title="Actual vs Predicted (X31)"
xlabel=paste("Means: Act=",
             signif(mean(x),digits=4),
             "PLS=",
             signif(mean(y),digits=4),
             "LM=",
             signif(mean(z),digits=4),
             "\n Standard Deviation: Act=",
             signif(sd(x),digits=4),
             "PLS=",
             signif(sd(y),digits=4),
             "LM=",
             signif(sd(z),digits=4))
ylabel="Predicted"
dualGraphScatterplot(x,y,z,title,xlabel,ylabel,xmax=xmax,ymax=ymax,xmin=xmin,ymin=ymin)


#Actual vs Predicted (x32)
y<-predTest$predictedMeasurements[,"x32"]
x<-testData[,"x32"]
z<-lm$predTest$predictedMeasurements[,"x32"]
title="Actual vs Predicted (X32)"
xlabel=paste("Means: Act=",
             signif(mean(x),digits=4),
             "PLS=",
             signif(mean(y),digits=4),
             "LM=",
             signif(mean(z),digits=4),
             "\n Standard Deviation: Act=",
             signif(sd(x),digits=4),
             "PLS=",
             signif(sd(y),digits=4),
             "LM=",
             signif(sd(z),digits=4))
ylabel="Predicted"
dualGraphScatterplot(x,y,z,title,xlabel,ylabel,xmax=xmax,ymax=ymax,xmin=xmin,ymin=ymin)

#Actual vs Predicted (x33)
y<-predTest$predictedMeasurements[,"x33"]
x<-testData[,"x33"]
z<-lm$predTest$predictedMeasurements[,"x33"]
title="Actual vs Predicted (X33)"
xlabel=paste("Means: Act=",
             signif(mean(x),digits=4),
             "PLS=",
             signif(mean(y),digits=4),
             "LM=",
             signif(mean(z),digits=4),
             "\n Standard Deviation: Act=",
             signif(sd(x),digits=4),
             "PLS=",
             signif(sd(y),digits=4),
             "LM=",
             signif(sd(z),digits=4))
ylabel="Predicted"
dualGraphScatterplot(x,y,z,title,xlabel,ylabel,xmax=xmax,ymax=ymax,xmin=xmin,ymin=ymin)

#Actual vs Predicted (x34)
y<-predTest$predictedMeasurements[,"x34"]
x<-testData[,"x34"]
z<-lm$predTest$predictedMeasurements[,"x34"]
title="Actual vs Predicted (X34)"
xlabel=paste("Means: Act=",
             signif(mean(x),digits=4),
             "PLS=",
             signif(mean(y),digits=4),
             "LM=",
             signif(mean(z),digits=4),
             "\n Standard Deviation: Act=",
             signif(sd(x),digits=4),
             "PLS=",
             signif(sd(y),digits=4),
             "LM=",
             signif(sd(z),digits=4))
ylabel="Predicted"
dualGraphScatterplot(x,y,z,title,xlabel,ylabel,xmax=xmax,ymax=ymax,xmin=xmin,ymin=ymin)

#Set the panels
par(mfrow=c(2,2))

#Find scales
xmax<-ceiling(max(c(lm$predTest$predictedMeasurements[,"x31"],
                    lm$predTest$predictedMeasurements[,"x32"],
                    lm$predTest$predictedMeasurements[,"x33"],
                    lm$predTest$predictedMeasurements[,"x34"])))

ymax<-ceiling(max(c(predTest$predictedMeasurements[,"x31"],
                    predTest$predictedMeasurements[,"x32"],
                    predTest$predictedMeasurements[,"x33"],
                    predTest$predictedMeasurements[,"x34"])))

xmin<-floor(min(c(lm$predTest$predictedMeasurements[,"x31"],
                  lm$predTest$predictedMeasurements[,"x32"],
                  lm$predTest$predictedMeasurements[,"x33"],
                  lm$predTest$predictedMeasurements[,"x34"])))

ymin<-floor(min(c(predTest$predictedMeasurements[,"x31"],
                  predTest$predictedMeasurements[,"x32"],
                  predTest$predictedMeasurements[,"x33"],
                  predTest$predictedMeasurements[,"x34"])))


#PLS: LM vs PLS (x31)
y<-predTest$predictedMeasurements[,"x31"]
x<-lm$predTest$predictedMeasurements[,"x31"]
title="LM vs PLS (x31)"
xlabel="Linear Model"
ylabel="PLS"
graphScatterplot42(x,y,title,xlabel,ylabel,xmax=xmax,ymax=ymax,xmin=xmin,ymin=ymin)

#PLS: LM vs PLS (x32)
y<-predTest$predictedMeasurements[,"x32"]
x<-lm$predTest$predictedMeasurements[,"x32"]
title="LM vs PLS (x32)"
xlabel="Linear Model"
ylabel="PLS"
graphScatterplot42(x,y,title,xlabel,ylabel,xmax=xmax,ymax=ymax,xmin=xmin,ymin=ymin)

#PLS: LM vs PLS (x33)
y<-predTest$predictedMeasurements[,"x33"]
x<-lm$predTest$predictedMeasurements[,"x33"]
title="LM vs PLS (x33)"
xlabel="Linear Model"
ylabel="PLS"
graphScatterplot42(x,y,title,xlabel,ylabel,xmax=xmax,ymax=ymax,xmin=xmin,ymin=ymin)

#PLS: LM vs PLS (x34)
y<-predTest$predictedMeasurements[,"x34"]
x<-lm$predTest$predictedMeasurements[,"x34"]
title="LM vs PLS (x34)"
xlabel="Linear Model"
ylabel="PLS"
graphScatterplot42(x,y,title,xlabel,ylabel,xmax=xmax,ymax=ymax,xmin=xmin,ymin=ymin)

#Set the panels
par(mfrow=c(2,2))
xrange<-c(-4,4)
yrange<-c(0,0.8)
title<-"PLS vs LM Prediction Residuals"
xcolor=rgb(0.25,0.25,0.25,0.5)
ycolor=rgb(0,0,0,0.5)

graphCombinedResiduals("x31",lm$residualsTest,predTest$residuals ,title,xrange,yrange,10,"LM","PLS",xcolor,ycolor)
graphCombinedResiduals("x32",lm$residualsTest,predTest$residuals ,title,xrange,yrange,10,"LM","PLS",xcolor,ycolor)
graphCombinedResiduals("x33",lm$residualsTest,predTest$residuals ,title,xrange,yrange,10,"LM","PLS",xcolor,ycolor)
graphCombinedResiduals("x34",lm$residualsTest,predTest$residuals ,title,xrange,yrange,10,"LM","PLS",xcolor,ycolor)

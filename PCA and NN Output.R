
#----------------------------------Initializing Environment--------------------------------

#Clear Environment
rm(list=ls())

#Load our Algorithm
source("./lib/simplePLS.R")

#Load Library for PCA
library(psych)

#Load Data
trainData=read.csv("./data/trainingData_Random.csv")
testData=read.csv("./data/testData_Random.csv")

#Load Data Suneel
Anime=read.csv("./data/originalData.csv",header=T)
set.seed(123)
index=sample.int(dim(Anime)[1],83,replace=F)
trainData=Anime[-index,]
testData=Anime[index,]
npred=read.csv("./data/npred.csv")

#Prepare objects
original <- list (lm = list(),
                  pls = list(),
                  nn = list())

pca <- list (lm = list(),
             pls = list())

#Calculate neural net residuals
nn_residuals <- testData[,c("AA.0","AA.1","AA.2","AA.3")] - npred[,c("AA.0","AA.1","AA.2","AA.3")]

#Load Neural net Prediction
nn <- list(predTest = list(predictedMeasurements = npred),
           residualsTest= nn_residuals)
original$nn<- nn

#----------------------Do PCA Analysis to produce new scenarios---------------------------------

#Filter the dataset for the AA values
Data<-trainData[,c("AA.0","AA.1","AA.2","AA.3")]

#Do Principal Components Analysis
pcaData <- principal(Data,4,rotate ="none")

#Add the Principal Components to the Original Data
trainData<-cbind(trainData,pcaData$scores)

#Filter the dataset for the AA values
Data<-testData[,c("AA.0","AA.1","AA.2","AA.3")]

#Do Principal Components Analysis
pcaData <- principal(Data,4,rotate ="none")

#Add the Principal Components to the Original Data
testData<-cbind(testData,pcaData$scores)


#-------------------------------Begin the PLS-PM Analysis---------------------------------

#PCA Analysis

#Create the Matrix of the Structural Model
smMatrix <- matrix(c("Perceived Visual Complexity", "Aproach/Avoidance",
                     "Arousal","Aproach/Avoidance"),nrow=2,ncol=2,byrow =TRUE,
                   dimnames = list(1:2,c("source","target")))

#Create the Matrix of the Measurement Model
mmMatrix <- matrix(c("Perceived Visual Complexity","VX.0","F",
                     "Perceived Visual Complexity","VX.1","F",
                     "Perceived Visual Complexity","VX.2","F",
                     "Perceived Visual Complexity","VX.3","F",
                     "Perceived Visual Complexity","VX.4","F",
                     "Arousal","Aro1","F",
                     "Arousal","Aro2","F",
                     "Arousal","Aro3","F",
                     "Arousal","Aro4","F",
                     "Aproach/Avoidance","PC1","R",
                     "Aproach/Avoidance","PC2","R",
                     "Aproach/Avoidance","PC3","R",
                     "Aproach/Avoidance","PC4","R"),nrow=13,ncol=3,byrow =TRUE,
                   dimnames = list(1:13,c("latent","measurement","type")))


#Call PLS-PM Function
plsModel<-simplePLS(trainData,smMatrix,mmMatrix)

#Call Prediction Function
predTrain <- PLSpredict(plsModel,trainData)
predTest <- PLSpredict(plsModel,testData)

#Get Residuals
residualsTraining <- predTrain$residuals
residualsTest <- predTest$residuals

#Prepare Object
pls <- list(predTrain = predTrain,
            predTest = predTest,
            residualsTraining = residualsTraining,
            residualsTest= residualsTest)

#Assign to List
pca$pls = pls

#Original Data

#Create the Matrix of the Structural Model
smMatrix <- matrix(c("Perceived Visual Complexity", "Aproach/Avoidance",
                     "Arousal","Aproach/Avoidance"),nrow=2,ncol=2,byrow =TRUE,
                   dimnames = list(1:2,c("source","target")))

#Create the Matrix of the Measurement Model
mmMatrix <- matrix(c("Perceived Visual Complexity","VX.0","F",
                     "Perceived Visual Complexity","VX.1","F",
                     "Perceived Visual Complexity","VX.2","F",
                     "Perceived Visual Complexity","VX.3","F",
                     "Perceived Visual Complexity","VX.4","F",
                     "Arousal","Aro1","F",
                     "Arousal","Aro2","F",
                     "Arousal","Aro3","F",
                     "Arousal","Aro4","F",
                     "Aproach/Avoidance","AA.0","R",
                     "Aproach/Avoidance","AA.1","R",
                     "Aproach/Avoidance","AA.2","R",
                     "Aproach/Avoidance","AA.3","R"),nrow=13,ncol=3,byrow =TRUE,
                   dimnames = list(1:13,c("latent","measurement","type")))

#Call PLS-PM Function
plsModel<-simplePLS(trainData,smMatrix,mmMatrix)

#Call Prediction Function
predTrain <- PLSpredict(plsModel,trainData)
predTest <- PLSpredict(plsModel,testData)

#Get Residuals
residualsTraining <- predTrain$residuals
residualsTest <- predTest$residuals

#Prepare Object
pls <- list(predTrain = predTrain,
            predTest = predTest,
            residualsTraining = residualsTraining,
            residualsTest= residualsTest)

#Assign to List
original$pls<-pls

#--------------------------------------Begin LM Analysis---------------------------------

#PCA

#Multiple LInear Regeresion for each output variable
lmAA0 <- with(trainData, lm(PC1 ~ VX.0+VX.1+VX.2+VX.3+VX.4+Aro1+Aro2+Aro3+Aro4))
lmAA1 <- with(trainData, lm(PC2 ~ VX.0+VX.1+VX.2+VX.3+VX.4+Aro1+Aro2+Aro3+Aro4))
lmAA2 <- with(trainData, lm(PC3 ~ VX.0+VX.1+VX.2+VX.3+VX.4+Aro1+Aro2+Aro3+Aro4))
lmAA3 <- with(trainData, lm(PC4 ~ VX.0+VX.1+VX.2+VX.3+VX.4+Aro1+Aro2+Aro3+Aro4))

#Use our custom function to predict each value for training data
predTrainAA0 <- predictlm (lmAA0,trainData)
predTrainAA1 <- predictlm (lmAA1,trainData)
predTrainAA2 <- predictlm (lmAA2,trainData)
predTrainAA3 <- predictlm (lmAA3,trainData)

#Join the Predictions in vector
predTrain <- cbind(predTrainAA0,predTrainAA1,predTrainAA2,predTrainAA3)
names(predTrain)<-c("PC1","PC2","PC3","PC4")

#Join the Residuals in vector
residualsTraining <- trainData[,c("PC1","PC2","PC3","PC4")]-cbind(predTrainAA0,predTrainAA1,predTrainAA2,predTrainAA3)

#Use our custom function to predict each value for Test data
predTestAA0 <- predictlm (lmAA0,testData)
predTestAA1 <- predictlm (lmAA1,testData)
predTestAA2 <- predictlm (lmAA2,testData)
predTestAA3 <- predictlm (lmAA3,testData)

#Join the Predictions in vector
predTest <- cbind(predTestAA0,predTestAA1,predTestAA2,predTestAA3)
names(predTest)<-c("PC1","PC2","PC3","PC4")

#Calculate the resuduals for the training Data
residualsTest <- testData[,c("PC1","PC2","PC3","PC4")]-cbind(predTestAA0,predTestAA1,predTestAA2,predTestAA3)

#Prepare Object
lm <- list(predTrain = list(predictedMeasurements = predTrain),
           predTest = list(predictedMeasurements = predTest),
           residualsTraining = residualsTraining,
           residualsTest= residualsTest)

#Assign to List
pca$lm<-lm

#Original

#Multiple LInear Regeresion for each output variable
lmAA0 <- with(trainData, lm(AA.0 ~ VX.0+VX.1+VX.2+VX.3+VX.4+Aro1+Aro2+Aro3+Aro4))
lmAA1 <- with(trainData, lm(AA.1 ~ VX.0+VX.1+VX.2+VX.3+VX.4+Aro1+Aro2+Aro3+Aro4))
lmAA2 <- with(trainData, lm(AA.2 ~ VX.0+VX.1+VX.2+VX.3+VX.4+Aro1+Aro2+Aro3+Aro4))
lmAA3 <- with(trainData, lm(AA.3 ~ VX.0+VX.1+VX.2+VX.3+VX.4+Aro1+Aro2+Aro3+Aro4))

#Use our custom function to predict each value for training data
predTrainAA0 <- predictlm (lmAA0,trainData)
predTrainAA1 <- predictlm (lmAA1,trainData)
predTrainAA2 <- predictlm (lmAA2,trainData)
predTrainAA3 <- predictlm (lmAA3,trainData)

#Join the Predictions in vector
predTrain <- cbind(predTrainAA0,predTrainAA1,predTrainAA2,predTrainAA3)
names(predTrain)<-c("AA.0","AA.1","AA.2","AA.3")

#Join the Residuals in vector
residualsTraining <- trainData[,c("AA.0","AA.1","AA.2","AA.3")]-cbind(predTrainAA0,predTrainAA1,predTrainAA2,predTrainAA3)

#Use our custom function to predict each value for Test data
predTestAA0 <- predictlm (lmAA0,testData)
predTestAA1 <- predictlm (lmAA1,testData)
predTestAA2 <- predictlm (lmAA2,testData)
predTestAA3 <- predictlm (lmAA3,testData)

#Join the Predictions in vector
predTest <- cbind(predTestAA0,predTestAA1,predTestAA2,predTestAA3)
names(predTest)<-c("AA.0","AA.1","AA.2","AA.3")

#Calculate the resuduals for the training Data
residualsTest <- testData[,c("AA.0","AA.1","AA.2","AA.3")]-cbind(predTestAA0,predTestAA1,predTestAA2,predTestAA3)

#Prepare Object
lm <- list(predTrain = list(predictedMeasurements = predTrain),
           predTest = list(predictedMeasurements = predTest),
           residualsTraining = residualsTraining,
           residualsTest= residualsTest)

#Assign to List
original$lm<-lm

#--------------------------------------Graphic Analysis---------------------------------


#Set the panels
par(mfrow=c(2,2))

#LM VS PLS (PCA - TestData) PC1
x<-pca$lm$predTest$predictedMeasurements[,"PC1"]
y<-pca$pls$predTest$predictedMeasurements[,"PC1"]
z<-testData[,"PC1"]
title="LM VS PLS (Holdout) PC1"
xlabel=paste("LM \n Means: LM=",
             signif(mean(x),digits=4),
             "PLS=",
             signif(mean(y),digits=4),
             "Act=",
             signif(mean(z),digits=4))
ylabel="PLS-PM"
graphScatterplot(x,y,z,title,xlabel,ylabel)

#LM VS PLS (PCA - TestData) PC2
x<-pca$lm$predTest$predictedMeasurements[,"PC2"]
y<-pca$pls$predTest$predictedMeasurements[,"PC2"]
z<-testData[,"PC2"]
title="LM VS PLS (Holdout) PC2"
xlabel=paste("LM \n Means: LM=",
             signif(mean(x),digits=4),
             "PLS=",
             signif(mean(y),digits=4),
             "Act=",
             signif(mean(z),digits=4))
ylabel="PLS-PM"
graphScatterplot(x,y,z,title,xlabel,ylabel)

#LM VS PLS (PCA - TestData) PC3
x<-pca$lm$predTest$predictedMeasurements[,"PC3"]
y<-pca$pls$predTest$predictedMeasurements[,"PC3"]
z<-testData[,"PC3"]
title="LM VS PLS (Holdout) PC3"
xlabel=paste("LM \n Means: LM=",
             signif(mean(x),digits=4),
             "PLS=",
             signif(mean(y),digits=4),
             "Act=",
             signif(mean(z),digits=4))
ylabel="PLS-PM"
graphScatterplot(x,y,z,title,xlabel,ylabel)

#LM VS PLS (PCA - TestData) PC4
x<-pca$lm$predTest$predictedMeasurements[,"PC4"]
y<-pca$pls$predTest$predictedMeasurements[,"PC4"]
z<-testData[,"PC4"]
title="LM VS PLS (Holdout) PC4"
xlabel=paste("LM \n Means: LM=",
             signif(mean(x),digits=4),
             "PLS=",
             signif(mean(y),digits=4),
             "Act=",
             signif(mean(z),digits=4))
ylabel="PLS-PM"
graphScatterplot(x,y,z,title,xlabel,ylabel)

#Prepare One Panel
par(mfrow=c(2,2))

title<-"PCA - LM Vs PLS (Holdout)"

#Random Data - PLS (Formative) Training PC1
graphCombinedResiduals("PC1",pca$lm$residualsTest,pca$pls$residualsTest,title,c(-4,4),c(0,50),10)

#Random Data - PLS (Formative) Training PC2
graphCombinedResiduals("PC2",pca$lm$residualsTest,pca$pls$residualsTest,title,c(-4,4),c(0,50),20)

#Random Data - PLS (Formative) Training PC3
graphCombinedResiduals("PC3",pca$lm$residualsTest,pca$pls$residualsTest,title,c(-4,4),c(0,50),10)

#Random Data - PLS (Formative) Training PC4
graphCombinedResiduals("PC4",pca$lm$residualsTest,pca$pls$residualsTest,title,c(-4,4),c(0,50),20)




#PLS Vs. NN Analysis

#Set the panels
par(mfrow=c(2,2))

#NN VS PLS (Original - TestData) AA.0
x<-original$nn$predTest$predictedMeasurements[,"AA.0"]
y<-original$pls$predTest$predictedMeasurements[,"AA.0"]
z<-testData[,"AA.0"]
title="NN VS PLS (Holdout) AA.0"
xlabel=paste("NN \n Means: NN=",
             signif(mean(x),digits=4),
             "PLS=",
             signif(mean(y),digits=4),
             "Act=",
             signif(mean(z),digits=4))
ylabel="PLS-PM"
graphScatterplot(x,y,z,title,xlabel,ylabel)

#NN VS PLS (Original - TestData) AA.1
x<-original$nn$predTest$predictedMeasurements[,"AA.1"]
y<-original$pls$predTest$predictedMeasurements[,"AA.1"]
z<-testData[,"AA.1"]
title="NN VS PLS (Holdout) AA.1"
xlabel=paste("NN \n Means: NN=",
             signif(mean(x),digits=4),
             "PLS=",
             signif(mean(y),digits=4),
             "Act=",
             signif(mean(z),digits=4))
ylabel="PLS-PM"
graphScatterplot(x,y,z,title,xlabel,ylabel)


#NN VS PLS (Original - TestData) AA.
x<-original$nn$predTest$predictedMeasurements[,"AA.2"]
y<-original$pls$predTest$predictedMeasurements[,"AA.2"]
z<-testData[,"AA.2"]
title="NN VS PLS (Holdout) AA.2"
xlabel=paste("NN \n Means: NN=",
             signif(mean(x),digits=4),
             "PLS=",
             signif(mean(y),digits=4),
             "Act=",
             signif(mean(z),digits=4))
ylabel="PLS-PM"
graphScatterplot(x,y,z,title,xlabel,ylabel)

#NN VS PLS (Original - TestData) AA.3
x<-original$nn$predTest$predictedMeasurements[,"AA.3"]
y<-original$pls$predTest$predictedMeasurements[,"AA.3"]
z<-testData[,"AA.3"]
title="NN VS PLS (Holdout) AA.3"
xlabel=paste("NN \n Means: NN=",
             signif(mean(x),digits=4),
             "PLS=",
             signif(mean(y),digits=4),
             "Act=",
             signif(mean(z),digits=4))
ylabel="PLS-PM"
graphScatterplot(x,y,z,title,xlabel,ylabel)


#Prepare One Panel
par(mfrow=c(2,2))

title<-"NN Vs PLS (Holdout)"

#Random Data - PLS (Formative) Training PC1
graphCombinedResiduals("AA.0",original$nn$residualsTest,original$pls$residualsTest,title,c(-6,6),c(0,40),10)

#Random Data - PLS (Formative) Training PC2
graphCombinedResiduals("AA.1",original$nn$residualsTest,original$pls$residualsTest,title,c(-6,6),c(0,40),20)

#Random Data - PLS (Formative) Training PC3
graphCombinedResiduals("AA.2",original$nn$residualsTest,original$pls$residualsTest,title,c(-6,6),c(0,40),10)

#Random Data - PLS (Formative) Training PC4
graphCombinedResiduals("AA.3",original$nn$residualsTest,original$pls$residualsTest,title,c(-6,6),c(0,40),20)


#Prepare One Panel
par(mfrow=c(1,2))

#NN VS PLS (Original - TestData) AA.0
x<-testData[,"AA.0"]
y<-original$nn$predTest$predictedMeasurements[,"AA.0"]
z<-original$pls$predTest$predictedMeasurements[,"AA.0"]
title="NN VS PLS (Holdout) AA.0"
xlabel=paste("Actual \n Means: Act=",
             signif(mean(x),digits=4),
             "NN=",
             signif(mean(y),digits=4),
             "PLS=",
             signif(mean(z),digits=4))
ylabel="NN"
graphScatterplot(x,y,z,title,xlabel,ylabel)

#NN VS PLS (Original - TestData) AA.0
x<-testData[,"AA.0"]
y<-original$pls$predTest$predictedMeasurements[,"AA.0"]
z<-original$nn$predTest$predictedMeasurements[,"AA.0"]
title="NN VS PLS (Holdout) AA.0"
xlabel=paste("Actual \n Means: Act=",
             signif(mean(x),digits=4),
             "PLS=",
             signif(mean(y),digits=4),
             "NN=",
             signif(mean(z),digits=4))
ylabel="PLS"
graphScatterplot(x,y,z,title,xlabel,ylabel)

#Prepare One Panel
par(mfrow=c(1,2))

#NN VS PLS (Original - TestData) AA.1
x<-testData[,"AA.1"]
y<-original$nn$predTest$predictedMeasurements[,"AA.1"]
z<-original$pls$predTest$predictedMeasurements[,"AA.1"]
title="NN VS PLS (Holdout) AA.1"
xlabel=paste("Actual \n Means: Act=",
             signif(mean(x),digits=4),
             "NN=",
             signif(mean(y),digits=4),
             "PLS=",
             signif(mean(z),digits=4))
ylabel="NN"
graphScatterplot(x,y,z,title,xlabel,ylabel)

#NN VS PLS (Original - TestData) AA.1
x<-testData[,"AA.1"]
y<-original$pls$predTest$predictedMeasurements[,"AA.1"]
z<-original$nn$predTest$predictedMeasurements[,"AA.1"]
title="NN VS PLS (Holdout) AA.1"
xlabel=paste("Actual \n Means: Act=",
             signif(mean(x),digits=4),
             "PLS=",
             signif(mean(y),digits=4),
             "NN=",
             signif(mean(z),digits=4))
ylabel="PLS"
graphScatterplot(x,y,z,title,xlabel,ylabel)

#Prepare One Panel
par(mfrow=c(1,2))

#NN VS PLS (Original - TestData) AA.2
x<-testData[,"AA.2"]
y<-original$nn$predTest$predictedMeasurements[,"AA.2"]
z<-original$pls$predTest$predictedMeasurements[,"AA.2"]
title="NN VS PLS (Holdout) AA.2"
xlabel=paste("Actual \n Means: Act=",
             signif(mean(x),digits=4),
             "NN=",
             signif(mean(y),digits=4),
             "PLS=",
             signif(mean(z),digits=4))
ylabel="NN"
graphScatterplot(x,y,z,title,xlabel,ylabel)

#NN VS PLS (Original - TestData) AA.2
x<-testData[,"AA.2"]
y<-original$pls$predTest$predictedMeasurements[,"AA.2"]
z<-original$nn$predTest$predictedMeasurements[,"AA.2"]
title="NN VS PLS (Holdout) AA.2"
xlabel=paste("Actual \n Means: Act=",
             signif(mean(x),digits=4),
             "PLS=",
             signif(mean(y),digits=4),
             "NN=",
             signif(mean(z),digits=4))
ylabel="PLS"
graphScatterplot(x,y,z,title,xlabel,ylabel)

#Prepare One Panel
par(mfrow=c(1,2))

#NN VS PLS (Original - TestData) AA.3
x<-testData[,"AA.3"]
y<-original$nn$predTest$predictedMeasurements[,"AA.3"]
z<-original$pls$predTest$predictedMeasurements[,"AA.3"]
title="NN VS PLS (Holdout) AA.3"
xlabel=paste("Actual \n Means: Act=",
             signif(mean(x),digits=4),
             "NN=",
             signif(mean(y),digits=4),
             "PLS=",
             signif(mean(z),digits=4))
ylabel="NN"
graphScatterplot(x,y,z,title,xlabel,ylabel)

#NN VS PLS (Original - TestData) AA.3
x<-testData[,"AA.3"]
y<-original$pls$predTest$predictedMeasurements[,"AA.3"]
z<-original$nn$predTest$predictedMeasurements[,"AA.3"]
title="NN VS PLS (Holdout) AA.3"
xlabel=paste("Actual \n Means: Act=",
             signif(mean(x),digits=4),
             "PLS=",
             signif(mean(y),digits=4),
             "NN=",
             signif(mean(z),digits=4))
ylabel="PLS"
graphScatterplot(x,y,z,title,xlabel,ylabel)




#Set the panels
par(mfrow=c(2,2))

#LM VS PLS (PCA - TestData) AA.0
x<-original$lm$predTest$predictedMeasurements[,"AA.0"]
y<-original$nn$predTest$predictedMeasurements[,"AA.0"]
z<-testData[,"AA.0"]
title="LM VS NN (Holdout) AA.0"
xlabel=paste("LM \n Means: LM=",
             signif(mean(x),digits=4),
             "NN=",
             signif(mean(y),digits=4),
             "Act=",
             signif(mean(z),digits=4))
ylabel="NN"
graphScatterplot(x,y,z,title,xlabel,ylabel)

#LM VS PLS (PCA - TestData) AA.1
x<-original$lm$predTest$predictedMeasurements[,"AA.1"]
y<-original$nn$predTest$predictedMeasurements[,"AA.1"]
z<-testData[,"AA.1"]
title="LM VS NN (Holdout) AA.1"
xlabel=paste("LM \n Means: LM=",
             signif(mean(x),digits=4),
             "NN=",
             signif(mean(y),digits=4),
             "Act=",
             signif(mean(z),digits=4))
ylabel="NN"
graphScatterplot(x,y,z,title,xlabel,ylabel)

#LM VS PLS (PCA - TestData) AA.2
x<-original$lm$predTest$predictedMeasurements[,"AA.2"]
y<-original$nn$predTest$predictedMeasurements[,"AA.2"]
z<-testData[,"AA.2"]
title="LM VS NN (Holdout) AA.2"
xlabel=paste("LM \n Means: LM=",
             signif(mean(x),digits=4),
             "NN=",
             signif(mean(y),digits=4),
             "Act=",
             signif(mean(z),digits=4))
ylabel="NN"
graphScatterplot(x,y,z,title,xlabel,ylabel)

#LM VS PLS (PCA - TestData) AA.3
x<-original$lm$predTest$predictedMeasurements[,"AA.3"]
y<-original$nn$predTest$predictedMeasurements[,"AA.3"]
z<-testData[,"AA.3"]
title="LM VS NN (Holdout) AA.3"
xlabel=paste("LM \n Means: LM=",
             signif(mean(x),digits=4),
             "NN=",
             signif(mean(y),digits=4),
             "Act=",
             signif(mean(z),digits=4))
ylabel="NN"
graphScatterplot(x,y,z,title,xlabel,ylabel)


#Prepare One Panel
par(mfrow=c(2,2))

title<-"NN Vs LM (Holdout)"

#Random Data - PLS (Formative) Training PC1
graphCombinedResiduals("AA.0",original$lm$residualsTest,original$nn$residualsTest,title,c(-6,6),c(0,40),10)

#Random Data - PLS (Formative) Training PC2
graphCombinedResiduals("AA.1",original$lm$residualsTest,original$nn$residualsTest,title,c(-6,6),c(0,40),20)

#Random Data - PLS (Formative) Training PC3
graphCombinedResiduals("AA.2",original$lm$residualsTest,original$nn$residualsTest,title,c(-6,6),c(0,40),10)

#Random Data - PLS (Formative) Training PC4
graphCombinedResiduals("AA.3",original$lm$residualsTest,original$nn$residualsTest,title,c(-6,6),c(0,40),20)

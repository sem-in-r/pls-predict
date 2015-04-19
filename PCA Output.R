
#Clear Environment
rm(list=ls())

#Set seed for randomness
set.seed(123)

#Load Training Data
trainData=read.csv("./data/trainingData_Random.csv")

#Load Library for PCA
library(psych)

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

#Begin the PLS-PM Analysis

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

#Load our Algorithm
source("./lib/simplePLS.R")

#Call PLS-PM Function
plsModel<-simplePLS(trainData,smMatrix,mmMatrix)

#Call Prediction Function
predTrain <- PLSpredict(plsModel,trainData)
predTest <- PLSpredict(plsModel,testData)

#Get Residuals
residualsTraining <- predTrain$residuals
residualsTest <- predTest$residuals

#Prepare Object
plsFormative <- list(predTrain = predTrain,
                     predTest = predTest,
                     residualsTraining = residualsTraining,
                     residualsTest= residualsTest)

#Assign to List
PCA$plsFormative = plsFormative


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
lm <- list(predTrain = predTrain,
           predTest = predTest,
           residualsTraining = residualsTraining,
           residualsTest= residualsTest)

#Assign to List
random$lm = lm




#Random Data - PLS (Formative) Training
graphResiduals ("PC4",PCA$plsFormative$residualsTraining,"PLS(Formative) Random 100 Training Data",c(-6,6))

par(mfrow=c(1,1))

#Set the panels
par(mfrow=c(2,2))

#LM VS PLS (TestData) PC1
plot(random$lm$predTest[,"PC1"], PCA$plsFormative$predTest$predictedMeasurements[,"PC1"] ,
     main ="LM VS PLS (Holdout) PC1",
     xlab=paste("LM \n Means: LM=",
                signif(mean(random$lm$predTest[,"PC1"]),digits=4),
                "PLS=",
                signif(mean(PCA$plsFormative$predTest$predictedMeasurements[,"PC1"]),digits=4),
                "Act=",
                signif(mean(trainData[,"PC1"]),digits=4)),
     ylab="PLS-PM",
     col="blue")
points(mean(testData[,"PC1"]),mean(testData[,"PC1"]), col= "black",pch=5)
points(mean(random$lm$predTest[,"PC1"]),mean(random$lm$predTest[,"PC1"]), col= "red",pch=2)
points(mean(PCA$plsFormative$predTest$predictedMeasurements[,"PC1"]),
       mean(PCA$plsFormative$predTest$predictedMeasurements[,"PC1"]), col= "darkgreen",pch=3)

#LM VS PLS (TestData) PC2
plot(random$lm$predTest[,"PC2"], PCA$plsFormative$predTest$predictedMeasurements[,"PC2"] ,
     main ="LM VS PLS (Holdout) PC2",
     xlab=paste("LM \n  Means: LM=",
                signif(mean(random$lm$predTest[,"PC2"]),digits=4),
                "PLS=",
                signif(mean(PCA$plsFormative$predTest$predictedMeasurements[,"PC2"]),digits=4),
                "Act=",
                signif(mean(trainData[,"PC2"]),digits=4)),
     ylab="PLS-PM",
     col="blue")
points(mean(testData[,"PC2"]),mean(testData[,"PC2"]), col= "black",pch=5)
points(mean(random$lm$predTest[,"PC2"]),mean(random$lm$predTest[,"PC2"]), col= "red",pch=2)
points(mean(PCA$plsFormative$predTest$predictedMeasurements[,"PC2"]),
       mean(PCA$plsFormative$predTest$predictedMeasurements[,"PC2"]), col= "darkgreen",pch=3)

#LM VS PLS (TestData) PC3
plot(random$lm$predTest[,"PC3"], PCA$plsFormative$predTest$predictedMeasurements[,"PC3"] ,
     main ="LM VS PLS (Holdout) PC3",
     xlab=paste("LM \n Means: LM=",
                signif(mean(random$lm$predTest[,"PC3"]),digits=4),
                "PLS=",
                signif(mean(PCA$plsFormative$predTest$predictedMeasurements[,"PC3"]),digits=4),
                "Act=",
                signif(mean(trainData[,"PC3"]),digits=4)),
     ylab="PLS-PM",
     col="blue")
points(mean(testData[,"PC3"]),mean(testData[,"PC3"]), col= "black",pch=5)
points(mean(random$lm$predTest[,"PC3"]),mean(random$lm$predTest[,"PC3"]), col= "red",pch=2)
points(mean(PCA$plsFormative$predTest$predictedMeasurements[,"PC3"]),
       mean(PCA$plsFormative$predTest$predictedMeasurements[,"PC3"]), col= "darkgreen",pch=3)


#LM VS PLS (TestData) PC4
plot(random$lm$predTest[,"PC4"], PCA$plsFormative$predTest$predictedMeasurements[,"PC4"] ,
     main ="LM VS PLS (Holdout) PC4",
     xlab=paste("LM \n Means: LM=",
                signif(mean(random$lm$predTest[,"PC4"]),digits=4),
                "PLS=",
                signif(mean(PCA$plsFormative$predTest$predictedMeasurements[,"PC4"]),digits=4),
                "Act=",
                signif(mean(trainData[,"PC4"]),digits=4)),
     ylab="PLS-PM",
     col="blue")
points(mean(testData[,"PC4"]),mean(testData[,"PC4"]), col= "black",pch=5)
points(mean(random$lm$predTest[,"PC4"]),mean(random$lm$predTest[,"PC4"]), col= "red",pch=2)
points(mean(PCA$plsFormative$predTest$predictedMeasurements[,"PC4"]),
       mean(PCA$plsFormative$predTest$predictedMeasurements[,"PC4"]), col= "darkgreen",pch=3)




library(scatterplot3d)
install.packages("scatterplot3d")

plot(random$lm$predTest[,"PC1"], PCA$plsFormative$predTest$predictedMeasurements[,"PC1"] ,
     
scatterplot3d(random$lm$predTest[,"PC1"],
              PCA$plsFormative$predTest$predictedMeasurements[,"PC1"],
              testData[,"PC1"])


#Load Functions
source("./lib/simplePLS.R")


#-----------------------------Scenario 1: Original Data--------------------

#Load Data
trainData <- read.csv("./data/trainingData_CutOff.csv", header=TRUE)
testData <- read.csv("./data/testData_CutOff.csv", header=TRUE)

#Prepare object
original <- list (lm = list(),
                  plsFormative = list(),
                  plsNeuralNet = list())

#-----------------------------Start of Linear Regression--------------------

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
lm <- list(predTrain = predTrain,
           predTest = predTest,
           residualsTraining = residualsTraining,
           residualsTest= residualsTest)

#Assign to List
original$lm = lm

#-------------------------Start of PLS-PM (Formative)----------------------

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
plsFormative <- list(predTrain = predTrain,
                     predTest = predTest,
                     residualsTraining = residualsTraining,
                     residualsTest= residualsTest)

#Assign to List
original$plsFormative = plsFormative

#-------------------------Start of PLS-PM (NeuralNet)----------------------
#Create the Matrix of the Structural Model
smMatrix <- matrix(c("PCV/Arousal","Aproach/Avoidance"),nrow=1,ncol=2,byrow =TRUE,
                   dimnames = list(1:1,c("source","target")))

#Create the Matrix of the Measurement Model
mmMatrix <- matrix(c("PCV/Arousal","VX.0","F",
                     "PCV/Arousal","VX.1","F",
                     "PCV/Arousal","VX.2","F",
                     "PCV/Arousal","VX.3","F",
                     "PCV/Arousal","VX.4","F",
                     "PCV/Arousal","Aro1","F",
                     "PCV/Arousal","Aro2","F",
                     "PCV/Arousal","Aro3","F",
                     "PCV/Arousal","Aro4","F",
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
plsNeuralNet <- list(predTrain = predTrain,
                     predTest = predTest,
                     residualsTraining = residualsTraining,
                     residualsTest= residualsTest)

#Assign to List
original$plsNeuralNet = plsNeuralNet

#-----------------------------Scenario 2: Random Data--------------------

#Random
trainData <- read.csv("./data/trainingData_Random.csv", header=TRUE)
testData <- read.csv("./data/testData_Random.csv", header=TRUE)

#Prepare object
random <- list (lm = list(),
                plsFormative = list(),
                plsNeuralNet = list())

#-----------------------------Start of Linear Regression--------------------

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
lm <- list(predTrain = predTrain,
           predTest = predTest,
           residualsTraining = residualsTraining,
           residualsTest= residualsTest)

#Assign to List
random$lm = lm

#-------------------------Start of PLS-PM (Formative)----------------------

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
plsFormative <- list(predTrain = predTrain,
                     predTest = predTest,
                     residualsTraining = residualsTraining,
                     residualsTest= residualsTest)

#Assign to List
random$plsFormative = plsFormative

#-------------------------Start of PLS-PM (NeuralNet)----------------------
#Create the Matrix of the Structural Model
smMatrix <- matrix(c("PCV/Arousal","Aproach/Avoidance"),nrow=1,ncol=2,byrow =TRUE,
                   dimnames = list(1:1,c("source","target")))

#Create the Matrix of the Measurement Model
mmMatrix <- matrix(c("PCV/Arousal","VX.0","F",
                     "PCV/Arousal","VX.1","F",
                     "PCV/Arousal","VX.2","F",
                     "PCV/Arousal","VX.3","F",
                     "PCV/Arousal","VX.4","F",
                     "PCV/Arousal","Aro1","F",
                     "PCV/Arousal","Aro2","F",
                     "PCV/Arousal","Aro3","F",
                     "PCV/Arousal","Aro4","F",
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
plsNeuralNet <- list(predTrain = predTrain,
                     predTest = predTest,
                     residualsTraining = residualsTraining,
                     residualsTest= residualsTest)

#Assign to List
random$plsNeuralNet = plsNeuralNet

#-----------------------Presenting Results (Graphics)----------------------

#Original Vs Random
  
#Set the panels
par(mfrow=c(2,2))

#Original Data - PLS (Formative) Training
graphResiduals ("AA.0",original$plsFormative$residualsTraining,"PLS(Formative) Original 100 Training Data",c(-6,6))

#Original Data - PLS (Formative) Holdout
graphResiduals ("AA.0",original$plsFormative$residualsTest,"PLS(Formative) Original 83 Holdout Data",c(-6,6))

#Random Data - PLS (Formative) Training
graphResiduals ("AA.0",random$plsFormative$residualsTraining,"PLS(Formative) Random 100 Training Data",c(-6,6))

#Random Data - PLS (Formative) Holdoout
graphResiduals ("AA.0",random$plsFormative$residualsTest,"PLS(Formative) Random 83 Holdout Data",c(-6,6))


#Actual Vs. Predict (Formative vs Lm)

#Set the panels
par(mfrow=c(2,2))

#Random Data - PLS (Formative) Training
plot(trainData[,"AA.0"], random$plsFormative$predTrain$predictedMeasurements[,"AA.0"] ,
     main ="PLS 100 Random Train Data, Actual Vs. Predicted (AA.0)",
     xlab=paste("Actual \n 100 Obs, Actual Mean=",
                signif(mean(trainData[,"AA.0"]),digits=4),
                "Predicted Mean =",
                signif(mean(random$plsFormative$predTrain$predictedMeasurements[,"AA.0"]),digits=4)),
     ylab="Predicted",
     col="blue")
points(mean(trainData[,"AA.0"]),mean(trainData[,"AA.0"]), col= "red",pch=2)
points(mean(random$plsFormative$predTrain$predictedMeasurements[,"AA.0"]),
       mean(random$plsFormative$predTrain$predictedMeasurements[,"AA.0"]), col= "darkgreen",pch=3)

#Random Data - PLS (Formative) Test
plot(testData[,"AA.0"], random$plsFormative$predTest$predictedMeasurements[,"AA.0"] ,
     main ="PLS 83 Random Holdout Data, Actual Vs. Predicted (AA.0)",
     xlab=paste("Actual \n 83 Obs, Actual Mean=",
                signif(mean(testData[,"AA.0"]),digits=4),
                "Predicted Mean =",
                signif(mean(random$plsFormative$predTest$predictedMeasurements[,"AA.0"]),digits=4)),
     ylab="Predicted",
     col="blue")
points(mean(testData[,"AA.0"]),mean(testData[,"AA.0"]), col= "red",pch=2)
points(mean(random$plsFormative$predTest$predictedMeasurements[,"AA.0"]),
       mean(random$plsFormative$predTest$predictedMeasurements[,"AA.0"]), col= "darkgreen",pch=3)


#Random Data - LM Training
plot(trainData[,"AA.0"], random$lm$predTrain[,"AA.0"] ,
     main ="LM 100 Random Train Data, Actual Vs. Predicted (AA.0)",
     xlab=paste("Actual \n 100 Obs, Actual Mean=",
                signif(mean(trainData[,"AA.0"]),digits=4),
                "Predicted Mean =",
                signif(mean(random$lm$predTrain[,"AA.0"]),digits=4)),
     ylab="Predicted",
     col="blue")
points(mean(trainData[,"AA.0"]),mean(trainData[,"AA.0"]), col= "red",pch=2)
points(mean(random$lm$predTrain[,"AA.0"]),
       mean(random$lm$predTrain[,"AA.0"]), col= "darkgreen",pch=3)

#Random Data - LM Test
plot(testData[,"AA.0"], random$lm$predTest[,"AA.0"] ,
     main ="LM 83 Random Holdout Data, Actual Vs. Predicted (AA.0)",
     xlab=paste("Actual \n 83 Obs, Actual Mean=",
                signif(mean(testData[,"AA.0"]),digits=4),
                "Predicted Mean =",
                signif(mean(random$lm$predTest[,"AA.0"]),digits=4)),
     ylab="Predicted",
     col="blue")
points(mean(testData[,"AA.0"]),mean(testData[,"AA.0"]), col= "red",pch=2)
points(mean(random$lm$predTest[,"AA.0"]),
       mean(random$lm$predTest[,"AA.0"]), col= "darkgreen",pch=3)




#Set the panels
par(mfrow=c(2,4))

#Random Data - PLS (Formative) Test AA.0
plot(testData[,"AA.0"], random$plsFormative$predTest$predictedMeasurements[,"AA.0"] ,
     main ="PLS 83 Random Holdout Data (AA.0)",
     xlab=paste("Actual \n 83 Obs, Actual Mean=",
                signif(mean(testData[,"AA.0"]),digits=4),
                "Predicted Mean =",
                signif(mean(random$plsFormative$predTest$predictedMeasurements[,"AA.0"]),digits=4)),
     ylab="Predicted",
     col="blue")
points(mean(testData[,"AA.0"]),mean(testData[,"AA.0"]), col= "red",pch=2)
points(mean(random$plsFormative$predTest$predictedMeasurements[,"AA.0"]),
       mean(random$plsFormative$predTest$predictedMeasurements[,"AA.0"]), col= "darkgreen",pch=3)

#Random Data - PLS (Formative) Test AA.1
plot(testData[,"AA.1"], random$plsFormative$predTest$predictedMeasurements[,"AA.1"] ,
     main ="PLS 83 Random Holdout Data (AA.1)",
     xlab=paste("Actual \n 83 Obs, Actual Mean=",
                signif(mean(testData[,"AA.1"]),digits=4),
                "Predicted Mean =",
                signif(mean(random$plsFormative$predTest$predictedMeasurements[,"AA.1"]),digits=4)),
     ylab="Predicted",
     col="blue")
points(mean(testData[,"AA.1"]),mean(testData[,"AA.1"]), col= "red",pch=2)
points(mean(random$plsFormative$predTest$predictedMeasurements[,"AA.1"]),
       mean(random$plsFormative$predTest$predictedMeasurements[,"AA.1"]), col= "darkgreen",pch=3)

#Random Data - PLS (Formative) Test AA.2
plot(testData[,"AA.2"], random$plsFormative$predTest$predictedMeasurements[,"AA.2"] ,
     main ="PLS 83 Random Holdout Data (AA.2)",
     xlab=paste("Actual \n 83 Obs, Actual Mean=",
                signif(mean(testData[,"AA.2"]),digits=4),
                "Predicted Mean =",
                signif(mean(random$plsFormative$predTest$predictedMeasurements[,"AA.2"]),digits=4)),
     ylab="Predicted",
     col="blue")
points(mean(testData[,"AA.2"]),mean(testData[,"AA.2"]), col= "red",pch=2)
points(mean(random$plsFormative$predTest$predictedMeasurements[,"AA.2"]),
       mean(random$plsFormative$predTest$predictedMeasurements[,"AA.2"]), col= "darkgreen",pch=3)

#Random Data - PLS (Formative) Test AA.3
plot(testData[,"AA.3"], random$plsFormative$predTest$predictedMeasurements[,"AA.3"] ,
     main ="PLS 83 Random Holdout Data (AA.3)",
     xlab=paste("Actual \n 83 Obs, Actual Mean=",
                signif(mean(testData[,"AA.3"]),digits=4),
                "Predicted Mean =",
                signif(mean(random$plsFormative$predTest$predictedMeasurements[,"AA.2"]),digits=4)),
     ylab="Predicted",
     col="blue")
points(mean(testData[,"AA.3"]),mean(testData[,"AA.3"]), col= "red",pch=2)
points(mean(random$plsFormative$predTest$predictedMeasurements[,"AA.3"]),
       mean(random$plsFormative$predTest$predictedMeasurements[,"AA.3"]), col= "darkgreen",pch=3)

#Random Data - LM Test AA.0
plot(testData[,"AA.0"], random$lm$predTest[,"AA.0"] ,
     main ="LM 83 Random Holdout Data (AA.0)",
     xlab=paste("Actual \n 83 Obs, Actual Mean=",
                signif(mean(testData[,"AA.0"]),digits=4),
                "Predicted Mean =",
                signif(mean(random$lm$predTest[,"AA.0"]),digits=4)),
     ylab="Predicted",
     col="blue")
points(mean(testData[,"AA.0"]),mean(testData[,"AA.0"]), col= "red",pch=2)
points(mean(random$lm$predTest[,"AA.0"]),
       mean(random$lm$predTest[,"AA.0"]), col= "darkgreen",pch=3)

#Random Data - LM Test AA.1
plot(testData[,"AA.1"], random$lm$predTest[,"AA.1"] ,
     main ="LM 83 Random Holdout Data (AA.1)",
     xlab=paste("Actual \n 83 Obs, Actual Mean=",
                signif(mean(testData[,"AA.1"]),digits=4),
                "Predicted Mean =",
                signif(mean(random$lm$predTest[,"AA.1"]),digits=4)),
     ylab="Predicted",
     col="blue")
points(mean(testData[,"AA.1"]),mean(testData[,"AA.1"]), col= "red",pch=2)
points(mean(random$lm$predTest[,"AA.1"]),
       mean(random$lm$predTest[,"AA.1"]), col= "darkgreen",pch=3)

#Random Data - LM Test AA.2
plot(testData[,"AA.2"], random$lm$predTest[,"AA.2"] ,
     main ="LM 83 Random Holdout Data (AA.2)",
     xlab=paste("Actual \n 83 Obs, Actual Mean=",
                signif(mean(testData[,"AA.2"]),digits=4),
                "Predicted Mean =",
                signif(mean(random$lm$predTest[,"AA.1"]),digits=4)),
     ylab="Predicted",
     col="blue")
points(mean(testData[,"AA.2"]),mean(testData[,"AA.2"]), col= "red",pch=2)
points(mean(random$lm$predTest[,"AA.2"]),
       mean(random$lm$predTest[,"AA.2"]), col= "darkgreen",pch=3)

#Random Data - LM Test AA.3
plot(testData[,"AA.3"], random$lm$predTest[,"AA.3"] ,
     main ="LM 83 Random Holdout Data (AA.3)",
     xlab=paste("Actual \n 83 Obs, Actual Mean=",
                signif(mean(testData[,"AA.3"]),digits=4),
                "Predicted Mean =",
                signif(mean(random$lm$predTest[,"AA.3"]),digits=4)),
     ylab="Predicted",
     col="blue")
points(mean(testData[,"AA.3"]),mean(testData[,"AA.3"]), col= "red",pch=2)
points(mean(random$lm$predTest[,"AA.3"]),
       mean(random$lm$predTest[,"AA.3"]), col= "darkgreen",pch=3)


#Histograms PLS Formative Vs LM


#Set the panels
par(mfrow=c(2,4))

#Random Data - PLS (Formative) Holdoout AA.0
graphResiduals ("AA.0",random$plsFormative$residualsTest,"PLS Random 83 Holdout Data",c(-6,6))

#Random Data - PLS (Formative) Holdoout AA.1
graphResiduals ("AA.1",random$plsFormative$residualsTest,"PLS Random 83 Holdout Data",c(-6,6))

#Random Data - PLS (Formative) Holdoout AA.2
graphResiduals ("AA.2",random$plsFormative$residualsTest,"PLS Random 83 Holdout Data",c(-6,6))

#Random Data - PLS (Formative) Holdoout AA.3
graphResiduals ("AA.3",random$plsFormative$residualsTest,"PLS Random 83 Holdout Data",c(-6,6))

#Random Data - LM Holdoout AA.0
graphResiduals ("AA.0",random$lm$residualsTest,"LM Random 83 Holdout Data",c(-6,6))

#Random Data - LM Holdoout AA.1
graphResiduals ("AA.1",random$lm$residualsTest,"LM Random 83 Holdout Data",c(-6,6))

#Random Data - LM Holdoout AA.2
graphResiduals ("AA.2",random$lm$residualsTest,"LM Random 83 Holdout Data",c(-6,6))

#Random Data - LM Holdoout AA.3
graphResiduals ("AA.3",random$lm$residualsTest,"LM Random 83 Holdout Data",c(-6,6))




#Formative Vs. Neural Net Simulation

#Set the panels
par(mfrow=c(2,2))

#Random Data - PLS (Formative) Training
plot(trainData[,"AA.0"], random$plsFormative$predTrain$predictedMeasurements[,"AA.0"] ,
     main ="Formative 100 Random Train Data, Actual Vs. Predicted (AA.0)",
     xlab=paste("Actual \n 100 Obs, Actual Mean=",
                signif(mean(trainData[,"AA.0"]),digits=4),
                "Predicted Mean =",
                signif(mean(random$plsFormative$predTrain$predictedMeasurements[,"AA.0"]),digits=4)),
     ylab="Predicted",
     col="blue")
points(mean(trainData[,"AA.0"]),mean(trainData[,"AA.0"]), col= "red",pch=2)
points(mean(random$plsFormative$predTrain$predictedMeasurements[,"AA.0"]),
       mean(random$plsFormative$predTrain$predictedMeasurements[,"AA.0"]), col= "darkgreen",pch=3)

#Random Data - PLS (Formative) Test
plot(testData[,"AA.0"], random$plsFormative$predTest$predictedMeasurements[,"AA.0"] ,
     main ="Formative 83 Random Holdout Data, Actual Vs. Predicted (AA.0)",
     xlab=paste("Actual \n 83 Obs, Actual Mean=",
                signif(mean(testData[,"AA.0"]),digits=4),
                "Predicted Mean =",
                signif(mean(random$plsFormative$predTest$predictedMeasurements[,"AA.0"]),digits=4)),
     ylab="Predicted",
     col="blue")
points(mean(testData[,"AA.0"]),mean(testData[,"AA.0"]), col= "red",pch=2)
points(mean(random$plsFormative$predTest$predictedMeasurements[,"AA.0"]),
       mean(random$plsFormative$predTest$predictedMeasurements[,"AA.0"]), col= "darkgreen",pch=3)


#Random Data - PLS (Neural Net) Training
plot(trainData[,"AA.0"], random$plsNeuralNet$predTrain$predictedMeasurements[,"AA.0"] ,
     main ="NN 100 Random Train Data, Actual Vs. Predicted (AA.0)",
     xlab=paste("Actual \n 100 Obs, Actual Mean=",
                signif(mean(trainData[,"AA.0"]),digits=4),
                "Predicted Mean =",
                signif(mean(random$plsNeuralNet$predTrain$predictedMeasurements[,"AA.0"]),digits=4)),
     ylab="Predicted",
     col="blue")
points(mean(trainData[,"AA.0"]),mean(trainData[,"AA.0"]), col= "red",pch=2)
points(mean(random$plsNeuralNet$predTrain$predictedMeasurements[,"AA.0"]),
       mean(random$plsNeuralNet$predTrain$predictedMeasurements[,"AA.0"]), col= "darkgreen",pch=3)

#Random Data - PLS (Neural Net) Test
plot(testData[,"AA.0"], random$plsNeuralNet$predTest$predictedMeasurements[,"AA.0"] ,
     main ="NN 83 Random Holdout Data, Actual Vs. Predicted (AA.0)",
     xlab=paste("Actual \n 83 Obs, Actual Mean=",
                signif(mean(testData[,"AA.0"]),digits=4),
                "Predicted Mean =",
                signif(mean(random$plsNeuralNet$predTest$predictedMeasurements[,"AA.0"]),digits=4)),
     ylab="Predicted",
     col="blue")
points(mean(testData[,"AA.0"]),mean(testData[,"AA.0"]), col= "red",pch=2)
points(mean(random$plsNeuralNet$predTest$predictedMeasurements[,"AA.0"]),
       mean(random$plsNeuralNet$predTest$predictedMeasurements[,"AA.0"]), col= "darkgreen",pch=3)

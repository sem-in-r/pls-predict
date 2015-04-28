#Load our Algorithm
source("./lib/simplePLS.R")

#Load Data
Anime=read.csv("./data/originalData.csv",header=T)
set.seed(123)
index=sample.int(dim(Anime)[1],83,replace=F)
trainData=Anime[-index,]
testData=Anime[index,]

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
plsModel<-simplePLS(trainData,smMatrix,mmMatrix,9)

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


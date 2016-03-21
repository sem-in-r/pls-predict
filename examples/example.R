#Load our Algorithm
source("./lib/simplePLS.R")

#Load Data
Anime=read.csv("./data/AnimData.csv",header=T)
set.seed(123)
index=sample.int(dim(Anime)[1],83,replace=F)
trainData=Anime[-index,]
testData=Anime[index,]

#Create the Matrix of the Structural Model
smMatrix <- matrix(c("PerceivedVisualComplexity", "ApproachAvoidance",
                     "Arousal","ApproachAvoidance"),nrow=2,ncol=2,byrow =TRUE,
                   dimnames = list(1:2,c("source","target")))

#Create the Matrix of the Measurement Model
mmMatrix <- matrix(c("PerceivedVisualComplexity","VX.0","F",
                     "PerceivedVisualComplexity","VX.1","F",
                     "PerceivedVisualComplexity","VX.2","F",
                     "PerceivedVisualComplexity","VX.3","F",
                     "PerceivedVisualComplexity","VX.4","F",
                     "Arousal","Aro1","F",
                     "Arousal","Aro2","F",
                     "Arousal","Aro3","F",
                     "Arousal","Aro4","F",
                     "ApproachAvoidance","AA.0","R",
                     "ApproachAvoidance","AA.1","R",
                     "ApproachAvoidance","AA.2","R",
                     "ApproachAvoidance","AA.3","R"),nrow=13,ncol=3,byrow =TRUE,
                   dimnames = list(1:13,c("latent","measurement","type")))

#Call PLS-PM Function
plsModel<-simplePLS(Anime,smMatrix,mmMatrix,300,7)

#Call Prediction Function
predTrain <- PLSpredict(trainData, smMatrix, mmMatrix, 300,9, testData)

#Predicted compositescores
predTrain$compositeScores

#Get Residuals
residualsTraining <- predTrain$residuals
residualsTest <- predTest$residuals

#Prepare Object
pls <- list(predTrain = predTrain,
            predTest = predTest,
            residualsTraining = residualsTraining,
            residualsTest= residualsTest)

#Call validatepredict

predictionMetrics <- validatePredict(Anime, smMatrix, mmMatrix,noFolds=10)
predictionMetrics$totalRMSE
predictionMetrics$totalMAPE
predictionMetrics$totalMAD

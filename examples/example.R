#Load our Algorithm
source("./lib/simplePLS.R")
source("./lib/PLSpredict.R")
source("./lib/predictionInterval.R")
source("./lib/validatePredict.R")

#Load Data
Anime=read.csv("./data/AnimData.csv",header=T)
set.seed(123)
index=sort(sample.int(dim(Anime)[1],83,replace=F))
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
predTrain <- PLSpredict(Anime, Anime, smMatrix, mmMatrix, 300,9)

#Call predictionInterval
PIntervals <- predictionInterval(trainData, smMatrix, mmMatrix, PIprobs = 0.95, noBoots = 200, testData)
PIntervals <- predictionInterval(trainData, smMatrix, mmMatrix, PIprobs = 0.9, maxIt=300, stopCriterion=7,noBoots=200, testData)
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
predictionMetrics$PLSRMSE
predictionMetrics$LMRMSE
predictionMetrics$PLSMAPE
predictionMetrics$LMMAPE
predictionMetrics$PLSMAD
predictionMetrics$LMMAD

#Boxplot of AA.o PI, CI, predicted and actual
# Plot of average case PI, actual and predicted values 
average <- data.frame(PIntervals$averageCasePI[4])
average <- rbind(average, data.frame(t(predTrain$testData[,4])))
average <- rbind(average, data.frame(t(predTrain$predictedMeasurements[,4])))
newdata <- average[order(average[3,])] 
boxplot(newdata[1:2,])
points(1:183, newdata[3,], pch = 18, col = "red", lwd = 1)
points(1:183, newdata[4,], pch = 3, col = "blue", lwd = 1)

casewise <- data.frame(PIntervals$caseWisePI[4])
casewise <- rbind(casewise, data.frame(t(predTrain$testData[,4])))
casewise <- rbind(casewise, data.frame(t(predTrain$predictedMeasurements[,4])))
newdata2 <- casewise[order(casewise[3,])] 
boxplot(newdata2[1:2,])
points(1:183, newdata2[3,], pch = 18, col = "red", lwd = 1)
points(1:183, newdata2[4,], pch = 3, col = "blue", lwd = 1)

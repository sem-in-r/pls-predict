#Load our Algorithm
source("./lib/simplePLS.R")
source("./lib/PLSpredict.R")
source("./lib/predictionInterval.R")
source("./lib/validatePredict.R")

#Load Data
Anime=read.csv("./data/AnimData.csv",header=T)
set.seed(123)
#Slice data into training set and test set
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

#Call PLS-PM Function to estimate model
plsModel<-simplePLS(Anime,smMatrix,mmMatrix,300,7)

# Return results of the model estimation
plsModel

#Call Prediction Function 
predTrain <- PLSpredict(trainData, testData, smMatrix, mmMatrix, 300,9)

# Return the predicted data
predTrain

#Call predictionInterval (shortened number of bootstraps for demonstration)
PIntervals <- predictionInterval(trainData, smMatrix, mmMatrix, PIprobs = 0.95, maxIt=200, stopCriterion=7, noBoots=500, testData)

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

# Visualization of Prediction Intervals
##Create Holders & assign PI data
aveKC1 <- PIntervals$averageCasePI[[1]]
aveKC2 <- PIntervals$averageCasePI[[2]]
aveKC3 <- PIntervals$averageCasePI[[3]]
casewiseKC1 <- PIntervals$caseWisePI[[1]]
casewiseKC2 <- PIntervals$caseWisePI[[2]]
casewiseKC3 <- PIntervals$caseWisePI[[3]]

##Allocate and sort data - first by actual data and then by predicted data
dataholderKC1 <- cbind(t(aveKC1),predTrain$predictedMeasurements[,1], predTrain$testData[,1],t(casewiseKC1) )
dataholderKC2 <- cbind(t(aveKC2),predTrain$predictedMeasurements[,2], predTrain$testData[,2],t(casewiseKC2) )
dataholderKC3 <- cbind(t(aveKC3),predTrain$predictedMeasurements[,3], predTrain$testData[,3],t(casewiseKC3) )
KC1sorted <- dataholderKC1[order(dataholderKC1[,4], dataholderKC1[,3]) , ]
KC2sorted <- dataholderKC2[order(dataholderKC2[,4], dataholderKC2[,3]) , ]
KC3sorted <- dataholderKC3[order(dataholderKC3[,4], dataholderKC3[,3]) , ]

##Plot results function
###Item Y11 residuals
plot(predTrain$testData[,1], predTrain$residuals[,1],ylim = c(-4,4), ylab = "Residuals", xlab = "Actuals", main = "PLS Residuals for item Y11",pch = 16, col = rgb(0,0,0,0.2) )
abline(h = 0)
abline(h = predictionMetrics$PLSRMSE[,1], lty = 2)
abline(h = -predictionMetrics$PLSRMSE[,1], lty = 2)

### Item Y11 PLS Prediction Intervals
plot(NULL, xlim = c(1,nrow(KC1sorted)), ylim = c(0,9), ylab = "Ranges", xlab = "Cases", type = "n", main = "PLS Prediction Intervals for item Y11")
segments(c(1:83),KC1sorted[,5],c(1:83),KC1sorted[,6], col = 'lightgrey', lwd = 3)
segments(c(1:83),KC1sorted[,1],c(1:83),KC1sorted[,2], col = 'darkgrey', lwd = 3)
points(x = c(1:83), y = KC1sorted[,4],pch = 21, cex = 0.8, lwd = 2)
points(x = c(1:83), y = KC1sorted[,3],pch = 20, cex = 0.8)




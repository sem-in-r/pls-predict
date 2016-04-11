#Load our Algorithm
source("./lib/simplePLS.R")
source("./lib/PLSpredict.R")
source("./lib/predictionInterval.R")
source("./lib/validatePredict.R")
require(plspm)

#Load Data
data(russett)

#Create the Matrix of the Structural Model
smMatrix <- matrix(c("AGRIN", "POLINS",
                     "INDEV","POLINS"),nrow=2,ncol=2,byrow =TRUE,
                   dimnames = list(1:2,c("source","target")))

#Create the Matrix of the Measurement Model
mmMatrix <- matrix(c("AGRIN","farm","R",
                     "AGRIN","gini","R",
                     "AGRIN","rent","R",
                     "INDEV","gnpr","R",
                     "INDEV","labo","R",
                     "POLINS","death","R",
                     "POLINS","demostab","R",
                     "POLINS","dictator","R",
                     "POLINS","ecks","R",
                     "POLINS","inst","R",
                     "POLINS","demoinst","R"),nrow=11,ncol=3,byrow =TRUE,
                   dimnames = list(1:11,c("latent","measurement","type")))

#Call PLS-PM Function
plsModel<-simplePLS(russett,smMatrix,mmMatrix,300,7)

#Call Prediction Function 
predTrain <- PLSpredict(russett, russett, smMatrix, mmMatrix, 300,9)

# Prediction Metrics
predictionMetrics <- validatePredict(russett, smMatrix, mmMatrix,noFolds=10)
predictionMetrics$totalRMSE
predictionMetrics$totalMAPE
predictionMetrics$totalMAD




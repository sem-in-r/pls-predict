#PLSpredict
#Description: This library contains the PLSpredict function to perform prediction on PLS path models

#Function that receives a model and predicts measurements
PLSpredict <- function(model, testData, technique = predict_DA){
  library(seminr)
  source(file = "lib/library.R")
  #Call simplePLS function
  #plsModel <- estimate_pls(trainData, mmMatrix, interactions = NULL, smMatrix)

  #Get results from training model
  smMatrix <- model$smMatrix
  mmMatrix <- model$mmMatrix
  ltVariables <- model$ltVariables
  mmVariables <- model$mmVariables
  outer_weights <- model$outer_weights
  outer_loadings <- model$outer_loadings
  meanData <- model$meanData
  sdData <- model$sdData
  path_coef<- model$path_coef

  #Create container for Exogenous Variables
  exVariables = NULL

  #Create container for Endogenous Variables
  enVariables = NULL

  #Identify Exogenous and Endogenous Variables
  exVariables <- unique(smMatrix[,1])
  pMeasurements <- NULL
  for (i in 1:length(exVariables)){
    pMeasurements <- c(pMeasurements,mmMatrix[mmMatrix[,"construct"]==exVariables[i],"measurement"])
  }
  enVariables <- unique(smMatrix[,2])
  resMeasurements <- NULL
  for (i in 1:length(enVariables)){
    resMeasurements <- c(resMeasurements, mmMatrix[mmMatrix[, "construct"] == enVariables[i],"measurement"])
  }
  enVariables <- setdiff(enVariables,exVariables)
  eMeasurements <- NULL
  for (i in 1:length(enVariables)){
    eMeasurements <- c(eMeasurements, mmMatrix[mmMatrix[, "construct"] == enVariables[i],"measurement"])
  }

  #Extract Measurements needed for Predictions
  normData <- testData[,pMeasurements]

  #Normalize data
  for (i in pMeasurements)
  {
    normData[,i] <-(normData[,i] - meanData[i])/sdData[i]
  }

  #Convert dataset to matrix
  normData<-data.matrix(normData)

   #Add empty columns to normData for the estimated measurements
  for (i in 1:length(eMeasurements))
  {
    normData = cbind(normData, seq(0,0,length.out =nrow(normData)))
    colnames(normData)[length(colnames(normData))]=eMeasurements[i]
  }

  #Estimate Factor Scores from Outter Path
  fscores <- normData%*%outer_weights

  #Estimate Factor Scores from Inner Path and complete Matrix
  fscores <- technique(smMatrix, path_coef, fscores)

  #Predict Measurements with loadings
  predictedMeasurements<-fscores%*% t(outer_loadings)

  #Denormalize data
  for (i in mmVariables)
  {
    predictedMeasurements[,i]<-(predictedMeasurements[,i] * sdData[i])+meanData[i]
  }

  #Calculating the residuals
  residuals <- testData[,resMeasurements] - predictedMeasurements[,resMeasurements]

  #Prepare return Object
  predictResults <- list(testData = testData[,resMeasurements],
                         predicted_Measurements = predictedMeasurements[,resMeasurements],
                         residuals = residuals,
                         predicted_CompositeScores = fscores)

  class(predictResults) <- "predictResults"
  return(predictResults)
}

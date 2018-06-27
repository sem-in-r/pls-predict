#predictionInterval
#Description: This library contains the function utilized to generate a prediction Interval 
# for a PLSpredict prediction.

require(TeachingDemos)

#Function for generating average case and casewise Prediction Intervals
predictionInterval <- function(model, testData, technique = predict_DA, PIprobs = 0.9,noBoots=200){
  
  # Collect model specs
  trainData <- model$data
  smMatrix <- model$smMatrix
  mmMatrix <- model$mmMatrix
  
  # AVERAGE CASE PREDICTION INTERVAL
  # initialize output prediction dataframe
  # initialize output residual dataframe
  # TODO: get output factors (don't assume they are reflective)
  tempPredict <- as.data.frame(matrix(ncol=0, nrow=nrow(testData)))
  tempResidual <- as.data.frame(matrix(ncol=0, nrow=nrow(testData)))
  tempHolder <- as.data.frame(matrix(ncol=0, nrow=nrow(testData)))
  tempTotal <- as.data.frame(matrix(ncol=0, nrow=nrow(testData)))
  
  #Identify target variables
  #uniqueTarget <- unique(smMatrix[,2])
  #items <- mmMatrix[mmMatrix[, "latent"] == uniqueTarget,2]
  uniqueTarget <- unique(smMatrix[,2])
  items <- NULL
  for (i in 1:length(uniqueTarget)){
    items <- c(items, mmMatrix[mmMatrix[, "construct"] == uniqueTarget[i],"measurement"])
  }
  
  #Bootstrap
  #TODO: parallelize bootstrap
  for (i in 1:noBoots) { 
    boot.index <- sort(sample(1:nrow(trainData), replace=TRUE))
    trainData.boot <- trainData[boot.index,] 
    
    #Call PLSpredict
    utils::capture.output(trainModel <- estimate_pls(trainData.boot,
                               measurement_model = mmMatrix,
                               structural_model = smMatrix))
    tempModel <- PLSpredict(model = trainModel,
                            testData = testData,
                            technique = technique)
    tempPredict <- cbind(tempPredict,data.frame(tempModel$predicted_Measurements))
    tempResidual <- cbind(tempResidual,data.frame(tempModel$residuals))
  }
  
  
  
  # Initialize Average Case PI holder
  quantHolder <- list(NULL)
  
  # Calculate Quantiles HPD
  for (n in 1:length(items)) {
    quantHolder[[n]] <- data.frame(apply(tempPredict[,colnames(tempPredict)==items[n]] , 1, emp.hpd, conf = PIprobs))
  }
  ##quantHolder <- matrix(as.matrix(quantHolder), ncol = ncol(quantHolder), dimnames = NULL)
  
  ## CHECK OF VALUES IN INTERVAL ##
  ##origVal <- tempModel$testData$AA.0[i]
  ##findInterval(origVal, quantHolder[,i])
  
  ##withinInterval = c()
  ##for (i in 1:183) {
  ##  origVal <- tempModel$testData$AA.0[i]
  ##  withinInterval[i] = (findInterval(origVal, quantHolder[,i]) == 1)
  ##}
  ## END CHECK
  
  # Initialize Casewise PI holder
  casewiseHolder <- list(NULL)
  
  # Randomly shuffle indexes for random error retrieval
  # Columnwise, shuffle the tempResiduals rows
  for (q in 1:ncol(tempResidual)) {
    index <- sample.int(dim(testData),replace=F)
    tempHolder[,q] <- tempResidual[index,q]
  }
  
  # Add the predicted values and random error (residuals)
  tempTotal <- tempHolder + tempPredict
  names(tempTotal) <- names(tempResidual)
  
  # and calculate quantiles HPD on 
  for (n in 1:length(items)) {
    casewiseHolder[[n]] <- data.frame(apply(tempTotal[,colnames(tempTotal)==items[n]] , 1, emp.hpd, conf = PIprobs))
  }
  
  names(casewiseHolder) <- names(quantHolder) <- colnames(tempModel$predicted_Measurements)
  
  PIresults <- list(averageCasePI = quantHolder, 
                    caseWisePI = casewiseHolder)
  return(PIresults)
}

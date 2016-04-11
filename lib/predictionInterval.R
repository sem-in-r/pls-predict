#predictionInterval
#Description: This library contains the function utilized to generate a prediction Interval 
# for a PLSpredict prediction.

require(TeachingDemos)

#Function for generating average case and casewise Prediction Intervals
predictionInterval <- function(trainData, smMatrix, mmMatrix, PIprobs = 0.9, maxIt=300, stopCriterion=7,noBoots=200, testData = trainData){
  
  # AVERAGE CASE PREDICTION INTERVAL
  # initialize output prediction dataframe
  # initialize output residual dataframe
  # TODO: get output factors (don't assume they are reflective)
  tempPredict <- as.data.frame(matrix(ncol=0, nrow=nrow(trainData)))
  tempResidual <- as.data.frame(matrix(ncol=0, nrow=nrow(trainData)))
  tempHolder <- as.data.frame(matrix(ncol=0, nrow=nrow(trainData)))
  tempTotal <- as.data.frame(matrix(ncol=0, nrow=nrow(trainData)))
  
  #Identify target variables
  #uniqueTarget <- unique(smMatrix[,2])
  #items <- mmMatrix[mmMatrix[, "latent"] == uniqueTarget,2]
  uniqueTarget <- unique(smMatrix[,2])
  items <- NULL
  for (i in 1:length(uniqueTarget)){
    items <- c(items, mmMatrix[mmMatrix[, "latent"] == uniqueTarget[i],"measurement"])
  }
  
  #Bootstrap
  for (i in 1:noBoots) { 
    boot.index <- sort(sample(1:nrow(testData), replace=TRUE))
    testData.boot <- testData[boot.index,] 
    
    #Call PLSpredict
    tempModel <- PLSpredict(testData.boot,trainData,smMatrix, mmMatrix, maxIt, stopCriterion)
    tempPredict <- cbind(tempPredict,data.frame(tempModel$predictedMeasurements))
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
  
  PIresults <- list(averageCasePI = quantHolder, 
                    caseWisePI = casewiseHolder)
  return(PIresults)
}

# validatePredict
# Description: This library contains the function utilized to generate perform a k-fold validation 
# and subsequent calculation of prediction metrics (RMSE, MAPE, MAD) for PLS & LM

#Function for Generating and Evaluating Out-of-sample predictions using 10-fold cross-validation
validatePredict <- function(testData, smMatrix, mmMatrix, maxIt=300, stopCriterion=7,noFolds=10){
  
  #Randomly shuffle the data
  testData <- testData[sample(nrow(testData)),]
  
  #Create 10 equally size folds
  folds <- cut(seq(1,nrow(testData)),breaks=noFolds,labels=FALSE)
  
  #Identify variables to be tested
  uniqueTarget <- unique(smMatrix[,2])
  items <- NULL
  for (i in 1:length(uniqueTarget)){
    items <- c(items, mmMatrix[mmMatrix[, "latent"] == uniqueTarget[i],"measurement"])
  }
  uniqueSource <- unique(smMatrix[,1])
  sources <- NULL
  for (i in 1:length(uniqueSource)){
    sources <- c(sources,mmMatrix[mmMatrix[,"latent"]==uniqueSource[i],"measurement"])
  }
  
  # Initialize matrices for prediction metrics
  # Initialize RMSE holders
  PLSRMSE <- matrix(,nrow=1,ncol=length(items),byrow =TRUE,dimnames = list(1,items))
  PLSSSE <- matrix(,nrow=noFolds,ncol=length(items),byrow =TRUE,dimnames = list(1:noFolds,items))
  LMRMSE <- matrix(,nrow=1,ncol=length(items),byrow =TRUE,dimnames = list(1,items))
  LMSSSE <- matrix(,nrow=noFolds,ncol=length(items),byrow =TRUE,dimnames = list(1:noFolds,items))
  # Initialize predMAPE
  PLSSAPE <- matrix(,nrow=noFolds,ncol=length(items),byrow =TRUE,dimnames = list(1:noFolds,items))
  PLSMAPE <- matrix(,nrow=1,ncol=length(items),byrow =TRUE,dimnames = list(1,items))
  LMMAPE <- matrix(,nrow=1,ncol=length(items),byrow =TRUE,dimnames = list(1,items))
  LMSAPE <- matrix(,nrow=noFolds,ncol=length(items),byrow =TRUE,dimnames = list(1:noFolds,items))
  # Initialize predMAD
  PLSSAD <- matrix(,nrow=noFolds,ncol=length(items),byrow =TRUE,dimnames = list(1:noFolds,items))
  PLSMAD <- matrix(,nrow=1,ncol=length(items),byrow =TRUE,dimnames = list(1,items))
  LMMAD <- matrix(,nrow=1,ncol=length(items),byrow =TRUE,dimnames = list(1,items))
  LMSAD <- matrix(,nrow=noFolds,ncol=length(items),byrow =TRUE,dimnames = list(1:noFolds,items))
  
  # Extract the target and non-target variables for Linear Model
  independentMatrix <- testData[,sources]
  dependentMatrix <- testData[,items]
  
  #Perform 10 fold cross validation
  for(i in 1:noFolds){
    #Segment your data by fold using the which() function 
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testingData <- testData[testIndexes, ]
    trainingData <- testData[-testIndexes, ]
    indepTestData <- independentMatrix[testIndexes, ]
    indepTrainData <- independentMatrix[-testIndexes, ]
    depTestData <- dependentMatrix[testIndexes, ]
    depTrainData <- dependentMatrix[-testIndexes, ]
    
    #PLS model
    testHolder <- PLSpredict(trainingData, testingData ,smMatrix, mmMatrix, maxIt, stopCriterion)
    
    #Initialize lm residuals and actuals holder matrices
    lmprediction <- matrix(,nrow=nrow(depTestData),ncol=length(items),byrow =TRUE,dimnames = list(1:nrow(depTestData),items))
    lmresidual <- matrix(,nrow=nrow(depTestData),ncol=length(items),byrow =TRUE,dimnames = list(1:nrow(depTestData),items))
    lmactual <- matrix(,nrow=nrow(depTestData),ncol=length(items),byrow =TRUE,dimnames = list(1:nrow(depTestData),items))
    
    #LM Models
    for(l in 1:length(items)){
      trainLM <- lm(depTrainData[,l] ~ ., indepTrainData)
      lmprediction[,l] <- predict(trainLM, newdata = indepTestData)
      lmresidual[,l] <- lmprediction[,l] - depTestData[, l]
      lmactual[,l] <- depTestData[, l]
    }
    
    #Iterate over no of residuals columns
    for(j in 1:ncol(testHolder$residuals)){
      
      #Calculate SMSE
      PLSSSE[i,j] <- sum(testHolder$residuals[,j]^2) 
      LMSSSE[i,j] <- sum(lmresidual[,j]^2)
      #Calculate SAPE
      PLSSAPE[i,j] <- sum((abs(testHolder$residuals[,j]/testHolder$testData[,j])))
      #PLSSAPE[i,j] <- sum((abs((mean(testHolder$testData[,j]) - testHolder$predictedMeasurements[,j])/mean(testHolder$testData[,j]))))
      #PLSSAPE[i,j] <- sum((abs((testHolder$residuals[,j])/mean(testHolder$testData[,j]))))
      LMSAPE[i,j] <- sum((abs(lmresidual[,j]/lmactual[,j])))
      #Calculate SAD
      PLSSAD[i,j] <- sum(abs(testHolder$residuals[,j] - mean(testHolder$residuals[,j])))
      LMSAD[i,j] <- sum(abs(lmresidual[,j] - mean(lmresidual[,j])))
    }
  }
  
  #Final calculations 
  denom <- noFolds * nrow(testHolder$residuals)
  for (k in 1:length(items)) {
    LMRMSE[,k] <- sqrt((sum(LMSSSE[,k]))/denom)
    PLSRMSE[,k] <- sqrt((sum(PLSSSE[,k]))/denom)
    LMMAPE[,k] <- 100*(sum(LMSAPE[,k])/denom)
    PLSMAPE[,k] <- 100*(sum(PLSSAPE[,k])/denom)
    LMMAD[,k] <- sum(LMSAD[,k])/denom
    PLSMAD[,k] <- sum(PLSSAD[,k])/denom
  }
  
  
  validateResults <- list(PLSRMSE = PLSRMSE, 
                          PLSMAPE = PLSMAPE,
                          PLSMAD = PLSMAD,
                          LMRMSE = LMRMSE,
                          LMMAPE = LMMAPE,
                          LMMAD = LMMAD)
  return(validateResults)
  
}
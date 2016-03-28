#Function for Generating and Evaluating Out-of-sample predictions using 10-fold cross-validaiton
validatePredict <- function(testData, smMatrix, mmMatrix, maxIt=300, stopCriterion=7,noFolds=10){
  
  #Randomly shuffle the data
  testData <- testData[sample(nrow(testData)),]
  
  #Create 10 equally size folds
  folds <- cut(seq(1,nrow(testData)),breaks=noFolds,labels=FALSE)
  
  #Identify variables to be tested
  uniqueTarget <- unique(smMatrix[,2])
  items <- mmMatrix[mmMatrix[, "latent"] == uniqueTarget,2]
  
  # Initialize matrices for prediction metrics
  # Initialize RMSE holders
  totalRMSE <- matrix(,nrow=1,ncol=length(items),byrow =TRUE,dimnames = list(1,items))
  predSSE <- matrix(,nrow=noFolds,ncol=length(items),byrow =TRUE,dimnames = list(1:noFolds,items))
  # Initialize predMAPE
  predSAPE <- matrix(,nrow=noFolds,ncol=length(items),byrow =TRUE,dimnames = list(1:noFolds,items))
  totalMAPE <- matrix(,nrow=1,ncol=length(items),byrow =TRUE,dimnames = list(1,items))
  # Initialize predMAD
  predSAD <- matrix(,nrow=noFolds,ncol=length(items),byrow =TRUE,dimnames = list(1:noFolds,items))
  totalMAD <- matrix(,nrow=1,ncol=length(items),byrow =TRUE,dimnames = list(1,items))
  
  
  #Perform 10 fold cross validation
  for(i in 1:noFolds){
    #Segment your data by fold using the which() function 
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testingData <- testData[testIndexes, ]
    trainingData <- testData[-testIndexes, ]
    
    #PLS model
    testHolder <- PLSpredict(trainingData, testingData ,smMatrix, mmMatrix, maxIt, stopCriterion)
    
    #Iterate over no of residuals columns
    for(j in 1:ncol(testHolder$residuals)){
      
      #Calculate SMSE
      predSSE[i,j] <- sum(testHolder$residuals[,j]^2) 
      
      #Calculate SAPE
      predSAPE[i,j] <- sum((abs(testHolder$residuals[,j]/testHolder$testData[,j])))
      #predSAPE[i,j] <- sum((abs((testHolder$testData[,j] - testHolder$predictedMeasurements[,j])/testHolder$testData[,j])))
      #predSAPE[i,j] <- sum((abs((testHolder$testData[,j] - testHolder$predictedMeasurements[,j])/mean(testHolder$testData[,j]))))
      
      #Calculate SAD
      predSAD[i,j] <- sum(abs(testHolder$residuals[,j] - mean(testHolder$residuals[,j])))
    }
  }
  
  #Final calculations 
  denom <- noFolds * nrow(testHolder$residuals)
  for (k in 1:length(items)) {
    totalRMSE[,k] <- sqrt((sum(predSSE[,k]))/denom)
    totalMAPE[,k] <- 100*(sum(predSAPE[,k])/denom)
    totalMAD[,k] <- sum(predSAD[,k])/denom
  }
  
  validateResults <- list(totalRMSE = totalRMSE, 
                          totalMAPE = totalMAPE,
                          totalMAD = totalMAD)
  return(validateResults)
  
}
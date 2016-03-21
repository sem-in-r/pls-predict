#Function for Generating and Evaluating Out-of-sample predictions using 10-fold cross-validaiton
validatePredict <- function(newData, smMatrix, mmMatrix, maxIt=300, stopCriterion=7,noFolds=10){
  
  #Randomly shuffle the data
  newData <- newData[sample(nrow(newData)),]
  
  #Create 10 equally size folds
  folds <- cut(seq(1,nrow(newData)),breaks=noFolds,labels=FALSE)
  
  #Identify variables to be tested
  items <- mmMatrix[ which(mmMatrix[,3]=='R'), "measurement" ]
  
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
    testData <- newData[testIndexes, ]
    trainData <- newData[-testIndexes, ]
    
    #PLS model
    testHolder <- PLSpredict(trainData,smMatrix, mmMatrix, maxIt, stopCriterion, testData)
    
    #Iterate over no of residuals columns
    for(j in 1:ncol(testHolder$residuals)){
      
      #Calculate RMSE
      predSSE[i,j] <- sum(testHolder$residuals[,j]^2) 
      
      #Calculate MAPE
      predSAPE[i,j] <- sum((abs((testHolder$newData[,j] - testHolder$predictedMeasurements[,j])/testHolder$newData[,j])))
      
      #Calculate MAD
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
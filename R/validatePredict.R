# validatePredict
# Description: This library contains the function utilized to generate perform a k-fold validation 
# and subsequent calculation of prediction metrics (RMSE, MAPE, MAD) for PLS & LM

#Function for Generating and Evaluating Out-of-sample predictions using 10-fold cross-validation
validatePredict <- function(model, technique = predict_DA, noFolds=10){
  
  # Collect the objects from the model
  fullData <- model$data
  smMatrix <- model$smMatrix
  mmMatrix <- model$mmMatrix
  
  #Randomly shuffle the data
  fullData <- fullData[sample(nrow(fullData)),]
  
  #Create 10 equally size folds
  folds <- cut(seq(1,nrow(fullData)),breaks=noFolds,labels=FALSE)
  
  #Identify variables to be tested
  uniqueTarget <- unique(smMatrix[,2])
  items <- NULL
  for (i in 1:length(uniqueTarget)){
    items <- c(items, mmMatrix[mmMatrix[, "construct"] == uniqueTarget[i],"measurement"])
  }
  uniqueSource <- unique(smMatrix[,1])
  sources <- NULL
  for (i in 1:length(uniqueSource)){
    sources <- c(sources,mmMatrix[mmMatrix[,"construct"]==uniqueSource[i],"measurement"])
  }
  ##lmtarget <- ifelse(length(intersect(uniqueTarget, uniqueSource)) == 0, uniqueTarget,setdiff(uniqueTarget, uniqueSource))  
  targets <- NULL
  for (i in uniqueTarget){
    targets <- c(targets, mmMatrix[mmMatrix[, "construct"] == i,"measurement"])
  }
  
  
  # Initialize matrices for prediction metrics
  # Initialize RMSE holders
  PLSRMSE <- matrix(0,nrow=1,ncol=length(targets),byrow =TRUE,dimnames = list(1,targets))
  PLSSSE <- matrix(0,nrow=noFolds,ncol=length(targets),byrow =TRUE,dimnames = list(1:noFolds,targets))
  LMRMSE <- matrix(0,nrow=1,ncol=length(targets),byrow =TRUE,dimnames = list(1,targets))
  LMSSSE <- matrix(0,nrow=noFolds,ncol=length(targets),byrow =TRUE,dimnames = list(1:noFolds,targets))
  # Initialize predMAPE
  PLSSAPE <- matrix(0,nrow=noFolds,ncol=length(targets),byrow =TRUE,dimnames = list(1:noFolds,targets))
  PLSMAPE <- matrix(0,nrow=1,ncol=length(targets),byrow =TRUE,dimnames = list(1,targets))
  LMMAPE <- matrix(0,nrow=1,ncol=length(targets),byrow =TRUE,dimnames = list(1,targets))
  LMSAPE <- matrix(0,nrow=noFolds,ncol=length(targets),byrow =TRUE,dimnames = list(1:noFolds,targets))
  # Initialize predMAD
  PLSSAD <- matrix(0,nrow=noFolds,ncol=length(targets),byrow =TRUE,dimnames = list(1:noFolds,targets))
  PLSMAD <- matrix(0,nrow=1,ncol=length(targets),byrow =TRUE,dimnames = list(1,targets))
  LMMAD <- matrix(0,nrow=1,ncol=length(targets),byrow =TRUE,dimnames = list(1,targets))
  LMSAD <- matrix(0,nrow=noFolds,ncol=length(targets),byrow =TRUE,dimnames = list(1:noFolds,targets))
  
   #Perform 10 fold cross validation
  for(i in 1:noFolds){
    #Segment your data by fold 
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testingData <- fullData[testIndexes, ]
    trainingData <- fullData[-testIndexes, ]
    
    # Train PLS model
    utils::capture.output(train_pls_model <- estimate_pls(data = trainingData,
                                    measurement_model = mmMatrix,
                                    structural_model = smMatrix))
    testHolder <- PLSpredict(train_pls_model,
                             technique = technique,
                             testData = testingData)
    
    #Initialize PLS residuals and actuals holder matrices
    PLSactuals <- testHolder$testData[,targets]
    PLSresiduals <- testHolder$residuals[,targets]
    
    #Initialize lm residuals and actuals holder matrices
    lmprediction <- matrix(0,nrow=length(testIndexes),ncol=length(targets),byrow =TRUE,dimnames = list(1:length(testIndexes),targets))
    lmresidual <- matrix(0,nrow=length(testIndexes),ncol=length(targets),byrow =TRUE,dimnames = list(1:length(testIndexes),targets))
    lmactual <- matrix(0,nrow=length(testIndexes),ncol=length(targets),byrow =TRUE,dimnames = list(1:length(testIndexes),targets))
    
    #LM Models
    for(l in uniqueTarget){
      # Extract the target and non-target variables for Linear Model
      lmtargets <- mmMatrix[mmMatrix[,1] == l,2]
      for (item in lmtargets) {
        independentMatrix <- fullData[ , -which(names(fullData) %in% lmtargets)]
        dependentMatrix <- as.matrix(fullData[,lmtargets])
        indepTestData <- independentMatrix[testIndexes, ]
        indepTrainData <- independentMatrix[-testIndexes, ]
        depTestData <- as.matrix(dependentMatrix[testIndexes, ])
        depTrainData <- as.matrix(dependentMatrix[-testIndexes, ])
        colnames(depTrainData) <- colnames(depTestData) <- lmtargets
        
        trainLM <- stats::lm(depTrainData[,item] ~ ., indepTrainData)
        lmprediction[,item] <- stats::predict(trainLM, newdata = indepTestData)
        lmresidual[,item] <- depTestData[, item] - lmprediction[,item] 
        lmactual[,item] <- depTestData[, item]
      }
    }
    
    #Iterate over no of targets
    for(j in 1:length(targets)){
      
      #Calculate SMSE
      PLSSSE[i,j] <- sum(PLSresiduals[,j]^2) 
      LMSSSE[i,j] <- sum(lmresidual[,j]^2)
      #Calculate SAPE
      PLSSAPE[i,j] <- sum((abs(PLSresiduals[,j]/PLSactuals[,j])))
      #PLSSAPE[i,j] <- sum((abs((mean(testHolder$testData[,j]) - testHolder$predictedMeasurements[,j])/mean(testHolder$testData[,j]))))
      #PLSSAPE[i,j] <- sum((abs((testHolder$residuals[,j])/mean(testHolder$testData[,j]))))
      LMSAPE[i,j] <- sum((abs(lmresidual[,j]/lmactual[,j])))
      #Calculate SAD
      PLSSAD[i,j] <- sum(abs(PLSresiduals[,j] - mean(PLSresiduals[,j])))
      LMSAD[i,j] <- sum(abs(lmresidual[,j] - mean(lmresidual[,j])))
    }
  }
  
  #Final calculations 
  denom <- noFolds * nrow(testingData)
  for (k in 1:length(targets)) {
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

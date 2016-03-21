#simplePLS Library
#Author: Juan Manuel Velasquez Estrada
#Creation: March 2015
#Description: This library contains the functions utilized to run the PLS-PM 
# algorithm and its predictions.

#Function that estimates the PLS-PM Model
simplePLS <- function(obsData,smMatrix, mmMatrix, maxIt=300, stopCriterion=7){
  
  #Create list of Measurements Variables
  mmVariables <- mmMatrix[,"measurement"]
  
  #Create list of Latent Variables
  ltVariables <- unique(c(smMatrix[,1],smMatrix[,2]))

  #Extract and Normalize the measurements for the model
  normData <- scale(obsData[,mmVariables],TRUE,TRUE)

  #Extract Mean and Standard Deviation of measurements for future prediction
  meanData <- attr(normData, "scaled:center")
  sdData <- attr(normData, "scaled:scale")
  
  #Create a matrix of outer_weights
  outer_weights <- matrix(data=0,
                          nrow=length(mmVariables),
                          ncol=length(ltVariables),
                          dimnames = list(mmVariables,ltVariables))
  
  #Initialize outer_weights matrix with value 1 for each relationship in the measurement model
  for (i in 1:length(ltVariables))  {
    outer_weights [mmMatrix[mmMatrix[,"latent"]==ltVariables[i],
                            "measurement"],
                   ltVariables[i]] =1
  }
  
  #Create a matrix of inner paths
  #? inner_paths => inner_weights?
  inner_paths <- matrix(data=0,
                        nrow=length(ltVariables),
                        ncol=length(ltVariables),
                        dimnames = list(ltVariables,ltVariables))
  
  #Iterative Process Starts here
  for (iterations in 0:maxIt)  {
    
    #Estimate Factor Scores from Outter Path
    #? fscores <- normData%*%outer_weights
    fscores <- normData[,mmVariables]%*%outer_weights
    
    #Standarize Factor Scores
    fscores <- scale(fscores,TRUE,TRUE)
    
    #Estimate inner paths (symmetric matrix)
    for (i in 1:nrow(smMatrix))  {
      inner_paths[smMatrix[i,"source"],
                  smMatrix[i,"target"]] = cov(fscores[,smMatrix[i,"source"]],
                                              fscores[,smMatrix[i,"target"]])
      #? next step necessary?
      inner_paths[smMatrix[i,"target"],
                  smMatrix[i,"source"]] = cov(fscores[,smMatrix[i,"source"]],
                                              fscores[,smMatrix[i,"target"]])
    }
    
    #Estimate Factor Scores from Inner Path
    fscores<-fscores%*%inner_paths
    
    #Standarize Factor Scores
    fscores <- scale(fscores,TRUE,TRUE)
    
    #Save last outer_weights
    last_outer_weights <- outer_weights
    
    #Update outer_weights
    for (i in 1:length(ltVariables))  {
      
      #If the measurement model is Formative
      if(mmMatrix[mmMatrix[,"latent"]==ltVariables[i],"type"][1]=="F"){
        outer_weights[mmMatrix[mmMatrix[,"latent"]==ltVariables[i], "measurement"], ltVariables[i]] = 
          solve(cor(normData[,mmMatrix[mmMatrix[,"latent"]==ltVariables[i],"measurement"]])) %*%
                  cor(normData[,mmMatrix[mmMatrix[,"latent"]==ltVariables[i],"measurement"]],
                fscores[,ltVariables[i]])
      }
      
      #If the measurement model is Reflective
      if(mmMatrix[mmMatrix[,"latent"]==ltVariables[i],"type"][1]=="R"){
        outer_weights[mmMatrix[mmMatrix[,"latent"]==ltVariables[i], "measurement"], ltVariables[i]] = 
          cov(normData[,mmMatrix[mmMatrix[,"latent"]==ltVariables[i],"measurement"]],fscores[,ltVariables[i]])
      }
    }
    
    #Estimate Factor Scores from Outer Weights
    fscores <- normData[,mmVariables]%*%outer_weights
    
    #Standarize outer_weights
    for (i in 1:length(ltVariables))  {
      outer_weights [mmMatrix[mmMatrix[,"latent"]==ltVariables[i], "measurement"], ltVariables[i]] =
        outer_weights [mmMatrix[mmMatrix[,"latent"]==ltVariables[i], "measurement"], ltVariables[i]] / sd(fscores[,ltVariables[i]])
    }

    #Verify the stop criteria
    weightDiff <- sum(abs(outer_weights-last_outer_weights))
    if (weightDiff <(10^(-(stopCriterion))))
      break
    
  } #Finish Iterative Process
  
  #Estimate Factor Scores from Outter Path
  fscores <- normData[,mmVariables]%*%outer_weights
  
  
  #Initialize Matrix of Path Coefficients
  path_coef <- matrix(data=0,
                      nrow=length(ltVariables),
                      ncol=length(ltVariables),
                      dimnames = list(ltVariables,ltVariables))
  
  #Identify which variables have incoming paths
  dependant<-unique(smMatrix[,"target"])
  
  #We calculate a linear regresion for each dependant variable
  for (i in 1:length(dependant))  {
    
    #Indentify the independant variables
    independant<-smMatrix[smMatrix[,"target"]==dependant[i],"source"]
    
    #Solve the sistem of equations
    results<- solve(cor(fscores[,independant, drop=FALSE]),
                    cor(fscores[,independant], fscores[,dependant[i]]))
    
    #Transform to a generic vector
    coefficients <- as.vector(results)
    if(!is.null(rownames(results)))
      names(coefficients)<-rownames(results)
    else
      names(coefficients)<-names(results)
    
    #Assign the Beta Values to the Path Coefficient Matrix
    for (j in 1:length(independant))  
      path_coef[independant[j],dependant[i]]=coefficients[independant[j]]
    
  }
  
  #Create a matrix of Outer Loadings
  outer_loadings <- matrix(data=0,
                           nrow=length(mmVariables),
                           ncol=length(ltVariables),
                           dimnames = list(mmVariables,ltVariables))
  
  
  #Calculate the Outer Loadings
  for (i in 1:length(ltVariables))  {
    outer_loadings [mmMatrix[mmMatrix[,"latent"]==ltVariables[i],
                             "measurement"],
                    ltVariables[i]] = cov(normData[,mmMatrix[mmMatrix[,"latent"]==ltVariables[i],"measurement"]],fscores[,ltVariables[i]])
    
  }

  #Calculate R Squared
  
  #Get smMatrix
  modelMatrix <- data.frame(smMatrix)
  
  #Get endogenous composites
  uniquetarget <- as.character(unique(modelMatrix$target)) 
  
  #Get composite scores
  valuesMatrix <- fscores
  
  #Calculate Linear Models
  lmmodels <- lapply(uniquetarget, function(x) {lm(as.formula(paste(x,"~ .", sep = "")), 
                                                   data = data.frame(valuesMatrix[,colnames(valuesMatrix) %in% 
                                                                                    c(x,as.character(modelMatrix$source[which(modelMatrix$target==x)]))]))})
  
  #Initialize matrix holder for Rsquared values
  rSquared <- matrix(,nrow=1,ncol=length(uniquetarget),byrow =TRUE,dimnames = list(1,uniquetarget))
  
  # Iterate and extract every R^2 value 
  for (i in 1:length(lmmodels)) {
    rSquared[,i] <- summary(lmmodels[[i]])$r.squared
  }
  
    
  #Prepare return Object
  plsModel <- list(meanData = meanData,
                   sdData = sdData,
                   smMatrix = smMatrix,
                   mmMatrix = mmMatrix,
                   ltVariables = ltVariables,
                   mmVariables = mmVariables,
                   outer_loadings = outer_loadings,
                   outer_weights = outer_weights,
                   path_coef = path_coef,
                   iterations = iterations,
                   weightDiff = weightDiff,
                   fscores = fscores,
                   rSquared = rSquared)
  
  class(plsModel) <- "plsModel"
  return(plsModel)
}

#Function that receives a model and predicts measurements
PLSpredict <- function(obsData,smMatrix, mmMatrix, maxIt=300, stopCriterion=7, newData = obsData){
  
  #Call simplePLS function
  plsModel <- simplePLS(obsData, smMatrix, mmMatrix, maxIt, stopCriterion)
  
  #Get results from model
  smMatrix <- plsModel$smMatrix
  mmMatrix <- plsModel$mmMatrix
  ltVariables <- plsModel$ltVariables
  mmVariables <- plsModel$mmVariables
  outer_weights <- plsModel$outer_weights
  outer_loadings <- plsModel$outer_loadings
  meanData<-plsModel$meanData
  sdData <- plsModel$sdData
  path_coef<-plsModel$path_coef
  
  #Create container for Exogenous Variables
  exVariables = NULL
  
  #Create container for Endogenous Variables
  enVariables = NULL
  
  #Identify Exogenous and Endogenous Variables
  for (i in 1:length(ltVariables)){
    if (is.element(ltVariables[i], smMatrix[,"target"])==FALSE)
      exVariables <- c(exVariables, ltVariables[i])
    else
      enVariables <- c(enVariables, ltVariables[i])
  }
  
  #Create container for prediction Measurements
  pMeasurements = NULL
  
  #Identify prediction measurements
  for (i in 1:length(exVariables)){
    pMeasurements <- c(pMeasurements,mmMatrix[mmMatrix[,"latent"]==exVariables[i],"measurement"])
  }
  
  #Extract Measurements needed for Predictions
  normData <- newData[,pMeasurements]
  
  #Normalize data
  for (i in pMeasurements)
  {
    normData[,i] <-(normData[,i] - meanData[i])/sdData[i]
  }  
  
  #Convert dataset to matrix
  normData<-data.matrix(normData)
  
  #Create container for estimated measurements
  eMeasurements = NULL
  
  #Identify estimated measurements
  for (i in 1:length(enVariables)){
    eMeasurements <- c(eMeasurements,mmMatrix[mmMatrix[,"latent"]==enVariables[i],"measurement"])
  }
  
  #Add empty columns to normData for the estimated measurements
  for (i in 1:length(eMeasurements))
  {
    normData = cbind(normData, seq(0,0,length.out =nrow(normData)))
    colnames(normData)[length(colnames(normData))]=eMeasurements[i]
  }
  
  #Estimate Factor Scores from Outter Path
  fscores <- normData%*%outer_weights
  
  #Estimate Factor Scores from Inner Path and complete Matrix
  fscores <- fscores + fscores%*%path_coef
  
  #Predict Measurements with loadings
  predictedMeasurements<-fscores%*% t(outer_loadings)
  
  #Denormalize data
  for (i in mmVariables)
  {
    predictedMeasurements[,i]<-(predictedMeasurements[,i] * sdData[i])+meanData[i]
  }  
  
  #Calculating the residuals
  residuals <- newData[,eMeasurements] - predictedMeasurements[,eMeasurements]
  
  #Prepare return Object
  predictResults <- list(newData = newData[,eMeasurements],
                         predictedMeasurements = predictedMeasurements[,eMeasurements],
                         residuals = residuals,
                         compositeScores = fscores)
  
  class(predictResults) <- "predictResults"
  return(predictResults)
  
  
}

#Function that given a linear model, receives new data and uses the model to make predictions
predictlm <- function(model,newData){
  
  #Get Coefficients
  coef<-coefficients(model)
  
  #Initialize result vector with the intercept value
  result <-seq(coef["(Intercept)"],coef["(Intercept)"],length.out=nrow(newData))
  
  
  # Multiply the new values times their coefficient 
  for (i in 2:length(coef))  {
    result <- result+coef[i]*newData[names(coef)[i]]  
  }
  
  #Rename Column
  names(result)<-"result"
  
  #Return the prediction
  return (result)
}  


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
#Function for generating average case and casewise Prediction Intervals
predictionInterval <- function(obsData, PIprobs, smMatrix, mmMatrix, maxIt=300, stopCriterion=7,noBoots=200, newData = obsData){
    
  #Average Case Prediction Intervals
  tempPredict <- as.data.frame(matrix(ncol=0, nrow=nrow(obsData)))
  items <- mmMatrix[ which(mmMatrix[,3]=='R'), "measurement" ]
  #Bootstrap
  for (i in 1:noBoots) 
    { 
    boot.index <- sort(sample(1:nrow(newData), replace=TRUE))
    newData.boot <- newData[boot.index,] 
    
    #Call PLSpredict
    tempModel <- PLSpredict(newData.boot,smMatrix, mmMatrix, maxIt, stopCriterion, obsData)
    tempPredict <- cbind(tempPredict,data.frame(tempModel$predictedMeasurements))
  
    }
  # Initialize Quantiles holder
  quantHolder <- list(NULL)
  
  # Calculate Quantiles
  
  for (i in 1:length(items))
  {
    
    quantHolder[[1]] <- data.frame(apply(tempPredict[,colnames(tempPredict)==items[1]] , 1, quantile, probs = c(0.05, 0.9),  na.rm = TRUE))
  }
  
  #In each bootstrap compute n residuals for item
  
  #for each item add random error to each prediction 
}
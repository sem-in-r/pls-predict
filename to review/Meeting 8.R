#Function of the whole process
runProcess <- function(trainData,testData){
  
  #Prepare objects
  original <- list (lm = list(),
                    pls = list(),
                    nn = list())
  
  pca <- list (lm = list(),
               pls = list())
  
  #original$nn<- nn
  
  #----------------------Do PCA Analysis to produce new scenarios---------------------------------
  
  #Filter the dataset for the AA values
  Data<-trainData[,c("AA.0","AA.1","AA.2","AA.3")]
  
  #Do Principal Components Analysis
  pcaData <- principal(Data,4,rotate ="none")
  
  #Add the Principal Components to the Original Data
  trainData<-cbind(trainData,pcaData$scores)
  
  #Filter the dataset for the AA values
  Data<-testData[,c("AA.0","AA.1","AA.2","AA.3")]
  
  #Do Principal Components Analysis
  pcaData <- principal(Data,4,rotate ="none")
  
  #Add the Principal Components to the Original Data
  testData<-cbind(testData,pcaData$scores)
  
  
  #-------------------------------Begin the PLS-PM Analysis---------------------------------
  
  #PCA Analysis
  
  #Create the Matrix of the Structural Model
  smMatrix <- matrix(c("Perceived Visual Complexity", "Aproach/Avoidance",
                       "Arousal","Aproach/Avoidance"),nrow=2,ncol=2,byrow =TRUE,
                     dimnames = list(1:2,c("source","target")))
  
  #Create the Matrix of the Measurement Model
  mmMatrix <- matrix(c("Perceived Visual Complexity","VX.0","F",
                       "Perceived Visual Complexity","VX.1","F",
                       "Perceived Visual Complexity","VX.2","F",
                       "Perceived Visual Complexity","VX.3","F",
                       "Perceived Visual Complexity","VX.4","F",
                       "Arousal","Aro1","F",
                       "Arousal","Aro2","F",
                       "Arousal","Aro3","F",
                       "Arousal","Aro4","F",
                       "Aproach/Avoidance","PC1","R",
                       "Aproach/Avoidance","PC2","R",
                       "Aproach/Avoidance","PC3","R",
                       "Aproach/Avoidance","PC4","R"),nrow=13,ncol=3,byrow =TRUE,
                     dimnames = list(1:13,c("latent","measurement","type")))
  
  
  #Call PLS-PM Function
  plsModel<-simplePLS(trainData,smMatrix,mmMatrix)
  
  #Call Prediction Function
  predTrain <- PLSpredict(plsModel,trainData)
  predTest <- PLSpredict(plsModel,testData)
  
  #Get Residuals
  residualsTraining <- predTrain$residuals
  residualsTest <- predTest$residuals
  
  #Prepare Object
  pls <- list(predTrain = predTrain,
              predTest = predTest,
              residualsTraining = residualsTraining,
              residualsTest= residualsTest)
  
  #Assign to List
  pca$pls = pls
  
  #Original Data
  
  #Create the Matrix of the Structural Model
  smMatrix <- matrix(c("Perceived Visual Complexity", "Aproach/Avoidance",
                       "Arousal","Aproach/Avoidance"),nrow=2,ncol=2,byrow =TRUE,
                     dimnames = list(1:2,c("source","target")))
  
  #Create the Matrix of the Measurement Model
  mmMatrix <- matrix(c("Perceived Visual Complexity","VX.0","F",
                       "Perceived Visual Complexity","VX.1","F",
                       "Perceived Visual Complexity","VX.2","F",
                       "Perceived Visual Complexity","VX.3","F",
                       "Perceived Visual Complexity","VX.4","F",
                       "Arousal","Aro1","F",
                       "Arousal","Aro2","F",
                       "Arousal","Aro3","F",
                       "Arousal","Aro4","F",
                       "Aproach/Avoidance","AA.0","R",
                       "Aproach/Avoidance","AA.1","R",
                       "Aproach/Avoidance","AA.2","R",
                       "Aproach/Avoidance","AA.3","R"),nrow=13,ncol=3,byrow =TRUE,
                     dimnames = list(1:13,c("latent","measurement","type")))
  
  #Call PLS-PM Function
  plsModel<-simplePLS(trainData,smMatrix,mmMatrix)
  
  #Call Prediction Function
  predTrain <- PLSpredict(plsModel,trainData)
  predTest <- PLSpredict(plsModel,testData)
  
  #Get Residuals
  residualsTraining <- predTrain$residuals
  residualsTest <- predTest$residuals
  
  #Prepare Object
  pls <- list(predTrain = predTrain,
              predTest = predTest,
              residualsTraining = residualsTraining,
              residualsTest= residualsTest)
  
  #Assign to List
  original$pls<-pls
  
  #--------------------------------------Begin LM Analysis---------------------------------
  
  #PCA
  
  #Multiple LInear Regeresion for each output variable
  lmAA0 <- with(trainData, lm(PC1 ~ VX.0+VX.1+VX.2+VX.3+VX.4+Aro1+Aro2+Aro3+Aro4))
  lmAA1 <- with(trainData, lm(PC2 ~ VX.0+VX.1+VX.2+VX.3+VX.4+Aro1+Aro2+Aro3+Aro4))
  lmAA2 <- with(trainData, lm(PC3 ~ VX.0+VX.1+VX.2+VX.3+VX.4+Aro1+Aro2+Aro3+Aro4))
  lmAA3 <- with(trainData, lm(PC4 ~ VX.0+VX.1+VX.2+VX.3+VX.4+Aro1+Aro2+Aro3+Aro4))
  
  #Use our custom function to predict each value for training data
  predTrainAA0 <- predictlm (lmAA0,trainData)
  predTrainAA1 <- predictlm (lmAA1,trainData)
  predTrainAA2 <- predictlm (lmAA2,trainData)
  predTrainAA3 <- predictlm (lmAA3,trainData)
  
  #Join the Predictions in vector
  predTrain <- cbind(predTrainAA0,predTrainAA1,predTrainAA2,predTrainAA3)
  names(predTrain)<-c("PC1","PC2","PC3","PC4")
  
  #Join the Residuals in vector
  residualsTraining <- trainData[,c("PC1","PC2","PC3","PC4")]-cbind(predTrainAA0,predTrainAA1,predTrainAA2,predTrainAA3)
  
  #Use our custom function to predict each value for Test data
  predTestAA0 <- predictlm (lmAA0,testData)
  predTestAA1 <- predictlm (lmAA1,testData)
  predTestAA2 <- predictlm (lmAA2,testData)
  predTestAA3 <- predictlm (lmAA3,testData)
  
  #Join the Predictions in vector
  predTest <- cbind(predTestAA0,predTestAA1,predTestAA2,predTestAA3)
  names(predTest)<-c("PC1","PC2","PC3","PC4")
  
  #Calculate the resuduals for the training Data
  residualsTest <- testData[,c("PC1","PC2","PC3","PC4")]-cbind(predTestAA0,predTestAA1,predTestAA2,predTestAA3)
  
  #Prepare Object
  lm <- list(predTrain = list(predictedMeasurements = predTrain),
             predTest = list(predictedMeasurements = predTest),
             residualsTraining = residualsTraining,
             residualsTest= residualsTest)
  
  #Assign to List
  pca$lm<-lm
  
  #Original
  
  #Multiple LInear Regeresion for each output variable
  lmAA0 <- with(trainData, lm(AA.0 ~ VX.0+VX.1+VX.2+VX.3+VX.4+Aro1+Aro2+Aro3+Aro4))
  lmAA1 <- with(trainData, lm(AA.1 ~ VX.0+VX.1+VX.2+VX.3+VX.4+Aro1+Aro2+Aro3+Aro4))
  lmAA2 <- with(trainData, lm(AA.2 ~ VX.0+VX.1+VX.2+VX.3+VX.4+Aro1+Aro2+Aro3+Aro4))
  lmAA3 <- with(trainData, lm(AA.3 ~ VX.0+VX.1+VX.2+VX.3+VX.4+Aro1+Aro2+Aro3+Aro4))
  
  #Use our custom function to predict each value for training data
  predTrainAA0 <- predictlm (lmAA0,trainData)
  predTrainAA1 <- predictlm (lmAA1,trainData)
  predTrainAA2 <- predictlm (lmAA2,trainData)
  predTrainAA3 <- predictlm (lmAA3,trainData)
  
  #Join the Predictions in vector
  predTrain <- cbind(predTrainAA0,predTrainAA1,predTrainAA2,predTrainAA3)
  names(predTrain)<-c("AA.0","AA.1","AA.2","AA.3")
  
  #Join the Residuals in vector
  residualsTraining <- trainData[,c("AA.0","AA.1","AA.2","AA.3")]-cbind(predTrainAA0,predTrainAA1,predTrainAA2,predTrainAA3)
  
  #Use our custom function to predict each value for Test data
  predTestAA0 <- predictlm (lmAA0,testData)
  predTestAA1 <- predictlm (lmAA1,testData)
  predTestAA2 <- predictlm (lmAA2,testData)
  predTestAA3 <- predictlm (lmAA3,testData)
  
  #Join the Predictions in vector
  predTest <- cbind(predTestAA0,predTestAA1,predTestAA2,predTestAA3)
  names(predTest)<-c("AA.0","AA.1","AA.2","AA.3")
  
  #Calculate the resuduals for the training Data
  residualsTest <- testData[,c("AA.0","AA.1","AA.2","AA.3")]-cbind(predTestAA0,predTestAA1,predTestAA2,predTestAA3)
  
  #Prepare Object
  lm <- list(predTrain = list(predictedMeasurements = predTrain),
             predTest = list(predictedMeasurements = predTest),
             residualsTraining = residualsTraining,
             residualsTest= residualsTest)
  
  #Assign to List
  original$lm<-lm
  
  result <- list (original = original,
                  pca = pca)
  
  class(result) <- "result"
  return(result)
  
}

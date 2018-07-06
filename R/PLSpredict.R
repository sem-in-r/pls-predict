# Predict function for SEMinR PLS models
#' @export
predict.seminr_model <- function(object, testData, technique = predict_DA, na.print=".", digits=3, ...){
  stopifnot(inherits(object, "seminr_model"))

  #Extract Measurements needed for Predictions
  normData <- testData[,object$mmVariables]

  # Standardize data
  normData[,object$mmVariables] <- standardize_data(normData[,object$mmVariables],object$meanData[object$mmVariables],object$sdData[object$mmVariables])

  #Convert dataset to matrix
  normData<-data.matrix(normData)

  #Estimate Factor Scores from Outter Path
  construct_scores <- normData%*%object$outer_weights

  #Estimate Factor Scores from Inner Path and complete Matrix
  construct_scores <- technique(object$smMatrix, object$path_coef, construct_scores)

  #Predict Measurements with loadings
  predictedMeasurements<-construct_scores%*% t(object$outer_loadings)

  # Unstandardize data
  predictedMeasurements[,object$mmVariables] <- unstandardize_data(predictedMeasurements[,object$mmVariables],object$meanData[object$mmVariables],object$sdData[object$mmVariables])

  #Calculating the residuals
  residuals <- testData[,object$mmVariables] - predictedMeasurements[,object$mmVariables]

  #Prepare return Object
  predictResults <- list(testData = testData[,object$mmVariables],
                         predicted_Measurements = predictedMeasurements[,object$mmVariables],
                         residuals = residuals,
                         predicted_CompositeScores = construct_scores)

  class(predictResults) <- "predictResults"
  return(predictResults)
}

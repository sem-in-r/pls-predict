# Predict function for SEMinR PLS models
#' @export
predict.seminr_model <- function(object, testData, technique = predict_DA, na.print=".", digits=3, ...){
  stopifnot(inherits(object, "seminr_model"))

  # Calculate actuals_star
  # First reappend the testData to the trainData to get fulldata (do not duplicate rows)
  fulldata <- object$data
  fulldata[rownames(testData),] <- testData
  utils::capture.output(fullmodel <- seminr::estimate_pls(data =fulldata,
                                    measurement_model = object$mmMatrix,
                                    interactions = object$interactions,
                                    structural_model = object$smMatrix,
                                    inner_weights = object$inner_weights))
  actual_star <- fullmodel$construct_scores

  #Extract Measurements needed for Predictions
  normData <- testData[,object$mmVariables]

  # Standardize data
  normData[,object$mmVariables] <- standardize_data(normData[,object$mmVariables],object$meanData[object$mmVariables],object$sdData[object$mmVariables])

  #Convert dataset to matrix
  normData<-data.matrix(normData)

  #Estimate Factor Scores from Outter Path
  predicted_construct_scores <- normData%*%object$outer_weights

  #Estimate Factor Scores from Inner Path and complete Matrix
  predicted_construct_scores <- technique(object$smMatrix, object$path_coef, predicted_construct_scores)

  #Predict Measurements with loadings
  predictedMeasurements<-predicted_construct_scores%*% t(object$outer_loadings)

  # Unstandardize data
  predictedMeasurements[,object$mmVariables] <- unstandardize_data(predictedMeasurements[,object$mmVariables],object$meanData[object$mmVariables],object$sdData[object$mmVariables])

  #Calculating the residuals
  residuals <- testData[,object$mmVariables] - predictedMeasurements[,object$mmVariables]

  #Prepare return Object
  predictResults <- list(testData = testData[,object$mmVariables],
                         predicted_items = predictedMeasurements[,object$mmVariables],
                         item_residuals = residuals,
                         predicted_composite_scores = predicted_construct_scores,
                         composite_residuals = (actual_star[rownames(testData),] - predicted_construct_scores),
                         actual_star = actual_star)

  class(predictResults) <- "PLSprediction"
  return(predictResults)
}

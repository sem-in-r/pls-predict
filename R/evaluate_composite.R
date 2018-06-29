
#' @export
evaluate_composite <- function(model, technique = predict_DA, noFolds = 10) {
  # collect model specs
  data <- model$data
  measurement_model <- model$mmMatrix
  interactions <- model$mobi_xm
  structural_model <- model$smMatrix
  inner_weights <- model$inner_weights
  constructs <- model$constructs
  actuals_star <- model$construct_scores


  # shuffle data
  order <- sample(nrow(data),nrow(data), replace = FALSE)
  data <- data[order,]

  #Create 10 equally sized folds
  folds <- cut(seq(1,nrow(data)),breaks=noFolds,labels=FALSE)

  # Prepare matrices
  PLS_predicted_outsample <- matrix(0,nrow = nrow(data),ncol = length(constructs),dimnames = list(1:nrow(data),constructs))
  # In-sample predictions
  PLS_predicted_insample <- matrix(0,nrow = nrow(data),ncol = (noFolds*length(constructs)),dimnames = list(1:nrow(data),rep(constructs,noFolds)))

  for(x in 1:noFolds) {
    testIndexes <- which(folds==x,arr.ind=TRUE)
    trainIndexes <- which(folds!=x,arr.ind=TRUE)
    testingData <- data[testIndexes, ]
    trainingData <- data[-testIndexes, ]

    #PLS prediction on testset model
    train_model <- seminr::estimate_pls(data = trainingData,
                                        measurement_model = measurement_model,
                                        interactions = interactions,
                                        structural_model = structural_model,
                                        inner_weights = inner_weights)
    test_predictions <- PLSpredict(model = train_model,
                             testData = testingData,
                             technique = technique)

    PLS_predicted_outsample[testIndexes,] <-  test_predictions$predicted_CompositeScores

    #PLS prediction on trainset model
    train_predictions <- PLSpredict(model = train_model,
                                    testData = trainingData,
                                    technique = technique)
    PLS_predicted_insample[trainIndexes,(((x-1)*length(constructs))+1):(x*length(constructs))] <- train_predictions$predicted_CompositeScores
  }
  # Collect the relevant data
  average_insample <- matrix(0,nrow = nrow(data), ncol = length(constructs), dimnames = list(1:nrow(data),constructs))
  for (z in 1:length(constructs)) {
    average_insample[,z] <- rowSums(PLS_predicted_insample[,(0:(noFolds-1)*length(constructs))+z])/(noFolds-1)
  }

  rownames(PLS_predicted_outsample) <- rownames(average_insample) <- order

  results <- list(composite_out_of_sample_predictions = PLS_predicted_outsample,
                  composite_in_sample_predictions = average_insample,
                  actuals_star = actuals_star[order,])
  class(results) <- "PLSpredictions"
  return(results)
}

#' @export
predictive_accuracy <- function(results, construct) {

  # Collect information
  actuals_star <- results$actuals_star
  in_sample_predictions <- results$composite_in_sample_predictions
  out_sample_predictions <- results$composite_out_of_sample_predictions

  # Evaluate metrics
  oos_RMSE <- sqrt(mean((actuals_star[,construct] - out_sample_predictions[,construct])^2))
  oos_MAE <- mean(abs(actuals_star[,construct] - out_sample_predictions[,construct]))

  is_RMSE <- sqrt(mean((actuals_star[,construct] - in_sample_predictions[,construct])^2))
  is_MAE <- mean(abs(actuals_star[,construct] - in_sample_predictions[,construct]))

  ##Allocate and sort data - first by actual data and then by predicted data
  holder <- as.data.frame(cbind(in_sample_predictions[,construct],out_sample_predictions[,construct], actuals_star[,construct]))
  colnames(holder) <- c("IS","OOS","actual")
  holder_sorted <- holder[order(holder[,"actual"], holder[,"OOS"]) , ]

  ## Allocate PIntervals
  holder_sorted$average_case_PI_upper <- holder_sorted$OOS + 1.96*oos_RMSE
  holder_sorted$average_case_PI_lower <- holder_sorted$OOS - 1.96*oos_RMSE

  ### PLS Prediction Interval Plot
  plot(NULL,
       xlim = c(1,nrow(holder_sorted)),
       ylim = c(-3,3),
       ylab = "Predictions, Actuals",
       xlab = "Cases, sorted by actuals then predicted values",
       type = "n",
       main = "PLS OOS Predictive Evaluation for Construct")
  segments(c(1:nrow(holder_sorted)),
           holder_sorted$average_case_PI_lower,
           c(1:nrow(holder_sorted)),
           holder_sorted$average_case_PI_upper,
           col = 'lightgrey',
           lwd = 3)
  points(x = c(1:nrow(holder_sorted)),
         y = holder_sorted$actual,
         pch = 21,
         cex = 0.4)
  points(x = c(1:nrow(holder_sorted)),
         y = holder_sorted$OOS,
         pch = 20,
         cex = 0.3)

  cat("IS RMSE : ")
  cat(is_RMSE)
  cat("\n")
  cat("OOS RMSE : ")
  cat(oos_RMSE)
  cat("\n")
  cat("IS MAE : ")
  cat(is_MAE)
  cat("\n")
  cat("OOS MAE : ")
  cat(oos_MAE)
  cat("\n")
}

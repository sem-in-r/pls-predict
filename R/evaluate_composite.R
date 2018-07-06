
#' @export
kfold_predict <- function(model, technique = predict_DA, noFolds = 10) {

  # shuffle data
  order <- sample(nrow(model$data),nrow(model$data), replace = FALSE)
  ordered_data <- model$data[order,]

  #Create 10 equally sized folds
  folds <- cut(seq(1,nrow(ordered_data)),breaks=noFolds,labels=FALSE)

  # Prepare matrices
  PLS_predicted_outsample <- matrix(0,nrow = nrow(ordered_data),ncol = length(model$constructs),dimnames = list(1:nrow(ordered_data),model$constructs))
  # In-sample predictions
  PLS_predicted_insample <- matrix(0,nrow = nrow(ordered_data),ncol = (noFolds*length(model$constructs)),dimnames = list(1:nrow(ordered_data),rep(model$constructs,noFolds)))

  for(x in 1:noFolds) {
    testIndexes <- which(folds==x,arr.ind=TRUE)
    trainIndexes <- which(folds!=x,arr.ind=TRUE)
    testingData <- ordered_data[testIndexes, ]
    trainingData <- ordered_data[-testIndexes, ]

    #PLS prediction on testset model
    train_model <- seminr::estimate_pls(data = trainingData,
                                        measurement_model = model$mmMatrix,
                                        interactions = model$mobi_xm,
                                        structural_model = model$smMatrix,
                                        inner_weights = model$inner_weights)
    test_predictions <- stats::predict(object = train_model,
                                testData = testingData,
                                technique = technique)

    PLS_predicted_outsample[testIndexes,] <-  test_predictions$predicted_CompositeScores

    #PLS prediction on trainset model
    train_predictions <- stats::predict(object = train_model,
                                 testData = trainingData,
                                 technique = technique)
    PLS_predicted_insample[trainIndexes,(((x-1)*length(model$constructs))+1):(x*length(model$constructs))] <- train_predictions$predicted_CompositeScores
  }
  # Collect the relevant data
  average_insample <- matrix(0,nrow = nrow(ordered_data), ncol = length(model$constructs), dimnames = list(1:nrow(ordered_data),model$constructs))
  for (z in 1:length(model$constructs)) {
    average_insample[,z] <- rowSums(PLS_predicted_insample[,(0:(noFolds-1)*length(model$constructs))+z])/(noFolds-1)
  }

  rownames(PLS_predicted_outsample) <- rownames(average_insample) <- order

  results <- list(composite_out_of_sample = PLS_predicted_outsample,
                  composite_in_sample = average_insample,
                  actuals_star = model$construct_scores[order,])
  class(results) <- "kfold_predictions"
  return(results)
}

#' @export
predictive_accuracy <- function(kfold_predictions, construct) {
  stopifnot(inherits(kfold_predictions, "kfold_predictions"))

  # Evaluate metrics
  oos_RMSE <- sqrt(mean((kfold_predictions$actuals_star[,construct] - kfold_predictions$composite_out_of_sample[,construct])^2))
  oos_MAE <- mean(abs(kfold_predictions$actuals_star[,construct] - kfold_predictions$composite_out_of_sample[,construct]))

  is_RMSE <- sqrt(mean((kfold_predictions$actuals_star[,construct] - kfold_predictions$composite_in_sample[,construct])^2))
  is_MAE <- mean(abs(kfold_predictions$actuals_star[,construct] - kfold_predictions$composite_in_sample[,construct]))

  ##Allocate and sort data - first by actual data and then by predicted data
  holder <- as.data.frame(cbind(kfold_predictions$composite_in_sample[,construct],kfold_predictions$composite_out_of_sample[,construct], kfold_predictions$actuals_star[,construct]))
  colnames(holder) <- c("IS","OOS","actual")
  holder_sorted <- holder[order(holder[,"actual"], holder[,"OOS"]) , ]

  ## Allocate PIntervals
  holder_sorted$average_case_PI_upper <- holder_sorted$OOS + 1.96*oos_RMSE
  holder_sorted$average_case_PI_lower <- holder_sorted$OOS - 1.96*oos_RMSE

  ## Highlight influential cases
  outliers <- rownames(subset(holder_sorted, (holder_sorted$actual < holder_sorted$average_case_PI_lower)|(holder_sorted$actual > holder_sorted$average_case_PI_upper) ))
  holder_sorted$outliers <- 1
  holder_sorted[outliers,"outliers"] <- 2

  return_list <- list(evaluation_matrix = holder_sorted,
                      IS_RMSE = is_RMSE,
                      OOS_RMSE = oos_RMSE,
                      IS_MAE = is_MAE,
                      OOS_MAE = oos_MAE,
                      outliers = outliers)
  class(return_list) <- "composite_accuracy"
  return(return_list)
}

#' @export
predictive_validity <- function(kfold_predictions, construct) {

  # Run calibration regression
  cal_lm <- stats::lm(kfold_predictions$composite_in_sample[,construct] ~ kfold_predictions$composite_out_of_sample[,construct])
  cal_sum <- summary(cal_lm)

  # create
  holder <- as.data.frame(cbind(kfold_predictions$composite_in_sample[,construct],kfold_predictions$composite_out_of_sample[,construct], kfold_predictions$actuals_star[,construct]))
  colnames(holder) <- c("IS","OOS","actual")
  holder$Cook <- stats::cooks.distance(cal_lm)
  holder$Cook_degree <- 0
  holder[holder$Cook < (4/nrow(holder)),"Cook_degree"] <- 1
  holder[holder$Cook > (4/nrow(holder)),"Cook_degree"] <- 2
  holder[holder$Cook > 1,"Cook_degree"] <- 3

  rownames(cal_sum$coefficients) <- c(paste("Bias -",construct),paste("Accuracy -", construct))
  cal_sum$coefficients[2,1] <- cal_sum$coefficients[2,1]
  cal_sum$coefficients[2,3] <- (cal_sum$coefficients[2,1]-1)/cal_sum$coefficients[2,2]
  cal_sum$coefficients[2,4] <- stats::pt(cal_sum$coefficients[2,3],df = nrow(holder)-1, lower.tail = TRUE)

  return(list(evaluation_matrix = holder,
              linear_model = cal_sum,
              influential_cases = holder[(holder$Cook_degree == 2)|(holder$Cook_degree == 3),c(1,2,4)]))
}

# Function to evaluate kfold_predictions
#' @export
evaluate_composite <- function(kfold_predictions, construct) {
  composite_accuracy <- predictive_accuracy(kfold_predictions, construct)
  composite_validity <- predictive_validity(kfold_predictions, construct)

  return_list <- list(composite_accuracy = composite_accuracy,
                      composite_validity = composite_validity,
                      construct = construct)
  class(return_list) <- "composite_evaluation"
  return(return_list)
}

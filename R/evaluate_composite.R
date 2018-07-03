
#' @export
kfold_predict <- function(model, technique = predict_DA, noFolds = 10) {
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
  #TODO: Remove predictions from variable names
  results <- list(composite_out_of_sample_predictions = PLS_predicted_outsample,
                  composite_in_sample_predictions = average_insample,
                  actuals_star = actuals_star[order,])
  class(results) <- "kfold_pls_predictions"
  return(results)
}

#' @export
predictive_accuracy <- function(results, construct) {
  # TODO: rename results to kfold_predictions
  # TODO: Remove collect information sections from all methods - consider better naming strategies
  # Collect information
  actuals_star <- results$actuals_star
  in_sample_predictions <- results$composite_in_sample_predictions
  out_sample_predictions <- results$composite_out_of_sample_predictions

  # Evaluate metrics
  oos_RMSE <- sqrt(mean((actuals_star[,construct] - out_sample_predictions[,construct])^2))
  oos_MAE <- mean(abs(actuals_star[,construct] - out_sample_predictions[,construct]))

  is_RMSE <- sqrt(mean((actuals_star[,construct] - in_sample_predictions[,construct])^2))
  is_MAE <- mean(abs(actuals_star[,construct] - in_sample_predictions[,construct]))

  # TODO: Move vis to a seperate function
  ##Allocate and sort data - first by actual data and then by predicted data
  holder <- as.data.frame(cbind(in_sample_predictions[,construct],out_sample_predictions[,construct], actuals_star[,construct]))
  colnames(holder) <- c("IS","OOS","actual")
  holder_sorted <- holder[order(holder[,"actual"], holder[,"OOS"]) , ]

  ## Allocate PIntervals
  holder_sorted$average_case_PI_upper <- holder_sorted$OOS + 1.96*oos_RMSE
  holder_sorted$average_case_PI_lower <- holder_sorted$OOS - 1.96*oos_RMSE

  ## Highlight influential cases
  outliers <- rownames(subset(holder_sorted, (holder_sorted$actual < holder_sorted$average_case_PI_lower)|(holder_sorted$actual > holder_sorted$average_case_PI_upper) ))
  holder_sorted$outliers <- 1
  holder_sorted[outliers,"outliers"] <- 2
  ### PLS Prediction Interval Plot
  graphics::plot(NULL,
       xlim = c(1,nrow(holder_sorted)),
       ylim = c(-3,3),
       ylab = "Predictions, Actuals",
       xlab = "Cases, sorted by actuals then predicted values",
       type = "n",
       main = "PLS OOS Predictive Evaluation for Construct")
  graphics::segments(c(1:nrow(holder_sorted)),
           holder_sorted$average_case_PI_lower,
           c(1:nrow(holder_sorted)),
           holder_sorted$average_case_PI_upper,
           col = 'lightgrey',
           lwd = 3)
  graphics::points(x = c(1:nrow(holder_sorted)),
         y = holder_sorted$actual,
         pch = c(21,24)[holder_sorted$outliers],
         col = c((grDevices::rgb(0,0,0, alpha = 1)),(grDevices::rgb(0,0,1, alpha = 1)))[holder_sorted$outliers],
         cex = 0.4)
  graphics::points(x = c(1:nrow(holder_sorted)),
         y = holder_sorted$OOS,
         pch = c(20,16)[holder_sorted$outliers],
         col = c((grDevices::rgb(0,0,0, alpha = 1)),(grDevices::rgb(0,0,1, alpha = 1)))[holder_sorted$outliers],
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
  cat("Outlier Predictions:\n")
  cat(outliers)
  cat("\n")
  return(list(evaluation_matrix = holder_sorted,
              IS_RMSE = is_RMSE,
              OOS_RMSE = oos_RMSE,
              IS_MAE = is_MAE,
              OOS_MAE = oos_MAE,
              outliers = outliers))
}

#' @export
predictive_validity <- function(results, construct) {

  # Collect information
  actuals_star <- results$actuals_star
  in_sample_predictions <- results$composite_in_sample_predictions
  out_sample_predictions <- results$composite_out_of_sample_predictions

  # Run calibration regression
  cal_lm <- stats::lm(in_sample_predictions[,construct] ~ out_sample_predictions[,construct])
  cal_sum <- summary(cal_lm)

  # create
  holder <- as.data.frame(cbind(in_sample_predictions[,construct],out_sample_predictions[,construct], actuals_star[,construct]))
  colnames(holder) <- c("IS","OOS","actual")
  holder$Cook <- stats::cooks.distance(cal_lm)
  holder$Cook_degree <- 0
  holder[holder$Cook < (4/nrow(holder)),"Cook_degree"] <- 1
  holder[holder$Cook > (4/nrow(holder)),"Cook_degree"] <- 2
  holder[holder$Cook > 1,"Cook_degree"] <- 3

  graphics::plot(y = holder$IS, x = holder$OOS,
       xlab = "Out-of-sample Predictions",
       ylab = "In-sample Predictions",
       xlim = c(-2.5, 2.5),
       ylim = c(-2.5, 2.5),
       pch = c(16, 15, 17)[holder$Cook_degree],
       col = c((grDevices::rgb(0,0,0, alpha = 0.6)),(grDevices::rgb(0,0,1, alpha = 1)),(grDevices::rgb(1,0,1, alpha = 0.6)))[holder$Cook_degree],
       main = paste("Overfit Diagram - ",construct))
  graphics::abline(v = 0, h = 0)
  graphics::abline(a = 0, b = 1)
  graphics::abline(a = cal_lm$coefficients[1], b = cal_lm$coefficients[2], col = "red")

  rownames(cal_sum$coefficients) <- c(paste("Bias -",construct),paste("Accuracy -", construct))
  cal_sum$coefficients[2,1] <- cal_sum$coefficients[2,1]
  cal_sum$coefficients[2,3] <- (cal_sum$coefficients[2,1]-1)/cal_sum$coefficients[2,2]
  cal_sum$coefficients[2,4] <- stats::pt(cal_sum$coefficients[2,3],df = nrow(holder)-1, lower.tail = TRUE)
  print(cal_sum$coefficients)
  cat("\n")
  cat("Cases with Cook's Distance > 1\n")
  if (length(rownames(holder[holder$Cook_degree == 3,]))>0) {
    #cat(rownames(holder[holder$Cook_degree == 3,]))
    print(holder[holder$Cook_degree == 3,c(1,2,4)])
  } else {
    cat("None\n")
  }
  cat("Cases with Cook's Distance > 4/n\n")
  if (length(rownames(holder[holder$Cook_degree == 2,]))>0) {
    #cat(rownames(holder[holder$Cook_degree == 2,]))
    print(holder[holder$Cook_degree == 2,c(1,2,4)])
  } else {
    cat("None\n")
  }
  cat("\n")
  return(list(evaluation_matrix = holder,
              linear_model = cal_sum,
              influential_cases = holder[(holder$Cook_degree == 2)|(holder$Cook_degree == 3),c(1,2,4)]))
}

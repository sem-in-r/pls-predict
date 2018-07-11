
#' @export
predict_pls <- function(model, technique = predict_DA, noFolds = 10) {

  stopifnot(inherits(model, "seminr_model"))
  # shuffle data
  order <- sample(nrow(model$data),nrow(model$data), replace = FALSE)
  ordered_data <- model$data[order,]

  #Create noFolds equally sized folds
  folds <- cut(seq(1,nrow(ordered_data)),breaks=noFolds,labels=FALSE)

  # collect in-sample and out-sample prediction matrices
  pred_matrices <- prediction_matrices(folds, noFolds, ordered_data, model,technique)
  PLS_predicted_outsample_construct <- pred_matrices$out_of_sample_construct
  PLS_predicted_insample_construct <- pred_matrices$in_sample_construct
  PLS_predicted_outsample_item <- pred_matrices$out_of_sample_item
  PLS_predicted_insample_item <- pred_matrices$in_sample_item
  LM_predicted_outsample_item <- pred_matrices$out_of_sample_lm_item
  LM_predicted_insample_item <- pred_matrices$in_sample_lm_item

  results <- list(composite_out_of_sample = PLS_predicted_outsample_construct,
                  composite_in_sample = PLS_predicted_insample_construct,
                  actuals_star = model$construct_scores[order,],
                  item_out_of_sample = PLS_predicted_outsample_item,
                  item_in_sample = PLS_predicted_insample_item,
                  lm_out_of_sample = LM_predicted_outsample_item,
                  lm_in_sample = LM_predicted_insample_item)
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

# Function to calculate accuracy of a composite
composite_accuracy <- function(construct, pls_prediction_kfold) {
  stopifnot(inherits(pls_prediction_kfold, "pls_prediction_kfold"))

  # Evaluate metrics
  oos_RMSE <- sqrt(mean((pls_prediction_kfold$composites$actuals_star[,construct] - pls_prediction_kfold$composites$composite_out_of_sample[,construct])^2))
  oos_MAE <- mean(abs(pls_prediction_kfold$composites$actuals_star[,construct] - pls_prediction_kfold$composites$composite_out_of_sample[,construct]))

  is_RMSE <- sqrt(mean((pls_prediction_kfold$composites$actuals_star[,construct] - pls_prediction_kfold$composites$composite_in_sample[,construct])^2))
  is_MAE <- mean(abs(pls_prediction_kfold$composites$actuals_star[,construct] - pls_prediction_kfold$composites$composite_in_sample[,construct]))

  ##Allocate and sort data - first by actual data and then by predicted data
  holder <- as.data.frame(cbind(pls_prediction_kfold$composites$composite_in_sample[,construct],pls_prediction_kfold$composites$composite_out_of_sample[,construct], pls_prediction_kfold$composites$actuals_star[,construct]))
  colnames(holder) <- c("IS","OOS","actual")
  holder_sorted <- holder[order(holder[,"actual"], holder[,"OOS"]) , ]

  ## Allocate PIntervals
  holder_sorted$average_case_PI_upper <- holder_sorted$OOS + 1.96*oos_RMSE
  holder_sorted$average_case_PI_lower <- holder_sorted$OOS - 1.96*oos_RMSE

  ## Highlight influential cases
  outliers <- rownames(subset(holder_sorted, (holder_sorted$actual < holder_sorted$average_case_PI_lower)|(holder_sorted$actual > holder_sorted$average_case_PI_upper) ))
  holder_sorted$outliers <- 1
  holder_sorted[outliers,"outliers"] <- 2

  return_list <- list(accuracy_matrix = holder_sorted,
                      IS_RMSE = is_RMSE,
                      OOS_RMSE = oos_RMSE,
                      IS_MAE = is_MAE,
                      OOS_MAE = oos_MAE,
                      outliers = outliers)
  class(return_list) <- "composite_accuracy"
  return(return_list)
}

# Function to calculate validity (overfit) of a composite
composite_validity <- function(construct, pls_prediction_kfold) {

  # Run calibration regression
  cal_lm <- stats::lm(pls_prediction_kfold$composites$composite_in_sample[,construct] ~ pls_prediction_kfold$composites$composite_out_of_sample[,construct])
  cal_sum <- summary(cal_lm)

  # create
  holder <- as.data.frame(cbind(pls_prediction_kfold$composites$composite_in_sample[,construct],pls_prediction_kfold$composites$composite_out_of_sample[,construct], pls_prediction_kfold$composites$actuals_star[,construct]))
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

  return(list(validity_matrix = holder,
              linear_model = cal_sum,
              influential_cases = holder[(holder$Cook_degree == 2)|(holder$Cook_degree == 3),c(1,2,4)]))
}

# Function to evaluate pls_prediction_kfold
evaluate_composite <- function(pls_prediction_kfold) {
  composite_accuracy <- sapply(colnames(pls_prediction_kfold$composites$composite_out_of_sample), composite_accuracy, pls_prediction_kfold = pls_prediction_kfold)
  composite_validity <- sapply(colnames(pls_prediction_kfold$composites$composite_out_of_sample), composite_validity, pls_prediction_kfold = pls_prediction_kfold)
  return_list <- list(composite_accuracy = composite_accuracy,
                      composite_validity = composite_validity)
  class(return_list) <- "composite_evaluation"
  return(return_list)
}

# Function to calculate item metrics
item_metrics <- function(pls_prediction_kfold) {

  # Genereate IS PLS metrics
  PLS_item_predictive_metrics_IS <- apply(pls_prediction_kfold$items$pls_in_sample_residuals, 2, prediction_metrics)

  # Generate OOS PLS metrics
  PLS_item_residuals_OOS <- as.matrix(pls_prediction_kfold$items$item_actuals[,colnames(pls_prediction_kfold$items$item_out_of_sample)] - pls_prediction_kfold$items$item_out_of_sample)
  PLS_item_predictive_metrics_OOS <- apply(PLS_item_residuals_OOS, 2, prediction_metrics)

  # Generate IS LM metrics
  LM_item_predictive_metrics_IS <- apply(pls_prediction_kfold$items$lm_in_sample_residuals, 2, prediction_metrics)

  # Generate OOS LM metrics
  LM_item_residuals_OOS <- as.matrix(pls_prediction_kfold$items$item_actuals[,colnames(pls_prediction_kfold$items$lm_out_of_sample)] - pls_prediction_kfold$items$lm_out_of_sample)
  LM_item_predictive_metrics_OOS <- apply(LM_item_residuals_OOS, 2, prediction_metrics)

  # Assign rownames to matrices
  rownames(PLS_item_predictive_metrics_IS) <- rownames(PLS_item_predictive_metrics_OOS) <- rownames(LM_item_predictive_metrics_OOS) <- c("RMSE","MAD")
  rownames(LM_item_predictive_metrics_OOS) <- rownames(LM_item_predictive_metrics_IS) <- c("RMSE","MAD")

  return(list(PLS_item_predictive_metrics_IS = PLS_item_predictive_metrics_IS,
              PLS_item_predictive_metrics_OOS = PLS_item_predictive_metrics_OOS,
              LM_item_predictive_metrics_IS = LM_item_predictive_metrics_IS,
              LM_item_predictive_metrics_OOS = LM_item_predictive_metrics_OOS))
}

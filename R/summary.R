# Summary method for PLSpredict
#' @export
summary.PLSprediction <- function(object, na.print=".", digits=3, ...) {

  stopifnot(inherits(object, "PLSprediction"))
  # calculate RMSE and MAD
  item_metrics <- apply(object$item_residuals,2, prediction_metrics)
  composite_metrics <- apply(object$composite_residuals, 2, prediction_metrics)
  rownames(item_metrics) <- rownames(composite_metrics) <- c("RMSE","MAD")
  # prepare return object with class
  PLSprediction_summary <- list(predicted_items = object$predicted_items,
                                predicted_composite_scores = object$predicted_composite_scores,
                                item_predictive_metrics = item_metrics,
                                composite_predictive_metrics = composite_metrics)
  class(PLSprediction_summary) <- "summary.PLSprediction"
  return(PLSprediction_summary)
}

# Function to print summary of PLSpredict
#' @export
print.summary.PLSprediction <- function(x, na.print=".", digits=3, ...) {

  stopifnot(inherits(x, "summary.PLSprediction"))
  cat("Summary of PLS Prediction\n")
  cat("Item Predictions:\n")
  print(x$predicted_items, digits = digits, na.print = na.print)
  cat("\nItem Predictive Metrics:\n")
  print(x$item_predictive_metrics, digits = digits, na.print = na.print)
  cat("\nComposite Predictions:\n")
  print(x$predicted_composite_scores, digits = digits, na.print = na.print)
  cat("\nComposite Predictive Metrics:\n")
  print(x$composite_predictive_metrics, digits = digits, na.print = na.print)
  cat("\n")
  invisible(x)
}

#' @export
summary.pls_prediction_kfold <- function(object, construct = NULL, na.print=".", digits=3, ...) {

  stopifnot(inherits(object, "pls_prediction_kfold"))

  # Composite Evaluation
  composite_evaluation <- evaluate_composite(object)

  # Item evaluation
  item_evaluation <- item_metrics(object)

  # If a composite was specified, return that composite, else return all
  if (is.null(construct)) {construct <- colnames(object$composites$composite_out_of_sample) }
  composite_accuracy <- composite_evaluation$composite_accuracy[,construct]
  composite_validity <- composite_evaluation$composite_validity[,construct]

  model_summary <- list(composite_accuracy = composite_accuracy,
                        composite_validity = composite_validity,
                        item_evaluation = item_evaluation,
                        construct = construct)
  class(model_summary) <- "summary.pls_prediction_kfold"
  return(model_summary)
}

# Print summary method for PLSpredict
#' @export
print.summary.pls_prediction_kfold <- function(x, na.print=".", digits=3, ...) {

  stopifnot(inherits(x, "summary.pls_prediction_kfold"))

  #sapply(x$construct, print_composite_evaluation, object = x)

  # Print the validity and accuracy for each construct
  for (construct in x$construct) { print_composite_evaluation(construct, x, na.print=".", digits=3) }

  # Print the item metrics PLS & LM
  print_item_evaluation(x, na.print = na.print, digits = digits)
  invisible(x)
}

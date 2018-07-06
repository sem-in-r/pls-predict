# Summary method for PLSpredict
#' @export
summary.composite_evaluation <- function(object, na.print=".", digits=3, ...) {

  stopifnot(inherits(object, "composite_evaluation"))
  # Composite accuracy
  model_summary <- list(construct = object$construct,
                        IS_RMSE = object$composite_accuracy$IS_RMSE,
                        OOS_RMSE = object$composite_accuracy$OOS_RMSE,
                        IS_MAE = object$composite_accuracy$IS_MAE,
                        OOS_MAE = object$composite_accuracy$OOS_MAE,
                        outliers = object$composite_accuracy$outliers,
                        accuracy_matrix = object$composite_accuracy$evaluation_matrix,
                        validity_matrix = object$composite_validity$evaluation_matrix,
                        validity_lm = object$composite_validity$linear_model,
                        inflential_cases = object$composite3_validity$influential_cases)
  class(model_summary) <- "summary.composite_evaluation"
  return(model_summary)
}

# Print summary method for PLSpredict
#' @export
print.summary.composite_evaluation <- function(x, na.print=".", digits=3, ...) {

  stopifnot(inherits(x, "summary.composite_evaluation"))
  # Composite Accuracy
  cat("IS RMSE : ")
  print(x$IS_RMSE, na.print = na.print, digits=digits)
  cat("\n")
  cat("OOS RMSE : ")
  print(x$OOS_RMSE, na.print = na.print, digits=digits)
  cat("\n")
  cat("IS MAE : ")
  print(x$IS_MAE, na.print = na.print, digits=digits)
  cat("\n")
  cat("OOS MAE : ")
  print(x$OOS_MAE, na.print = na.print, digits=digits)
  cat("\n")
  cat("Outlier Predictions:\n")
  cat(x$outliers)
  cat("\n")

  # Composite Validity
  cat("\n")
  cat("Cases with Cook's Distance > 1\n")
  if (length(rownames(x$evaluation_matrix[x$evaluation_matrix$Cook_degree == 3,]))>0) {
    #cat(rownames(holder[holder$Cook_degree == 3,]))
    print(x$evaluation_matrix[x$evaluation_matrix$Cook_degree == 3,c(1,2,4)], na.print = na.print, digits=digits)
  } else {
    cat("None\n")
  }
  cat("Cases with Cook's Distance > 4/n\n")
  if (length(rownames(x$evaluation_matrix[x$evaluation_matrix$Cook_degree == 2,]))>0) {
    #cat(rownames(holder[holder$Cook_degree == 2,]))
    print(x$evaluation_matrix[x$evaluation_matrix$Cook_degree == 2,c(1,2,4)], na.print = na.print, digits=digits)
  } else {
    cat("None\n")
  }
  cat("\n")
  print(x$linear_model$coefficients, na.print = na.print, digits=digits)
  cat("\n")
  invisible(x)

}

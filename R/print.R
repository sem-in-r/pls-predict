# Print Composite Accuracy
print_composite_evaluation <- function(x, object, na.print=".", digits=3) {
  if (length(object$construct) <= 1) {
    print_composite_accuracy(object$composite_accuracy, composite = x, na.print=".", digits=3)
    print_composite_validity(object$composite_validity, composite = x, na.print=".", digits=3)
  } else {
    print_composite_accuracy(object$composite_accuracy[,x], composite = x, na.print=".", digits=3)
    print_composite_validity(object$composite_validity[,x], composite = x, na.print=".", digits=3)
  }
  invisible(x)
}

# Print Composite Validity
print_composite_validity <- function(x, composite,na.print=".", digits=3) {
  # Composite Validity
  cat(paste("\n-------- Composite Validity for",composite,"--------"))
  cat("\n\n")
  cat("Cases with Cook's Distance > 1\n")
  if (length(rownames(x$validity_matrix[x$validity_matrix$Cook_degree == 3,]))>0) {
    #cat(rownames(holder[holder$Cook_degree == 3,]))
    print(x$validity_matrix[x$validity_matrix$Cook_degree == 3,c(1,2,4)], na.print = na.print, digits=digits)
  } else {
    cat("None\n")
  }
  cat("Cases with Cook's Distance > 4/n\n")
  if (length(rownames(x$validity_matrix[x$validity_matrix$Cook_degree == 2,]))>0) {
    #cat(rownames(holder[holder$Cook_degree == 2,]))
    print(x$validity_matrix[x$validity_matrix$Cook_degree == 2,c(1,2,4)], na.print = na.print, digits=digits)
  } else {
    cat("None\n")
  }
  cat("\n")
  print(x$linear_model$coefficients, na.print = na.print, digits=digits)
  cat("\n")

  invisible(x)
}

# Print composite Accuracy
print_composite_accuracy <- function(x, composite, na.print=".", digits=3) {
  cat(paste("\n-------- Composite Accuracy for",composite,"--------"))
  cat("\n\n")
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
  invisible(x)
}

# Print all the item metrics LM & PLS
print_item_evaluation <- function(x, na.print=".", digits=3) {
  cat(paste("\n-------- Item Evaluation for PLS Model","--------"))
  cat("\n\n")
  cat("PLS in-sample item metrics:\n")
  print(x$item_evaluation$PLS_item_predictive_metrics_IS, na.print = na.print, digits = digits)
  cat("\nPLS out-of-sample item metrics:\n")
  print(x$item_evaluation$PLS_item_predictive_metrics_OOS, na.print = na.print, digits = digits)
  cat("\nLM in-sample item metrics:\n")
  print(x$item_evaluation$LM_item_predictive_metrics_IS, na.print = na.print, digits = digits)
  cat("\nLM out-of-sample item metrics:\n")
  print(x$item_evaluation$LM_item_predictive_metrics_OOS, na.print = na.print, digits = digits)
  cat("\n")
  invisible(x)
}

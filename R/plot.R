#
#' @export
plot.summary.pls_prediction_kfold <- function(x, constructs = NULL, ...) {
  stopifnot(inherits(x, "summary.pls_prediction_kfold"))
  if (is.null(constructs)) { constructs <- colnames(x$composite_accuracy) }

  plots <- list()
  for (construct in constructs) {
    plots[[construct]] <- print_composite_plots(construct, x)
  }
  invisible(plots)
}

#
#' @export
plot.summary.bootstrap_prediction <- function(x, ...) {
  stopifnot(inherits(x, "summary.bootstrap_prediction"))
  ifelse(is.null(x$items), items <- names(x$object$average_case_PI), items <- x$items)

  plots <- list()
  for (item in items) {
    plots[[item]] <- print_prediction_intervals(item, x$object)
  }
  invisible(x)
}

# Function to print prediction intervals plot
print_prediction_intervals <- function(item, object) {

  ##Create Holders & assign PI data
  ave_item_PI <- object$average_case_PI[[item]]
  case_item_PI <- object$case_wise_PI[[item]]

  ##Allocate and sort data - first by actual data and then by predicted data
  dataholder <- cbind(ave_item_PI,object$point_predictions[,item], object$actuals[,item],case_item_PI )
  dataholder_sorted <- dataholder[order(dataholder[,4], dataholder[,3]),]

  ### Item PLS Prediction Intervals
  graphics::plot(NULL, xlim = c(1,nrow(dataholder_sorted)), ylim = c(min(dataholder_sorted[,5]),max(dataholder_sorted[,6])), ylab = "Ranges", xlab = "Cases", type = "n", main = paste("PLS Prediction Intervals for item",item))
  graphics::segments(c(1:nrow(dataholder_sorted)),dataholder_sorted[,5],c(1:nrow(dataholder_sorted)),dataholder_sorted[,6], col = 'lightgrey', lwd = 3)
  graphics::segments(c(1:nrow(dataholder_sorted)),dataholder_sorted[,1],c(1:nrow(dataholder_sorted)),dataholder_sorted[,2], col = 'darkgrey', lwd = 3)
  graphics::points(x = c(1:nrow(dataholder_sorted)), y = dataholder_sorted[,4],pch = 21, cex = 0.8, lwd = 2)
  graphics::points(x = c(1:nrow(dataholder_sorted)), y = dataholder_sorted[,3],pch = 20, cex = 0.8)

}

# Function to print composite plots
print_composite_plots <- function(construct, object) {
  ### PLS Prediction Interval Plot
  accuracy_plot <- graphics::plot(NULL,
                                  xlim = c(1,nrow(object$composite_accuracy["accuracy_matrix",construct][[1]])),
                                  ylim = c(-3,3),
                                  ylab = "Predictions, Actuals",
                                  xlab = "Cases, sorted by actuals then predicted values",
                                  type = "n",
                                  main = paste("PLS OOS Predictive Evaluation - ",construct))
  accuracy_plot <- graphics::segments(c(1:nrow(object$composite_accuracy["accuracy_matrix",construct][[1]])),
                                      object$composite_accuracy["accuracy_matrix",construct][[1]][,"average_case_PI_lower"],
                                      c(1:nrow(object$composite_accuracy["accuracy_matrix",construct][[1]])),
                                      object$composite_accuracy["accuracy_matrix",construct][[1]][,"average_case_PI_upper"],
                                      col = 'lightgrey',
                                      lwd = 3)
  accuracy_plot <- graphics::points(x = c(1:nrow(object$composite_accuracy["accuracy_matrix",construct][[1]])),
                                    y = object$composite_accuracy["accuracy_matrix",construct][[1]][,"actual"],
                                    pch = c(21,24)[object$composite_accuracy["accuracy_matrix",construct][[1]][,"outliers"]],
                                    col = c((grDevices::rgb(0,0,0, alpha = 1)),(grDevices::rgb(0,0,1, alpha = 1)))[object$composite_accuracy["accuracy_matrix",construct][[1]][,"outliers"]],
                                    cex = 0.4)
  accuracy_plot <- graphics::points(x = c(1:nrow(object$composite_accuracy["accuracy_matrix",construct][[1]])),
                                    y = object$composite_accuracy["accuracy_matrix",construct][[1]][,"OOS"],
                                    pch = c(20,16)[object$composite_accuracy["accuracy_matrix",construct][[1]][,"outliers"]],
                                    col = c((grDevices::rgb(0,0,0, alpha = 1)),(grDevices::rgb(0,0,1, alpha = 1)))[object$composite_accuracy["accuracy_matrix",construct][[1]][,"outliers"]],
                                    cex = 0.3)


  # PLS validity Plot
  validity_plot <- graphics::plot(y = object$composite_validity[,construct]$validity_matrix[,"IS"], x = object$composite_validity[,construct]$validity_matrix[,"OOS"],
                                  xlab = "Out-of-sample Predictions",
                                  ylab = "In-sample Predictions",
                                  xlim = c(-2.5, 2.5),
                                  ylim = c(-2.5, 2.5),
                                  pch = c(16, 15, 17)[object$composite_validity[,construct]$validity_matrix[,"Cook_degree"]],
                                  col = c((grDevices::rgb(0,0,0, alpha = 0.6)),(grDevices::rgb(0,0,1, alpha = 1)),(grDevices::rgb(1,0,1, alpha = 0.6)))[object$composite_validity[,construct]$validity_matrix[,"Cook_degree"]],
                                  main = paste("Overfit Diagram - ",construct))
  validity_plot <- graphics::abline(v = 0, h = 0)
  validity_plot <- graphics::abline(a = 0, b = 1)
  validity_plot <- graphics::abline(a = object$composite_validity[,construct]$linear_model$coefficients[1], b = object$composite_validity[,construct]$linear_model$coefficients[2], col = "red")
  return_list <- list(accuracy_plot = accuracy_plot,
                      validity_plot = validity_plot)
  return(return_list)
}

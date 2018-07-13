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

# Function to print composite plots
print_composite_plots <- function(construct, object) {
  ### PLS Prediction Interval Plot
  accuracy_plot <- graphics::plot(NULL,
                                  xlim = c(1,nrow(x$composite_accuracy["accuracy_matrix",construct][[1]])),
                                  ylim = c(-3,3),
                                  ylab = "Predictions, Actuals",
                                  xlab = "Cases, sorted by actuals then predicted values",
                                  type = "n",
                                  main = paste("PLS OOS Predictive Evaluation - ",construct))
  accuracy_plot <- graphics::segments(c(1:nrow(x$composite_accuracy["accuracy_matrix",construct][[1]])),
                                      x$composite_accuracy["accuracy_matrix",construct][[1]][,"average_case_PI_lower"],
                                      c(1:nrow(x$composite_accuracy["accuracy_matrix",construct][[1]])),
                                      x$composite_accuracy["accuracy_matrix",construct][[1]][,"average_case_PI_upper"],
                                      col = 'lightgrey',
                                      lwd = 3)
  accuracy_plot <- graphics::points(x = c(1:nrow(x$composite_accuracy["accuracy_matrix",construct][[1]])),
                                    y = x$composite_accuracy["accuracy_matrix",construct][[1]][,"actual"],
                                    pch = c(21,24)[x$composite_accuracy["accuracy_matrix",construct][[1]][,"outliers"]],
                                    col = c((grDevices::rgb(0,0,0, alpha = 1)),(grDevices::rgb(0,0,1, alpha = 1)))[x$composite_accuracy["accuracy_matrix",construct][[1]][,"outliers"]],
                                    cex = 0.4)
  accuracy_plot <- graphics::points(x = c(1:nrow(x$composite_accuracy["accuracy_matrix",construct][[1]])),
                                    y = x$composite_accuracy["accuracy_matrix",construct][[1]][,"OOS"],
                                    pch = c(20,16)[x$composite_accuracy["accuracy_matrix",construct][[1]][,"outliers"]],
                                    col = c((grDevices::rgb(0,0,0, alpha = 1)),(grDevices::rgb(0,0,1, alpha = 1)))[x$composite_accuracy["accuracy_matrix",construct][[1]][,"outliers"]],
                                    cex = 0.3)


  # PLS validity Plot
  validity_plot <- graphics::plot(y = x$composite_validity[,construct]$validity_matrix[,"IS"], x = x$composite_validity[,construct]$validity_matrix[,"OOS"],
                                  xlab = "Out-of-sample Predictions",
                                  ylab = "In-sample Predictions",
                                  xlim = c(-2.5, 2.5),
                                  ylim = c(-2.5, 2.5),
                                  pch = c(16, 15, 17)[x$composite_validity[,construct]$validity_matrix[,"Cook_degree"]],
                                  col = c((grDevices::rgb(0,0,0, alpha = 0.6)),(grDevices::rgb(0,0,1, alpha = 1)),(grDevices::rgb(1,0,1, alpha = 0.6)))[x$composite_validity[,construct]$validity_matrix[,"Cook_degree"]],
                                  main = paste("Overfit Diagram - ",construct))
  validity_plot <- graphics::abline(v = 0, h = 0)
  validity_plot <- graphics::abline(a = 0, b = 1)
  validity_plot <- graphics::abline(a = x$composite_validity[,construct]$linear_model$coefficients[1], b = x$composite_validity[,construct]$linear_model$coefficients[2], col = "red")
  return_list <- list(accuracy_plot = accuracy_plot,
                      validity_plot = validity_plot)
  return(return_list)
}

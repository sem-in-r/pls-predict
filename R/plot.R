#
#' @export
plot.composite_evaluation <- function(x,...) {
  stopifnot(inherits(x, "composite_evaluation"))

  ### PLS Prediction Interval Plot
  accuracy_plot <- graphics::plot(NULL,
                 xlim = c(1,nrow(x$composite_accuracy$evaluation_matrix)),
                 ylim = c(-3,3),
                 ylab = "Predictions, Actuals",
                 xlab = "Cases, sorted by actuals then predicted values",
                 type = "n",
                 main = paste("PLS OOS Predictive Evaluation - ",x$construct))
  accuracy_plot <- graphics::segments(c(1:nrow(x$composite_accuracy$evaluation_matrix)),
                     x$composite_accuracy$evaluation_matrix$average_case_PI_lower,
                     c(1:nrow(x$composite_accuracy$evaluation_matrix)),
                     x$composite_accuracy$evaluation_matrix$average_case_PI_upper,
                     col = 'lightgrey',
                     lwd = 3)
  accuracy_plot <- graphics::points(x = c(1:nrow(x$composite_accuracy$evaluation_matrix)),
                   y = x$composite_accuracy$evaluation_matrix$actual,
                   pch = c(21,24)[x$composite_accuracy$evaluation_matrix$outliers],
                   col = c((grDevices::rgb(0,0,0, alpha = 1)),(grDevices::rgb(0,0,1, alpha = 1)))[x$composite_accuracy$evaluation_matrix$outliers],
                   cex = 0.4)
  accuracy_plot <- graphics::points(x = c(1:nrow(x$composite_accuracy$evaluation_matrix)),
                   y = x$composite_accuracy$evaluation_matrix$OOS,
                   pch = c(20,16)[x$composite_accuracy$evaluation_matrix$outliers],
                   col = c((grDevices::rgb(0,0,0, alpha = 1)),(grDevices::rgb(0,0,1, alpha = 1)))[x$composite_accuracy$evaluation_matrix$outliers],
                   cex = 0.3)


  # PLS validity Plot
  validity_plot <- graphics::plot(y = x$composite_validity$evaluation_matrix$IS, x = x$composite_validity$evaluation_matrix$OOS,
                 xlab = "Out-of-sample Predictions",
                 ylab = "In-sample Predictions",
                 xlim = c(-2.5, 2.5),
                 ylim = c(-2.5, 2.5),
                 pch = c(16, 15, 17)[x$composite_validity$evaluation_matrix$Cook_degree],
                 col = c((grDevices::rgb(0,0,0, alpha = 0.6)),(grDevices::rgb(0,0,1, alpha = 1)),(grDevices::rgb(1,0,1, alpha = 0.6)))[x$composite_validity$evaluation_matrix$Cook_degree],
                 main = paste("Overfit Diagram - ",x$construct))
  validity_plot <- graphics::abline(v = 0, h = 0)
  validity_plot <- graphics::abline(a = 0, b = 1)
  validity_plot <- graphics::abline(a = x$composite_validity$linear_model$coefficients[1], b = x$composite_validity$linear_model$coefficients[2], col = "red")

  return_list <- list(accuracy_plot = accuracy_plot,
                      validity_plot = validity_plot)
  invisible(return_list)
}

#' @export
plot.summary.composite_evaluation <- function(x,...) {
  stopifnot(inherits(x, "summary.composite_evaluation"))

  ### PLS Prediction Interval Plot
  accuracy_plot <- graphics::plot(NULL,
                                  xlim = c(1,nrow(x$accuracy_matrix)),
                                  ylim = c(-3,3),
                                  ylab = "Predictions, Actuals",
                                  xlab = "Cases, sorted by actuals then predicted values",
                                  type = "n",
                                  main = paste("PLS OOS Predictive Evaluation - ",x$construct))
  accuracy_plot <- graphics::segments(c(1:nrow(x$accuracy_matrix)),
                                      x$accuracy_matrix$average_case_PI_lower,
                                      c(1:nrow(x$accuracy_matrix)),
                                      x$accuracy_matrix$average_case_PI_upper,
                                      col = 'lightgrey',
                                      lwd = 3)
  accuracy_plot <- graphics::points(x = c(1:nrow(x$accuracy_matrix)),
                                    y = x$accuracy_matrix$actual,
                                    pch = c(21,24)[x$accuracy_matrix$outliers],
                                    col = c((grDevices::rgb(0,0,0, alpha = 1)),(grDevices::rgb(0,0,1, alpha = 1)))[x$accuracy_matrix$outliers],
                                    cex = 0.4)
  accuracy_plot <- graphics::points(x = c(1:nrow(x$accuracy_matrix)),
                                    y = x$accuracy_matrix$OOS,
                                    pch = c(20,16)[x$accuracy_matrix$outliers],
                                    col = c((grDevices::rgb(0,0,0, alpha = 1)),(grDevices::rgb(0,0,1, alpha = 1)))[x$accuracy_matrix$outliers],
                                    cex = 0.3)


  # PLS validity Plot
  validity_plot <- graphics::plot(y = x$validity_matrix$IS, x = x$validity_matrix$OOS,
                                  xlab = "Out-of-sample Predictions",
                                  ylab = "In-sample Predictions",
                                  xlim = c(-2.5, 2.5),
                                  ylim = c(-2.5, 2.5),
                                  pch = c(16, 15, 17)[x$validity_matrix$Cook_degree],
                                  col = c((grDevices::rgb(0,0,0, alpha = 0.6)),(grDevices::rgb(0,0,1, alpha = 1)),(grDevices::rgb(1,0,1, alpha = 0.6)))[x$validity_matrix$Cook_degree],
                                  main = paste("Overfit Diagram - ",x$construct))
  validity_plot <- graphics::abline(v = 0, h = 0)
  validity_plot <- graphics::abline(a = 0, b = 1)
  validity_plot <- graphics::abline(a = x$validity_lm$coefficients[1], b = x$validity_lm$coefficients[2], col = "red")

  return_list <- list(accuracy_plot = accuracy_plot,
                      validity_plot = validity_plot)
  invisible(return_list)
}

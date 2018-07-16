#' @export
predict_pls <- function(model, technique = predict_DA, noFolds = NULL, cores = NULL) {

  stopifnot(inherits(model, "seminr_model"))

  # shuffle data
  order <- sample(nrow(model$data),nrow(model$data), replace = FALSE)
  ordered_data <- model$data[order,]

  # collect in-sample and out-sample prediction matrices and sort everything to original row indexes
  pred_matrices <- prediction_matrices( noFolds, ordered_data, model,technique, cores)
  PLS_predicted_outsample_construct <- pred_matrices$out_of_sample_construct[as.character(c(1:nrow(model$data))),]
  PLS_predicted_insample_construct <- pred_matrices$in_sample_construct[as.character(c(1:nrow(model$data))),]
  PLS_predicted_outsample_item <- pred_matrices$out_of_sample_item[as.character(c(1:nrow(model$data))),]
  PLS_predicted_insample_item <- pred_matrices$in_sample_item[as.character(c(1:nrow(model$data))),]
  LM_predicted_outsample_item <- pred_matrices$out_of_sample_lm_item[as.character(c(1:nrow(model$data))),]
  LM_predicted_insample_item <- pred_matrices$in_sample_lm_item[as.character(c(1:nrow(model$data))),]

  # Allocate results
  results <- list(composites = list(composite_out_of_sample = PLS_predicted_outsample_construct,
                                    composite_in_sample = PLS_predicted_insample_construct,
                                    actuals_star = model$construct_scores[as.character(c(1:nrow(model$data))),]),
                  items = list(item_out_of_sample = PLS_predicted_outsample_item,
                               item_in_sample = PLS_predicted_insample_item,
                               lm_out_of_sample = LM_predicted_outsample_item,
                               lm_in_sample = LM_predicted_insample_item,
                               item_actuals = ordered_data[as.character(c(1:nrow(model$data))),],
                               lm_in_sample_residuals = pred_matrices$lm_in_sample_item_residuals[as.character(c(1:nrow(model$data))),],
                               pls_in_sample_residuals = pred_matrices$pls_in_sample_item_residuals[as.character(c(1:nrow(model$data))),]))
  class(results) <- "pls_prediction_kfold"
  return(results)
}

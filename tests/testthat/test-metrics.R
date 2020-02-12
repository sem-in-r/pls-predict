context("PLSpredict correctly calculates the PLS and LM metrics DA and EA technique\n")

# Test cases
## DA Approach
set.seed(12345)
# seminr syntax for creating measurement model
mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5), weights = mode_A),
  composite("Expectation",  multi_items("CUEX", 1:3), weights = mode_A),
  composite("Value",        multi_items("PERV", 1:2), weights = mode_A),
  composite("Satisfaction", multi_items("CUSA", 1:3), weights = mode_A)
)

# structural model: note that name of the interactions construct should be
#  the names of its two main constructs joined by a '*' in between.
mobi_sm <- relationships(
  paths(to = "Satisfaction",
        from = c("Image", "Expectation", "Value"))
)

# Load data, assemble model, and estimate using PLSpredict
mobi <- mobi
utils::capture.output(seminr_model <- seminr::estimate_pls(mobi, mobi_mm, mobi_sm, inner_weights = path_weighting))
utils::capture.output(pred_mobi_pls_DA <- predict_pls(seminr_model,
                                                      technique = predict_DA,
                                                      noFolds = 10))

utils::capture.output(pred_mobi_pls_EA <- predict_pls(seminr_model,
                                                      technique = predict_EA,
                                                      noFolds = 10))

pred_sum_DA <- summary(pred_mobi_pls_DA, construct = "Satisfaction")
pred_sum_EA <- summary(pred_mobi_pls_EA, construct = "Satisfaction")

# Collect data to be tested
DA_item_metrics <- pred_sum_DA$item_evaluation

## Output originally created using following lines
# write.csv(pred_sum_DA$item_evaluation, file = "tests/fixtures/item_metrics_DA.csv")
# write.csv(pred_sum_EA$item_evaluation, file = "tests/fixtures/item_metrics_EA.csv")
# write.csv(c(pred_sum_DA$composite_accuracy$IS_RMSE,
#             pred_sum_DA$composite_accuracy$OOS_RMSE,
#             pred_sum_DA$composite_accuracy$IS_MAE,
#             pred_sum_DA$composite_accuracy$OOS_MAE), file = "tests/fixtures/composite_accuracy_DA.csv")
# write.csv(c(pred_sum_EA$composite_accuracy$IS_RMSE,
#             pred_sum_EA$composite_accuracy$OOS_RMSE,
#             pred_sum_EA$composite_accuracy$IS_MAE,
#             pred_sum_EA$composite_accuracy$OOS_MAE), file = "tests/fixtures/composite_accuracy_EA.csv")
# write.csv(c(pred_sum_DA$composite_accuracy$outliers,
#             pred_sum_EA$composite_accuracy$outliers), file = "tests/fixtures/outliers.csv")
# write.csv(pred_sum_DA$composite_validity$influential_cases, file = "tests/fixtures/influential_DA")
# write.csv(pred_sum_EA$composite_validity$influential_cases, file = "tests/fixtures/influential_EA")
# write.csv(pred_sum_DA$composite_validity$linear_model$coefficients, file = "tests/fixtures/lm_coefficients_DA")
# write.csv(pred_sum_EA$composite_validity$linear_model$coefficients, file = "tests/fixtures/lm_coefficients_EA")

# Load controls
DA_item_metrics_control <- as.matrix(read.csv("../fixtures/item_metrics_DA.csv", row.names = 1))
EA_item_metrics_control <- as.matrix(read.csv("../fixtures/item_metrics_EA.csv", row.names = 1))
DA_composite_accuracy_control <- as.matrix(read.csv("../fixtures/composite_accuracy_DA.csv", row.names = 1))
EA_composite_accuracy_control <- as.matrix(read.csv("../fixtures/composite_accuracy_EA.csv", row.names = 1))
outliers_control <- as.matrix(read.csv("../fixtures/outliers.csv", row.names = 1))
influential_cases_DA_control <- as.matrix(read.csv("../fixtures/influential_DA", row.names = 1))
influential_cases_EA_control <- as.matrix(read.csv("../fixtures/influential_EA", row.names = 1))
lm_coefficients_DA_control <- as.matrix(read.csv("../fixtures/lm_coefficients_DA", row.names = 1))
lm_coefficients_EA_control <- as.matrix(read.csv("../fixtures/lm_coefficients_EA", row.names = 1))

# Testing
test_that("PLSpredict calculates the PLS item RMSE & MAD IS correctly - DA", {
  diff <- mean(abs(pred_sum_DA$item_evaluation$PLS_item_predictive_metrics_IS - DA_item_metrics_control[1:2,1:13])/pred_sum_DA$item_evaluation$PLS_item_predictive_metrics_IS)
  expect_lt(diff, 0.02)
})

test_that("PLSpredict calculates the PLS item RMSE & MAD IS correctly - EA", {
  diff <- mean(abs(pred_sum_EA$item_evaluation$PLS_item_predictive_metrics_IS - EA_item_metrics_control[1:2,1:13])/pred_sum_EA$item_evaluation$PLS_item_predictive_metrics_IS)
  expect_lt(diff, 0.02)
})

test_that("PLSpredict calculates the PLS item RMSE & MAD OOS correctly - DA", {
  diff <- mean(abs(pred_sum_DA$item_evaluation$PLS_item_predictive_metrics_OOS - DA_item_metrics_control[1:2,14:26])/pred_sum_DA$item_evaluation$PLS_item_predictive_metrics_OOS)
  expect_lt(diff, 0.02)
})

test_that("PLSpredict calculates the PLS item RMSE & MAD OOS correctly - EA", {
  diff <- mean(abs(pred_sum_EA$item_evaluation$PLS_item_predictive_metrics_OOS - EA_item_metrics_control[1:2,14:26])/pred_sum_EA$item_evaluation$PLS_item_predictive_metrics_OOS)
  expect_lt(diff, 0.02)
})

test_that("PLSpredict calculates the LM item RMSE & MAD IS correctly - DA", {
  diff <- mean(abs(pred_sum_DA$item_evaluation$LM_item_predictive_metrics_IS - DA_item_metrics_control[1:2,27:29])/pred_sum_DA$item_evaluation$LM_item_predictive_metrics_IS)
  expect_lt(diff, 0.02)
})

test_that("PLSpredict calculates the LM item RMSE & MAD IS correctly - EA", {
  diff <- mean(abs(pred_sum_EA$item_evaluation$LM_item_predictive_metrics_IS - EA_item_metrics_control[1:2,27:29])/pred_sum_EA$item_evaluation$LM_item_predictive_metrics_IS)
  expect_lt(diff, 0.02)
})

test_that("PLSpredict calculates the LM item RMSE & MAD OOS correctly - DA", {
  diff <- mean(abs(pred_sum_DA$item_evaluation$LM_item_predictive_metrics_OOS - DA_item_metrics_control[1:2,30:32])/pred_sum_DA$item_evaluation$LM_item_predictive_metrics_OOS)
  expect_lt(diff, 0.02)
})

test_that("PLSpredict calculates the LM item RMSE & MAD OOS correctly - EA", {
  diff <- mean(abs(pred_sum_EA$item_evaluation$LM_item_predictive_metrics_OOS - EA_item_metrics_control[1:2,30:32])/pred_sum_EA$item_evaluation$LM_item_predictive_metrics_OOS)
  expect_lt(diff, 0.02)
})

test_that("PLSpredict calculates the PLS composite RMSE & MAD OOS correctly - DA", {
  diff <- mean(DA_composite_accuracy_control - c(pred_sum_DA$composite_accuracy$IS_RMSE, pred_sum_DA$composite_accuracy$OOS_RMSE, pred_sum_DA$composite_accuracy$IS_MAE, pred_sum_DA$composite_accuracy$OOS_MAE))
  expect_lt(diff, 0.02)
})

test_that("PLSpredict calculates the PLS composite RMSE & MAD OOS correctly - EA", {
  diff <- mean(EA_composite_accuracy_control - c(pred_sum_EA$composite_accuracy$IS_RMSE, pred_sum_EA$composite_accuracy$OOS_RMSE, pred_sum_EA$composite_accuracy$IS_MAE, pred_sum_EA$composite_accuracy$OOS_MAE))
  expect_lt(diff, 0.02)
})

test_that("PLSpredict calculates the PLS outliers correctly - DA & EA", {
  diff = sum(as.numeric(c(pred_sum_DA$composite_accuracy$outliers, pred_sum_EA$composite_accuracy$outliers)) - outliers_control)
  expect_equal(diff, 0)
})

test_that("PLSpredict calculates the PLS influential cases correctly - DA", {
  expect_equal(influential_cases_DA_control, as.matrix(pred_sum_DA$composite_validity$influential_cases))
})

test_that("PLSpredict calculates the PLS influential cases correctly - EA", {
  expect_equal(influential_cases_EA_control, as.matrix(pred_sum_EA$composite_validity$influential_cases))
})

test_that("PLSpredict calculates the PLS Validity Coefficients correctly - DA", {
  diff = mean(lm_coefficients_DA_control - as.matrix(pred_sum_DA$composite_validity$linear_model$coefficients))
  expect_lt(diff, 0.02)
})

test_that("PLSpredict calculates the PLS Validity Coefficients correctly - EA", {
  diff = mean(lm_coefficients_EA_control - as.matrix(pred_sum_EA$composite_validity$linear_model$coefficients))
  expect_lt(diff, 0.02)
})

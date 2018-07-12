context("PLSpredict correctly evaluates composite predictions\n")

# Test cases
library(seminr)
set.seed(123)
# Creating measurement model
mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5)),
  composite("Expectation",  multi_items("CUEX", 1:3)),
  composite("Quality",      multi_items("PERQ", 1:7)),
  composite("Value",        multi_items("PERV", 1:2))
)

# Creating structural model
mobi_sm <- relationships(
  paths(from = c("Image","Expectation","Quality"), to = c("Value"))
)

# Estimating the full model
utils::capture.output(mobi_pls <- seminr::estimate_pls(data = mobi,
                                 measurement_model = mobi_mm,
                                 structural_model = mobi_sm))

utils::capture.output(pred_mobi_pls_EA <- predict_pls(model = mobi_pls,
                                      technique = predict_EA,
                                      noFolds = 10))

utils::capture.output(pred_acc_EA <- composite_accuracy(pls_prediction_kfold = pred_mobi_pls_EA, construct = "Value"))
utils::capture.output(pred_val_EA <- composite_validity(pls_prediction_kfold = pred_mobi_pls_EA, construct = "Value"))

utils::capture.output(pred_mobi_pls_DA <- predict_pls(model = mobi_pls,
                                                               technique = predict_DA,
                                                               noFolds = 10))

utils::capture.output(pred_acc_DA <- composite_accuracy(pls_prediction_kfold = pred_mobi_pls_DA, construct = "Value"))
utils::capture.output(pred_val_DA <- composite_validity(pls_prediction_kfold = pred_mobi_pls_DA, construct = "Value"))

acc_matrix_EA <- pred_acc_EA$accuracy_matrix
IS_RMSE_EA <- pred_acc_EA$IS_RMSE
OOS_RMSE_EA <- pred_acc_EA$OOS_RMSE
OOS_MAE_EA <- pred_acc_EA$OOS_MAE
IS_MAE_EA <- pred_acc_EA$IS_MAE
acc_infl_cases_EA <- pred_acc_EA$outliers
acc_matrix_DA <- pred_acc_DA$accuracy_matrix
IS_RMSE_DA <- pred_acc_DA$IS_RMSE
OOS_RMSE_DA <- pred_acc_DA$OOS_RMSE
OOS_MAE_DA <- pred_acc_DA$OOS_MAE
IS_MAE_DA <- pred_acc_DA$IS_MAE
acc_infl_cases_DA <- pred_acc_DA$outliers

val_matrix_EA <- pred_val_EA$validity_matrix
val_coeff_EA <- pred_val_EA$linear_model$coefficients
val_infl_cases_EA <- pred_val_EA$influential_cases
val_matrix_DA <- pred_val_DA$validity_matrix
val_coeff_DA <- pred_val_DA$linear_model$coefficients
val_infl_cases_DA <- pred_val_DA$influential_cases

## Output originally created using following lines
# write.csv(acc_matrix_EA, file = "tests/fixtures/acc_matrix_EA.csv")
# write.csv(c(IS_RMSE_EA,OOS_RMSE_EA,OOS_MAE_EA,IS_MAE_EA ), file = "tests/fixtures/acc_metrics_EA.csv")
# write.csv(acc_infl_cases_EA, file = "tests/fixtures/acc_infl_cases_EA.csv")
# write.csv(acc_matrix_DA, file = "tests/fixtures/acc_matrix_DA.csv")
# write.csv(c(IS_RMSE_DA,OOS_RMSE_DA,OOS_MAE_DA,IS_MAE_DA ), file = "tests/fixtures/acc_metrics_DA.csv")
# write.csv(acc_infl_cases_DA, file = "tests/fixtures/acc_infl_cases_DA.csv")
# write.csv(val_matrix_EA, file = "tests/fixtures/val_matrix_EA.csv")
# write.csv(val_coeff_EA, file = "tests/fixtures/val_coeff_EA.csv")
# write.csv(val_infl_cases_EA, file = "tests/fixtures/val_infl_cases_EA.csv")
# write.csv(val_matrix_DA, file = "tests/fixtures/val_matrix_DA.csv")
# write.csv(val_coeff_DA, file = "tests/fixtures/val_coeff_DA.csv")
# write.csv(val_infl_cases_DA, file = "tests/fixtures/val_infl_cases_DA.csv")

# Load controls
acc_matrix_EA_control <- as.matrix(read.csv("../fixtures/acc_matrix_EA.csv", row.names = 1))
acc_metrics_EA_control <- as.matrix(read.csv("../fixtures/acc_metrics_EA.csv", row.names = 1))
acc_infl_cases_EA_control <- as.matrix(read.csv("../fixtures/acc_infl_cases_EA.csv", row.names = 1))
acc_matrix_DA_control <- as.matrix(read.csv("../fixtures/acc_matrix_DA.csv", row.names = 1))
acc_metrics_DA_control <- as.matrix(read.csv("../fixtures/acc_metrics_DA.csv", row.names = 1))
acc_infl_cases_DA_control <- as.matrix(read.csv("../fixtures/acc_infl_cases_DA.csv", row.names = 1))

val_matrix_EA_control <- as.matrix(read.csv("../fixtures/val_matrix_EA.csv", row.names = 1))
val_coeff_EA_control <- as.matrix(read.csv("../fixtures/val_coeff_EA.csv", row.names = 1))
val_infl_cases_EA_control <- as.matrix(read.csv("../fixtures/val_infl_cases_EA.csv", row.names = 1))
val_matrix_DA_control <- as.matrix(read.csv("../fixtures/val_matrix_DA.csv", row.names = 1))
val_coeff_DA_control <- as.matrix(read.csv("../fixtures/val_coeff_DA.csv", row.names = 1))
val_infl_cases_DA_control <- as.matrix(read.csv("../fixtures/val_infl_cases_DA.csv", row.names = 1))

# Testing

test_that("composite_accuracy() generates accuracy matrix EA correctly", {
  expect_equal(as.matrix(acc_matrix_EA), acc_matrix_EA_control)
})

test_that("composite_accuracy() generates accuracy matrix DA correctly", {
  expect_equal(as.matrix(acc_matrix_DA), acc_matrix_DA_control)
})

test_that("composite_accuracy() calculates metrics EA correctly", {
  expect_equal(c(IS_RMSE_EA,OOS_RMSE_EA,OOS_MAE_EA,IS_MAE_EA), as.vector(acc_metrics_EA_control))
})

test_that("composite_accuracy() calculates metrics DA correctly", {
  expect_equal(c(IS_RMSE_DA,OOS_RMSE_DA,OOS_MAE_DA,IS_MAE_DA), as.vector(acc_metrics_DA_control))
})

test_that("composite_accuracy() reports influential cases EA correctly", {
  expect_equal(acc_infl_cases_EA, as.character(as.vector(acc_infl_cases_EA_control)))
})

test_that("composite_accuracy() reports influential cases DA correctly", {
  expect_equal(acc_infl_cases_DA, as.character(as.vector(acc_infl_cases_DA_control)))
})

test_that("composite_validity() generates validity matrix EA correctly", {
  expect_equal(val_matrix_EA, as.data.frame(val_matrix_EA_control))
})

test_that("composite_validity() generates validity matrix DA correctly", {
  expect_equal(val_matrix_DA, as.data.frame(val_matrix_DA_control))
})

test_that("composite_validity() generates t-tests EA correctly", {
  expect_equal(as.vector(val_coeff_EA[1:2,]), as.vector(val_coeff_EA_control[1:2,]))
})

test_that("composite_validity() generates t-tests DA correctly", {
  expect_equal(as.vector(val_coeff_DA[1:2,]), as.vector(val_coeff_DA_control[1:2,]))
})

test_that("composite_validity() reports influential cases EA correctly", {
  expect_equal(val_infl_cases_EA, as.data.frame(val_infl_cases_EA_control))
})

test_that("composite_validity() reports influential cases DA correctly", {
  expect_equal(val_infl_cases_DA, as.data.frame(val_infl_cases_DA_control))
})


context("PLSpredict correctly generates predictions\n")

library(seminr)
set.seed(123)

# Test cases

# Creating measurement model
mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5)),
  composite("Expectation",  multi_items("CUEX", 1:3)),
  composite("Quality",      multi_items("PERQ", 1:7)),
  composite("Value",        multi_items("PERV", 1:2)),
  composite("Satisfaction", multi_items("CUSA", 1:3))
)

# Creating structural model
mobi_sm <- relationships(
  paths(from = c("Image","Expectation","Quality"), to = "Value"),
  paths(from = "Value", to = "Satisfaction")
)

# Estimating the full model
utils::capture.output(mobi_pls <- seminr::estimate_pls(data = mobi,
                                 measurement_model = mobi_mm,
                                 structural_model = mobi_sm))

utils::capture.output(pred_mobi_pls_EA <- predict_pls(model = mobi_pls,
                                      technique = predict_EA,
                                      noFolds = 10))

utils::capture.output(pred_mobi_pls_DA <- predict_pls(model = mobi_pls,
                                         technique = predict_DA,
                                         noFolds = 10))

out_of_sample_predictions_EA <- pred_mobi_pls_EA$composite_out_of_sample
in_sample_predictions_EA <- pred_mobi_pls_EA$composite_in_sample
actuals_star_EA <-  pred_mobi_pls_EA$actuals_star
out_of_sample_predictions_DA <- pred_mobi_pls_DA$composite_out_of_sample
in_sample_predictions_DA <- pred_mobi_pls_DA$composite_in_sample
actuals_star_DA <-  pred_mobi_pls_DA$actuals_star

## Output originally created using following lines
#write.csv(out_of_sample_predictions_EA, file = "tests/fixtures/out_of_sample_predictions_EA.csv")
#write.csv(in_sample_predictions_EA, file = "tests/fixtures/in_sample_predictions_EA.csv")
#write.csv(actuals_star_EA, file = "tests/fixtures/actuals_star_EA.csv")
#write.csv(out_of_sample_predictions_DA, file = "tests/fixtures/out_of_sample_predictions_DA.csv")
#write.csv(in_sample_predictions_DA, file = "tests/fixtures/in_sample_predictions_DA.csv")
#write.csv(actuals_star_DA, file = "tests/fixtures/actuals_star_DA.csv")

# Load controls
out_of_sample_predictions_EA_control <- as.matrix(read.csv("../fixtures/out_of_sample_predictions_EA.csv", row.names = 1))
in_sample_predictions_EA_control <- as.matrix(read.csv("../fixtures/in_sample_predictions_EA.csv", row.names = 1))
actuals_star_EA_control <- as.matrix(read.csv("../fixtures/actuals_star_EA.csv", row.names = 1))
out_of_sample_predictions_DA_control <- as.matrix(read.csv("../fixtures/out_of_sample_predictions_DA.csv", row.names = 1))
in_sample_predictions_DA_control <- as.matrix(read.csv("../fixtures/in_sample_predictions_DA.csv", row.names = 1))
actuals_star_DA_control <- as.matrix(read.csv("../fixtures/actuals_star_DA.csv", row.names = 1))

# Testing

test_that("generate_predictions() generates OOS composite predictions EA correctly", {
  expect_equal(out_of_sample_predictions_EA, out_of_sample_predictions_EA_control)
})

test_that("generate_predictions() generates IS composite predictions EA correctly", {
  expect_equal(in_sample_predictions_EA, in_sample_predictions_EA_control)
})

test_that("generate_predictions() generates actual star EA correctly", {
  expect_equal(actuals_star_EA, actuals_star_EA_control)
})

test_that("generate_predictions() generates OOS composite predictions DA correctly", {
  expect_equal(out_of_sample_predictions_DA, out_of_sample_predictions_DA_control)
})

test_that("generate_predictions() generates IS composite predictions DA correctly", {
  expect_equal(in_sample_predictions_DA, in_sample_predictions_DA_control)
})

test_that("generate_predictions() generates actual star DA correctly", {
  expect_equal(actuals_star_DA, actuals_star_DA_control)
})

# Order the actuals_star matrices

test_that("generate_predictions() generates actual star DA identical to actual star EA", {
  expect_equal(actuals_star_DA[order(rownames(actuals_star_DA)), ] , actuals_star_EA[order(rownames(actuals_star_EA)), ] )
})

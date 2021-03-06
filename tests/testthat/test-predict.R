context("PLSpredict correctly generates predictions\n")

# Test cases

# Creating measurement model
mobi_mm <- constructs(
  composite("Image",        multi_items("IMAG", 1:5)),
  composite("Expectation",  multi_items("CUEX", 1:3)),
  composite("Quality",      multi_items("PERQ", 1:7)),
  composite("Value",        multi_items("PERV", 1:2)),
  composite("Satisfaction", multi_items("CUSA", 1:3)),
  composite("Complaints",   single_item("CUSCO")),
  composite("Loyalty",      multi_items("CUSL", 1:3))
)

# Creating structural model
mobi_sm <- relationships(
  paths(from = "Image",        to = c("Expectation", "Satisfaction", "Loyalty")),
  paths(from = "Expectation",  to = c("Quality", "Value", "Satisfaction")),
  paths(from = "Quality",      to = c("Value", "Satisfaction")),
  paths(from = "Value",        to = c("Satisfaction")),
  paths(from = "Satisfaction", to = c("Complaints", "Loyalty")),
  paths(from = "Complaints",   to = "Loyalty")
)

# Estimating the full model
mobi_pls <- seminr::estimate_pls(data = mobi,
                         measurement_model = mobi_mm,
                         structural_model = mobi_sm)


#Slice data into training set and test set
trainData=mobi[1:200,]
testData=mobi[201:250,]


# Train the predictive model
utils::capture.output(mobi_pls_train <- seminr::estimate_pls(data = trainData,
                               measurement_model = mobi_mm,
                               structural_model = mobi_sm))

# Generate the predictions
utils::capture.output(mobi_pls_predict_DA <- predict(object = mobi_pls_train,
                                             testData = testData,
                                             technique = predict_DA))

# Generate the predictions
utils::capture.output(mobi_pls_predict_EA <- predict(object = mobi_pls_train,
                                             testData = testData,
                                             technique = predict_EA))

construct_predictions_DA <- mobi_pls_predict_DA$predicted_composite_scores
item_predictions_DA <- mobi_pls_predict_DA$predicted_items
construct_predictions_EA <- mobi_pls_predict_EA$predicted_composite_scores
item_predictions_EA <- mobi_pls_predict_EA$predicted_items


## Output originally created using following lines
# write.csv(construct_predictions_DA, file = "tests/fixtures/construct_predictions_DA.csv")
# write.csv(item_predictions_DA, file = "tests/fixtures/items_predictions_DA.csv")
# write.csv(construct_predictions_EA, file = "tests/fixtures/construct_predictions_EA.csv")
# write.csv(item_predictions_EA, file = "tests/fixtures/items_predictions_EA.csv")

# Load controls
construct_predictions_DA_control <- as.matrix(read.csv("../fixtures/construct_predictions_DA.csv", row.names = 1))
item_predictions_DA_control <- as.matrix(read.csv("../fixtures/items_predictions_DA.csv", row.names = 1))
construct_predictions_EA_control <- as.matrix(read.csv("../fixtures/construct_predictions_EA.csv", row.names = 1))
item_predictions_EA_control <- as.matrix(read.csv("../fixtures/items_predictions_EA.csv", row.names = 1))

# Testing

test_that("PLSpredict generates construct predictions DA correctly", {
  expect_equal(construct_predictions_DA, construct_predictions_DA_control)
})

test_that("PLSpredict generates item predictions DA correctly", {
  expect_equal(item_predictions_DA, item_predictions_DA_control)
})

test_that("PLSpredict generates construct predictions EA correctly", {
  expect_equal(construct_predictions_EA, construct_predictions_EA_control)
})

test_that("PLSpredict generates item predictions EA correctly", {
  expect_equal(item_predictions_EA, item_predictions_EA_control)
})

context("Summary correctly reports predictions and metrics for DA and EA\n")
pred_sum_DA <- summary(mobi_pls_predict_DA)
pred_sum_EA <- summary(mobi_pls_predict_EA)

summary_construct_predictions_DA <- pred_sum_DA$predicted_composite_scores
summary_item_predictions_DA <- pred_sum_DA$predicted_items
summary_construct_predictions_EA <- pred_sum_EA$predicted_composite_scores
summary_item_predictions_EA <- pred_sum_EA$predicted_items
summary_item_metrics_DA <- pred_sum_DA$item_predictive_metrics
summary_item_metrics_EA <- pred_sum_EA$item_predictive_metrics
summary_construct_metrics_DA <- pred_sum_DA$composite_predictive_metrics
summary_construct_metrics_EA <- pred_sum_EA$composite_predictive_metrics

# Score controls must be identical to earlier item and construct predictions
# Created metrics controls here
# write.csv(summary_item_metrics_DA, file = "tests/fixtures/summary_item_metrics_DA.csv")
# write.csv(summary_item_metrics_EA, file = "tests/fixtures/summary_item_metrics_EA.csv")
# write.csv(summary_construct_metrics_DA, file = "tests/fixtures/summary_construct_metrics_DA.csv")
# write.csv(summary_construct_metrics_EA, file = "tests/fixtures/summary_construct_metrics_EA.csv")

# Load controls
summary_item_metrics_DA_control <- as.matrix(read.csv("../fixtures/summary_item_metrics_DA.csv", row.names = 1))
summary_item_metrics_EA_control <- as.matrix(read.csv("../fixtures/summary_item_metrics_EA.csv", row.names = 1))
summary_construct_metrics_DA_control <- as.matrix(read.csv("../fixtures/summary_construct_metrics_DA.csv", row.names = 1))
summary_construct_metrics_EA_control <- as.matrix(read.csv("../fixtures/summary_construct_metrics_EA.csv", row.names = 1))

# Testing

test_that("Summary reports construct predictions DA correctly", {
  expect_equal(summary_construct_predictions_DA, construct_predictions_DA_control)
})

test_that("Summary reports item predictions DA correctly", {
  expect_equal(summary_item_predictions_DA, item_predictions_DA_control)
})

test_that("Summary reports construct predictions EA correctly", {
  expect_equal(summary_construct_predictions_EA, construct_predictions_EA_control)
})

test_that("Summary reports item predictions EA correctly", {
  expect_equal(summary_item_predictions_EA, item_predictions_EA_control)
})

test_that("Summary reports item predictive metrics DA correctly", {
  expect_equal(summary_item_metrics_DA, summary_item_metrics_DA_control)
})

test_that("Summary reports item predictive metrics EA correctly", {
  expect_equal(summary_item_metrics_EA, summary_item_metrics_EA_control)
})

test_that("Summary reports item predictive metrics DA correctly", {
  expect_equal(summary_construct_metrics_DA, summary_construct_metrics_DA_control)
})

test_that("Summary reports item predictive metrics EA correctly", {
  expect_equal(summary_construct_metrics_EA, summary_construct_metrics_EA_control)
})

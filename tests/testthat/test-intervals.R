context("PLSpredict correctly calculates the prediction intervals for DA technique\n")

# Test cases
## DA Approach
set.seed(4234)
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

# Load data, assemble model, and estimate using semPLS
trainData <- mobi[1:200,]
testData <- mobi[201:250,]
mobi_train <- seminr::estimate_pls(trainData, mobi_mm, interactions = NULL, mobi_sm, inner_weights = path_weighting)
prediction_intervals_DA <- bootstrap_prediction(model = mobi_train,
                                              testData = testData,
                                              technique = predict_DA,
                                              PIprobs = 0.95,
                                              noBoots=200)


## Output originally created using following lines
#write.csv(prediction_intervals_DA, file = "tests/fixtures/prediction_intervals_DA.csv")

# Load controls
prediction_intervals_DA_control <- as.matrix(read.csv("../fixtures/prediction_intervals_DA.csv", row.names = 1))

# Testing

test_that("bootstrap_prediction calculates the average case lower bound PI correctly", {
  diff <- sum(abs(prediction_intervals_DA$averageCasePI[501:550,1] - prediction_intervals_DA_control[1,501:550]))/sum(prediction_intervals_DA$averageCasePI[501:550,1])
  expect_lt(diff, 0.05)
})

test_that("bootstrap_prediction calculates the average case upper bound PI correctly", {
  diff <- sum(abs(prediction_intervals_DA$averageCasePI[501:550,2] - prediction_intervals_DA_control[2,501:550]))/sum(prediction_intervals_DA$averageCasePI[501:550,2])
  expect_lt(diff, 0.05)
})

test_that("bootstrap_prediction calculates the casewise lower bound PI correctly", {
  diff <- sum(abs(prediction_intervals_DA$caseWisePI[501:550,1] - prediction_intervals_DA_control[1,1151:1200]))/sum(prediction_intervals_DA$caseWisePI[501:550,1])
  expect_lt(diff, 0.08)
})

test_that("bootstrap_prediction calculates the casewise upper bound PI correctly", {
  diff <- sum(abs(prediction_intervals_DA$caseWisePI[501:550,2] - prediction_intervals_DA_control[2,1151:1200]))/sum(prediction_intervals_DA$caseWisePI[501:550,2])
  expect_lt(diff, 0.08)
})

context("PLSpredict correctly calculates the prediction intervals for EA technique\n")


prediction_intervals_EA <- bootstrap_prediction(model = mobi_train,
                                              testData = testData,
                                              technique = predict_EA,
                                              PIprobs = 0.95,
                                              noBoots=200)


## Output originally created using following lines
#write.csv(prediction_intervals_EA, file = "tests/fixtures/prediction_intervals_EA.csv")

# Load controls
prediction_intervals_EA_control <- as.matrix(read.csv("../fixtures/prediction_intervals_EA.csv", row.names = 1))

# Testing

test_that("bootstrap_prediction calculates the average case lower bound PI correctly", {
  diff <- sum(abs(prediction_intervals_EA$averageCasePI[501:550,1] - prediction_intervals_EA_control[1,501:550]))/sum(prediction_intervals_EA$averageCasePI[501:550,1])
  expect_lt(diff, 0.05)
})

test_that("bootstrap_prediction calculates the average case upper bound PI correctly", {
  diff <- sum(abs(prediction_intervals_EA$averageCasePI[501:550,2] - prediction_intervals_EA_control[2,501:550]))/sum(prediction_intervals_EA$averageCasePI[501:550,2])
  expect_lt(diff, 0.05)
})

test_that("bootstrap_prediction calculates the casewise lower bound PI correctly", {
  diff <- sum(abs(prediction_intervals_EA$caseWisePI[501:550,1] - prediction_intervals_EA_control[1,1151:1200]))/sum(prediction_intervals_EA$caseWisePI[501:550,1])
  expect_lt(diff, 0.08)
})

test_that("bootstrap_prediction calculates the casewise upper bound PI correctly", {
  diff <- sum(abs(prediction_intervals_EA$caseWisePI[501:550,2] - prediction_intervals_EA_control[2,1151:1200]))/sum(prediction_intervals_EA$caseWisePI[501:550,2])
  expect_lt(diff, 0.08)
})


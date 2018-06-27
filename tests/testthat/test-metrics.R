context("PLSpredict correctly calculates the PLS and LM metrics DA technique\n")

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

# Load data, assemble model, and estimate using semPLS
mobi <- mobi
seminr_model <- seminr::estimate_pls(mobi, mobi_mm, interactions = NULL, mobi_sm, inner_weights = path_weighting)
metrics_DA <- validatePredict(seminr_model,
                              technique = predict_DA,
                              noFolds = 10)


## Output originally created using following lines
#write.csv(metrics_DA, file = "tests/fixtures/metrics_DA.csv")

# Load controls
metrics_DA_control <- as.matrix(read.csv("../fixtures/metrics_DA.csv", row.names = 1))

# Testing

test_that("PLSpredict calculates the PLS RMSE correctly", {
  diff <- abs(metrics_DA$PLSRMSE - metrics_DA_control[,1:3])/metrics_DA$PLSRMSE
  expect_lt(diff[[1]], 0.05)
  expect_lt(diff[[2]], 0.05)
  expect_lt(diff[[3]], 0.05)
})

test_that("PLSpredict calculates the LM RMSE correctly", {
  diff <- abs(metrics_DA$LMRMSE - metrics_DA_control[,10:12])/metrics_DA$LMRMSE
  expect_lt(diff[[1]], 0.05)
  expect_lt(diff[[2]], 0.05)
  expect_lt(diff[[3]], 0.05)
})

test_that("PLSpredict calculates the PLS MAPE correctly", {
  diff <- abs(metrics_DA$PLSMAPE - metrics_DA_control[,4:6])/metrics_DA$PLSMAPE
  expect_lt(diff[[1]], 0.05)
  expect_lt(diff[[2]], 0.05)
  expect_lt(diff[[3]], 0.05)
})

test_that("PLSpredict calculates the LM MAPE correctly", {
  diff <- abs(metrics_DA$LMMAPE - metrics_DA_control[,13:15])/metrics_DA$LMMAPE
  expect_lt(diff[[1]], 0.05)
  expect_lt(diff[[2]], 0.05)
  expect_lt(diff[[3]], 0.05)
})

test_that("PLSpredict calculates the PLS MAD correctly", {
  diff <- abs(metrics_DA$PLSMAD - metrics_DA_control[,7:9])/metrics_DA$PLSMAD
  expect_lt(diff[[1]], 0.05)
  expect_lt(diff[[2]], 0.05)
  expect_lt(diff[[3]], 0.05)
})

test_that("PLSpredict calculates the LM MAD correctly", {
  diff <- abs(metrics_DA$LMMAD - metrics_DA_control[,16:18])/metrics_DA$LMMAD
  expect_lt(diff[[1]], 0.05)
  expect_lt(diff[[2]], 0.05)
  expect_lt(diff[[3]], 0.05)
})

context("PLSpredict correctly calculates the PLS and LM metrics EA technique\n")

# Test cases
## EA Approach

metrics_EA <- validatePredict(seminr_model,
                              technique = predict_EA,
                              noFolds = 10)


## Output originally created using following lines
#write.csv(metrics_EA, file = "tests/fixtures/metrics_EA.csv")

# Load controls
metrics_EA_control <- as.matrix(read.csv("../fixtures/metrics_EA.csv", row.names = 1))

# Testing

test_that("PLSpredict calculates the PLS RMSE correctly", {
  diff <- abs(metrics_EA$PLSRMSE - metrics_EA_control[,1:3])/metrics_EA$PLSRMSE
  expect_lt(diff[[1]], 0.05)
  expect_lt(diff[[2]], 0.05)
  expect_lt(diff[[3]], 0.05)
})

test_that("PLSpredict calculates the LM RMSE correctly", {
  diff <- abs(metrics_EA$LMRMSE - metrics_EA_control[,10:12])/metrics_EA$LMRMSE
  expect_lt(diff[[1]], 0.05)
  expect_lt(diff[[2]], 0.05)
  expect_lt(diff[[3]], 0.05)
})

test_that("PLSpredict calculates the PLS MAPE correctly", {
  diff <- abs(metrics_EA$PLSMAPE - metrics_EA_control[,4:6])/metrics_EA$PLSMAPE
  expect_lt(diff[[1]], 0.05)
  expect_lt(diff[[2]], 0.05)
  expect_lt(diff[[3]], 0.05)
})

test_that("PLSpredict calculates the LM MAPE correctly", {
  diff <- abs(metrics_EA$LMMAPE - metrics_EA_control[,13:15])/metrics_EA$LMMAPE
  expect_lt(diff[[1]], 0.05)
  expect_lt(diff[[2]], 0.05)
  expect_lt(diff[[3]], 0.05)
})

test_that("PLSpredict calculates the PLS MAD correctly", {
  diff <- abs(metrics_EA$PLSMAD - metrics_EA_control[,7:9])/metrics_EA$PLSMAD
  expect_lt(diff[[1]], 0.05)
  expect_lt(diff[[2]], 0.05)
  expect_lt(diff[[3]], 0.05)
})

test_that("PLSpredict calculates the LM MAD correctly", {
  diff <- abs(metrics_EA$LMMAD - metrics_EA_control[,16:18])/metrics_EA$LMMAD
  expect_lt(diff[[1]], 0.05)
  expect_lt(diff[[2]], 0.05)
  expect_lt(diff[[3]], 0.05)
})

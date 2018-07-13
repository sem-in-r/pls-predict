# Load SEMinR library
library(seminr)

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

#Slice data into training set and test set
index=sort(sample.int(dim(mobi)[1],50,replace=F))
trainData=mobi[-index,]
testData=mobi[index,]

# Estimating the full model
mobi_pls <- estimate_pls(data = mobi,
                         measurement_model = mobi_mm,
                         structural_model = mobi_sm)


# Generating predictions for the full model - k-fold
pred_mobi_pls <- predict_pls(mobi_pls,
                             technique = predict_DA,
                             noFolds = 10)

# Summarize the predict_pls results and print
pred_sum <- summary(pred_mobi_pls)
print(pred_sum)

# Plot the predict_pls results accuracy and validity plots
plot(pred_sum)
plot(pred_sum, constructs = "Loyalty")

# Summarize the results and print for one construct
pred_sum <- summary(pred_mobi_pls, construct = "Loyalty")
print(pred_sum)

# Train the predictive model
mobi_pls_train <- estimate_pls(data = trainData,
                               measurement_model = mobi_mm,
                               structural_model = mobi_sm)

# Generate the predictions
mobi_pls_predict <- predict(object = mobi_pls_train,
                            testData = testData,
                            technique = predict_DA)

# Summary of the predictions
summary(mobi_pls_predict)

#Call bootstrap_prediction (shortened number of bootstraps for demonstration)
PIntervals <- bootstrap_prediction(model = mobi_pls_train,
                                 testData = testData,
                                 technique = predict_DA,
                                 PIprobs = 0.95,
                                 noBoots = 500)
summary(PIntervals)
plot(summary(PIntervals))

summary(PIntervals, items = "CUSL1")
plot(summary(PIntervals, items = "CUSL1"))

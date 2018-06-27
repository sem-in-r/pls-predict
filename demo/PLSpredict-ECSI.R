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

# Estimating the full model
mobi_pls <- estimate_pls(data = mobi,
                         measurement_model = mobi_mm,
                         structural_model = mobi_sm)


#Slice data into training set and test set
index=sort(sample.int(dim(mobi)[1],83,replace=F))
trainData=mobi[-index,]
testData=mobi[index,]


# Train the predictive model
mobi_pls_train <- estimate_pls(data = trainData,
                               measurement_model = mobi_mm,
                               structural_model = mobi_sm)

# Generate the predictions
mobi_pls_predict <- PLSpredict(model = mobi_pls_train,
                               testData = testData,
                               technique = predict_EA)

# Return the predicted data
mobi_pls_predict

#Call predictionInterval (shortened number of bootstraps for demonstration)
PIntervals <- predictionInterval(model = mobi_pls_train,
                                 testData = testData,
                                 technique = predict_EA,
                                 PIprobs = 0.95,
                                 noBoots = 500)

#Call validatepredict
predictionMetrics <- validatePredict(mobi_pls, technique = predict_EA, noFolds=10)
predictionMetrics$PLSRMSE
predictionMetrics$LMRMSE
predictionMetrics$PLSMAPE
predictionMetrics$LMMAPE
predictionMetrics$PLSMAD
predictionMetrics$LMMAD

# Visualization of Prediction Intervals
##Create Holders & assign PI data
aveCUSL1 <- PIntervals$averageCasePI[[1]]
aveCUSL2 <- PIntervals$averageCasePI[[2]]
aveCUSL3 <- PIntervals$averageCasePI[[3]]
casewiseCUSL1 <- PIntervals$caseWisePI[[1]]
casewiseCUSL2 <- PIntervals$caseWisePI[[2]]
casewiseCUSL3 <- PIntervals$caseWisePI[[3]]

##Allocate and sort data - first by actual data and then by predicted data
dataholderCUSL1 <- cbind(t(aveCUSL1),mobi_pls_predict$predicted_Measurements[,"CUSL1"], mobi_pls_predict$testData[,"CUSL1"],t(casewiseCUSL1) )
dataholderCUSL2 <- cbind(t(aveCUSL2),mobi_pls_predict$predicted_Measurements[,"CUSL2"], mobi_pls_predict$testData[,"CUSL2"],t(casewiseCUSL2) )
dataholderCUSL3 <- cbind(t(aveCUSL3),mobi_pls_predict$predicted_Measurements[,"CUSL3"], mobi_pls_predict$testData[,"CUSL3"],t(casewiseCUSL3) )
CUSL1sorted <- dataholderCUSL1[order(dataholderCUSL1[,4], dataholderCUSL1[,3]) , ]
CUSL2sorted <- dataholderCUSL2[order(dataholderCUSL2[,4], dataholderCUSL2[,3]) , ]
CUSL3sorted <- dataholderCUSL3[order(dataholderCUSL3[,4], dataholderCUSL3[,3]) , ]

##Plot results function
###Item CUSL1 residuals
plot(mobi_pls_predict$testData[,"CUSL1"], mobi_pls_predict$residuals[,"CUSL1"],ylim = c(-10,10), ylab = "Residuals", xlab = "Actuals", main = "PLS Residuals for item CUSL1",pch = 16, col = rgb(0,0,0,0.2) )
abline(h = 0)
abline(h = predictionMetrics$PLSRMSE[,1], lty = 2)
abline(h = -predictionMetrics$PLSRMSE[,1], lty = 2)

### Item Y11 PLS Prediction Intervals
plot(NULL, xlim = c(1,nrow(CUSL1sorted)), ylim = c(0,12), ylab = "Ranges", xlab = "Cases", type = "n", main = "PLS Prediction Intervals for item CUSL1")
segments(c(1:83),CUSL1sorted[,5],c(1:83),CUSL1sorted[,6], col = 'lightgrey', lwd = 3)
segments(c(1:83),CUSL1sorted[,1],c(1:83),CUSL1sorted[,2], col = 'darkgrey', lwd = 3)
points(x = c(1:83), y = CUSL1sorted[,4],pch = 21, cex = 0.8, lwd = 2)
points(x = c(1:83), y = CUSL1sorted[,3],pch = 20, cex = 0.8)


evaluate_composite <- function(model, technique = predict_DA, noFolds = 10) {
  # collect model specs
  data <- model$data
  measurement_model <- model$mmMatrix
  interactions <- model$mobi_xm
  structural_model <- model$smMatrix
  inner_weights <- model$inner_weights
  constructs <- model$constructs


  # shuffle data
  data <- data[sample(nrow(data),nrow(data), replace = FALSE),]

  #Create 10 equally sized folds
  folds <- cut(seq(1,nrow(data)),breaks=noFolds,labels=FALSE)

  # Prepare matrices
  PLS_predicted_outsample <- matrix(0,nrow = nrow(data),ncol = length(constructs),dimnames = list(1:nrow(data),constructs))
  # In-sample predictions
  PLS_predicted_insample <- matrix(0,nrow = nrow(data),ncol = (noFolds*length(constructs)),dimnames = list(1:nrow(data),rep(constructs,noFolds)))

  for(x in 1:noFolds) {
    testIndexes <- which(folds==x,arr.ind=TRUE)
    trainIndexes <- which(folds!=x,arr.ind=TRUE)
    testingData <- data[testIndexes, ]
    trainingData <- data[-testIndexes, ]

    #PLS prediction on testset model
    train_model <- seminr::estimate_pls(data = trainingData,
                                        measurement_model = measurement_model,
                                        interactions = interactions,
                                        structural_model = structural_model,
                                        inner_weights = inner_weights)
    test_predictions <- PLSpredict(model = train_model,
                             testData = testingData,
                             technique = technique)

    PLS_predicted_outsample[testIndexes,] <-  test_predictions$predicted_CompositeScores

    #PLS prediction on trainset model
    train_predictions <- PLSpredict(model = train_model,
                                    testData = trainingData,
                                    technique = technique)
    PLS_predicted_insample[trainIndexes,(((x-1)*length(constructs))+1):(x*length(constructs))] <- train_predictions$predicted_CompositeScores
  }
  # Collect the relevant data
  average_insample <- matrix(0,nrow = nrow(data), ncol = length(constructs), dimnames = list(1:nrow(data),constructs))
  for (z in 1:length(constructs)) {
    average_insample[,z] <- rowSums(PLS_predicted_insample[,(0:(noFolds-1)*length(constructs))+z])/(noFolds-1)
  }

  results <- list(composite_out_of_sample_predictions = PLS_predicted_outsample,
                  composite_in_sample_predictions = average_insample)
  class(results) <- "PLSpredictions"
  return(results)
}

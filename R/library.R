# function to subset a smMatrix by construct(x)
subset_by_construct <- function(x, smMatrix) {
  smMatrix[smMatrix[,"source"] == c(x), "target"]
}

# Function to check whether a named construct's antecedents occur in a list
construct_antecedent_in_list <- function(x,list, smMatrix) {
  all(smMatrix[smMatrix[,"target"]==x,"source"] %in% list)
}

# Function to iterate over a vector of constructs and return the antecedents each construct depends on
depends_on <- function(constructs_vector, smMatrix) {
  return(unique(unlist(sapply(constructs_vector, subset_by_construct, smMatrix = smMatrix), use.names = FALSE)))
}

# Function to iterate over a vector of constructs and check whether their antecedents occur in a list
antecedents_in_list <- function(constructs_vector, list,smMatrix) {
  as.logical(sapply(constructs_vector, construct_antecedent_in_list, list = list, smMatrix = smMatrix))
}

# Function to organize order of endogenous constructs from most exogenous forwards
construct_order <- function(smMatrix) {

  # get purely endogenous and purely exogenous
  only_endogenous <- setdiff(unique(smMatrix[,2]), unique(smMatrix[,1]))
  only_exogenous <- setdiff(unique(smMatrix[,1]), unique(smMatrix[,2]))

  # get construct names
  construct_names <- unique(c(smMatrix[,1],smMatrix[,2]))

  # get all exogenous constructs
  all_exogenous_constructs <- setdiff(construct_names, only_endogenous)

  # initialize construct order with first purely exogenous construct
  construct_order <- only_exogenous

  # Iterate over constructs to generate construct_order
  while (!setequal(all_exogenous_constructs,construct_order)) {
    construct_order <- c(construct_order,setdiff(depends_on(construct_order,smMatrix)[antecedents_in_list(depends_on(construct_order,smMatrix), construct_order, smMatrix)], construct_order))
  }

  # return the order of endogenous constructs to be predicted
  final_list <- setdiff(construct_order,only_exogenous)
  return(c(final_list,only_endogenous))

}

# Function to standardize a matrix by sd vector and mean vector
standardize_data <- function(data_matrix,means_vector,sd_vector) {
  return(t(t(sweep(data_matrix,2,means_vector)) / sd_vector))
}

# Function to un-standardize a matrix by sd vector and mean vector
unstandardize_data <- function(data_matrix,means_vector,sd_vector) {
  return(sweep((data_matrix %*% diag(sd_vector)),2,means_vector,"+"))
}

# Function to sum rows of a matrix
sum_rows <- function(x, matrix, noFolds, constructs) {
  return(rowSums(matrix[,(0:(noFolds-1)*length(constructs))+x]))
}

# Function to mean rows of a matrix
mean_rows <- function(x, matrix, noFolds, constructs) {
  return(rowSums(matrix[,(0:(noFolds-1)*length(constructs))+x])/(noFolds-1))
}

# Function to return train and test predictions for a model
in_and_out_sample_predictions <- function(x, folds, ordered_data, model,technique) {
  testIndexes <- which(folds==x,arr.ind=TRUE)
  trainIndexes <- which(folds!=x,arr.ind=TRUE)
  testingData <- ordered_data[testIndexes, ]
  trainingData <- ordered_data[-testIndexes, ]

  # Create matrices for return data
  PLS_predicted_outsample_construct <- matrix(0,nrow = nrow(ordered_data),ncol = length(model$constructs),dimnames = list(1:nrow(ordered_data),model$constructs))
  PLS_predicted_insample_construct <- matrix(0,nrow = nrow(ordered_data),ncol = length(model$constructs),dimnames = list(1:nrow(ordered_data),model$constructs))
  PLS_predicted_outsample_item <- matrix(0,nrow = nrow(ordered_data),ncol = length(model$mmVariables),dimnames = list(1:nrow(ordered_data),model$mmVariables))
  PLS_predicted_insample_item <- matrix(0,nrow = nrow(ordered_data),ncol = length(model$mmVariables),dimnames = list(1:nrow(ordered_data),model$mmVariables))

  #PLS prediction on testset model
  utils::capture.output(train_model <- seminr::estimate_pls(data = trainingData,
                                      measurement_model = model$mmMatrix,
                                      interactions = model$mobi_xm,
                                      structural_model = model$smMatrix,
                                      inner_weights = model$inner_weights))
  test_predictions <- stats::predict(object = train_model,
                                     testData = testingData,
                                     technique = technique)

  PLS_predicted_outsample_construct[testIndexes,] <-  test_predictions$predicted_composite_scores
  PLS_predicted_outsample_item[testIndexes,] <- test_predictions$predicted_items

  #PLS prediction on trainset model
  train_predictions <- stats::predict(object = train_model,
                                      testData = trainingData,
                                      technique = technique)
  PLS_predicted_insample_construct[trainIndexes,] <- train_predictions$predicted_composite_scores
  PLS_predicted_insample_item[trainIndexes,] <- train_predictions$predicted_items
  return(list(PLS_predicted_insample = PLS_predicted_insample_construct,
         PLS_predicted_outsample = PLS_predicted_outsample_construct,
         PLS_predicted_insample_item = PLS_predicted_insample_item,
         PLS_predicted_outsample_item = PLS_predicted_outsample_item))
}

# Function to collect and parse prediction matrices
prediction_matrices <- function(folds, noFolds, ordered_data, model,technique) {
  # create prediction matrices
  matrices <- sapply(1:noFolds, in_and_out_sample_predictions, folds = folds,ordered_data = ordered_data, model = model, technique = technique)
  # collect the odd and even numbered matrices from the matrices return object
  in_sample_construct_matrix <- do.call(cbind, matrices[(1:(noFolds*4))[1:(noFolds*4)%%4==1]])
  out_sample_construct_matrix <- do.call(cbind, matrices[(1:(noFolds*4))[1:(noFolds*4)%%4==2]])
  in_sample_item_matrix <- do.call(cbind, matrices[(1:(noFolds*4))[1:(noFolds*4)%%4==3]])
  out_sample_item_matrix <- do.call(cbind, matrices[(1:(noFolds*4))[1:(noFolds*4)%%4==0]])

  # mean the in-sample construct predictions by row
  average_insample_construct <- sapply(1:length(model$constructs), mean_rows, matrix = in_sample_construct_matrix,
                                                                   noFolds = noFolds,
                                                                   constructs = model$constructs)

  # mean the in-sample item predictions by row
  average_insample_item <- sapply(1:length(model$mmVariables), mean_rows, matrix = in_sample_item_matrix,
                                       noFolds = noFolds,
                                       constructs = model$mmVariables)

  # sum the out-sample construct predictions by row
  average_outsample_construct <- sapply(1:length(model$constructs), sum_rows, matrix = out_sample_construct_matrix,
                                                                    noFolds = noFolds,
                                                                    constructs = model$constructs)

  # sum the out-sample item predictions by row
  average_outsample_item <- sapply(1:length(model$mmVariables), sum_rows, matrix = out_sample_item_matrix,
                                        noFolds = noFolds,
                                        constructs = model$mmVariables)



  colnames(average_insample_construct) <- colnames(average_outsample_construct) <- model$constructs

  return(list(out_of_sample_construct = average_outsample_construct,
              in_sample_construct = average_insample_construct,
              out_of_sample_item = average_outsample_item,
              in_sample_item = average_insample_item))
}

# Function to return the RMSE and MAE of a score
prediction_metrics <- function(residuals) {
  RMSE <- sqrt(mean(residuals^2))
  MAE <- mean(abs(residuals))
  return(matrix(c(RMSE,MAE), nrow = 2, ncol = 1, byrow = TRUE))
}

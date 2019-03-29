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
  PLS_predicted_outsample_construct <- matrix(0,nrow = nrow(ordered_data),ncol = length(model$constructs),dimnames = list(rownames(ordered_data),model$constructs))
  PLS_predicted_insample_construct <- matrix(0,nrow = nrow(ordered_data),ncol = length(model$constructs),dimnames = list(rownames(ordered_data),model$constructs))
  PLS_predicted_outsample_item <- matrix(0,nrow = nrow(ordered_data),ncol = length(model$mmVariables),dimnames = list(rownames(ordered_data),model$mmVariables))
  PLS_predicted_insample_item <- matrix(0,nrow = nrow(ordered_data),ncol = length(model$mmVariables),dimnames = list(rownames(ordered_data),model$mmVariables))
  PLS_predicted_insample_item_residuals <- matrix(0,nrow = nrow(ordered_data),ncol = length(model$mmVariables),dimnames = list(rownames(ordered_data),model$mmVariables))
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
  PLS_predicted_insample_item_residuals[trainIndexes,] <- as.matrix(train_predictions$item_residuals)

  ## Perform prediction on LM models for benchmark
  # Identify endogenous items
  endogenous_items <- unlist(sapply(unique(model$smMatrix[,2]), function(x) model$mmMatrix[model$mmMatrix[, "construct"] == x,"measurement"]), use.names = FALSE)

  #LM Matrices
  lm_holder <- sapply(unique(model$smMatrix[,2]), generate_lm_predictions, model = model,
                          ordered_data = ordered_data[,model$mmVariables],
                          testIndexes = testIndexes,
                          endogenous_items = endogenous_items,
                          trainIndexes = trainIndexes)

  lmprediction_in_sample <- matrix(0, ncol = 0 , nrow = length(trainIndexes))
  lmprediction_out_sample <- matrix(0, ncol = 0 , nrow = length(testIndexes))
  lmprediction_in_sample_residuals <- matrix(0,nrow=nrow(ordered_data),ncol=length(endogenous_items),byrow =TRUE,dimnames = list(rownames(ordered_data),endogenous_items))

  # collect the odd and even numbered matrices from the matrices return object
  lmprediction_in_sample <- do.call(cbind, lm_holder[((1:(length(unique(model$smMatrix[,2]))*2))[1:(length(unique(model$smMatrix[,2]))*2)%%2==1])])
  lmprediction_out_sample <- do.call(cbind, lm_holder[((1:(length(unique(model$smMatrix[,2]))*2))[1:(length(unique(model$smMatrix[,2]))*2)%%2==0])])
  lmprediction_in_sample_residuals[trainIndexes,] <- as.matrix(ordered_data[trainIndexes,endogenous_items]) - lmprediction_in_sample[trainIndexes,endogenous_items]

  return(list(PLS_predicted_insample = PLS_predicted_insample_construct,
         PLS_predicted_outsample = PLS_predicted_outsample_construct,
         PLS_predicted_insample_item = PLS_predicted_insample_item,
         PLS_predicted_outsample_item = PLS_predicted_outsample_item,
         LM_predicted_insample_item = lmprediction_in_sample,
         LM_predicted_outsample_item = lmprediction_out_sample,
         PLS_predicted_insample_item_residuals = PLS_predicted_insample_item_residuals,
         LM_predicted_insample_item_residuals = lmprediction_in_sample_residuals))
}

# Function to collect and parse prediction matrices
prediction_matrices <- function(noFolds, ordered_data, model,technique, cores) {
  out <- tryCatch(
    {
    # If noFolds is NULL, perform parallel LOOCV
    if (is.null(noFolds)) {
      # Automatically perform LOOCV if number of folds not specified
      noFolds = nrow(model$data)
      #Create noFolds equally sized folds
      folds <- cut(seq(1,nrow(ordered_data)),breaks=noFolds,labels=FALSE)

      # Create cluster
      suppressWarnings(ifelse(is.null(cores), cl <- parallel::makeCluster(parallel::detectCores()), cl <- parallel::makeCluster(cores)))

      #generate_lm_predictions <- PLSpredict:::generate_lm_predictions
      #predict_lm_matrices <- PLSpredict:::predict_lm_matrices
      # Export variables and functions to cluster
      parallel::clusterExport(cl=cl, varlist=c("generate_lm_predictions",
                                               "predict_lm_matrices"), envir=environment())
      #parallel::clusterEvalQ(cl=cl,expr = "library(PLSpredict)")

      # Execute the bootstrap
      utils::capture.output(matrices <- parallel::parSapply(cl,1:noFolds,in_and_out_sample_predictions,folds = folds,
                                                            ordered_data = ordered_data,
                                                            model = model,
                                                            technique = technique))
      # Stop cluster
      parallel::stopCluster(cl)
    } else {
      #Create noFolds equally sized folds
      folds <- cut(seq(1,nrow(ordered_data)),breaks=noFolds,labels=FALSE)
      matrices <- sapply(1:noFolds, in_and_out_sample_predictions, folds = folds,ordered_data = ordered_data, model = model, technique = technique)
    }

    # collect the odd and even numbered matrices from the matrices return object
    in_sample_construct_matrix <- do.call(cbind, matrices[(1:(noFolds*8))[1:(noFolds*8)%%8==1]])
    out_sample_construct_matrix <- do.call(cbind, matrices[(1:(noFolds*8))[1:(noFolds*8)%%8==2]])
    in_sample_item_matrix <- do.call(cbind, matrices[(1:(noFolds*8))[1:(noFolds*8)%%8==3]])
    out_sample_item_matrix <- do.call(cbind, matrices[(1:(noFolds*8))[1:(noFolds*8)%%8==4]])
    in_sample_lm_matrix <- do.call(cbind, matrices[(1:(noFolds*8))[1:(noFolds*8)%%8==5]])
    out_sample_lm_matrix <- do.call(cbind, matrices[(1:(noFolds*8))[1:(noFolds*8)%%8==6]])
    PLS_in_sample_item_residuals <- do.call(cbind, matrices[(1:(noFolds*8))[1:(noFolds*8)%%8==7]])
    LM_in_sample_item_residuals <- do.call(cbind, matrices[(1:(noFolds*8))[1:(noFolds*8)%%8==0]])

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

    # square the out-sample pls residuals, mean them and take the square root
    average_insample_pls_item_residuals <- sqrt(sapply(1:length(model$mmVariables), mean_rows, matrix = PLS_in_sample_item_residuals^2,
                                                   noFolds = noFolds,
                                                   constructs = model$mmVariables))
    # Collect endogenous items
    endogenous_items <- unlist(sapply(unique(model$smMatrix[,2]), function(x) model$mmMatrix[model$mmMatrix[, "construct"] == x,"measurement"]), use.names = FALSE)

    # mean the in-sample lm predictions by row
    average_insample_lm <- sapply(1:length(endogenous_items), mean_rows, matrix = in_sample_lm_matrix,
                                  noFolds = noFolds,
                                  constructs = endogenous_items)

    # sum the out-sample item predictions by row
    average_outsample_lm <- sapply(1:length(endogenous_items), sum_rows, matrix = out_sample_lm_matrix,
                                     noFolds = noFolds,
                                     constructs = endogenous_items)

    # square the out-sample lm residuals, mean them, and take square root
    average_insample_lm_item_residuals <- sqrt(sapply(1:length(endogenous_items), mean_rows, matrix = LM_in_sample_item_residuals^2,
                                                       noFolds = noFolds,
                                                       constructs = endogenous_items))

    colnames(average_insample_construct) <- colnames(average_outsample_construct) <- model$constructs
    colnames(average_insample_item) <- colnames(average_insample_pls_item_residuals) <- colnames(average_outsample_item) <- model$mmVariables
    colnames(average_insample_lm) <- colnames(average_outsample_lm) <- colnames(average_insample_lm_item_residuals) <- endogenous_items

    return(list(out_of_sample_construct = average_outsample_construct,
                in_sample_construct = average_insample_construct,
                out_of_sample_item = average_outsample_item,
                in_sample_item = average_insample_item,
                out_of_sample_lm_item = average_outsample_lm,
                in_sample_lm_item = average_insample_lm,
                pls_in_sample_item_residuals = average_insample_pls_item_residuals,
                lm_in_sample_item_residuals = average_insample_lm_item_residuals))
    },
    error=function(cond) {
      message("Parallel encountered this ERROR: ")
      message(cond)
      parallel::stopCluster(cl)
      return(NULL)
    },
    warning=function(cond) {
      message("Parallel encountered this WARNING:")
      message(cond)
      parallel::stopCluster(cl)
      return(NULL)
    },
    finally={
      #
    }
  )
}

# Function to return the RMSE and MAE of a score
prediction_metrics <- function(residuals) {
  RMSE <- sqrt(mean(residuals^2))
  MAE <- mean(abs(residuals))
  return(matrix(c(RMSE,MAE), nrow = 2, ncol = 1, byrow = TRUE))
}

predict_lm_matrices <- function(x, depTrainData, indepTrainData,indepTestData, endogenous_items) {
  # Train LM
  trainLM <- stats::lm(depTrainData[,x] ~ ., indepTrainData)
  # Predict out of sample
  lmprediction_out_sample <- stats::predict(trainLM, newdata = indepTestData)
  # Predict in sample
  lmprediction_in_sample <- stats::predict(trainLM, newdata = indepTrainData)
  return(list(lm_prediction_in_sample = lmprediction_in_sample,
              lm_prediction_out_sample = lmprediction_out_sample))
}

generate_lm_predictions <- function(x, model, ordered_data, testIndexes, endogenous_items, trainIndexes) {
  # Extract the target and non-target variables for Linear Model
  dependant_items <- model$mmMatrix[model$mmMatrix[,1] == x,2]

  # Create matrix return object holders
  in_sample_matrix <- matrix(0,nrow = nrow(ordered_data), ncol = length(dependant_items), dimnames = list(rownames(ordered_data),dependant_items))
  out_sample_matrix <- matrix(0,nrow = nrow(ordered_data), ncol = length(dependant_items), dimnames = list(rownames(ordered_data),dependant_items))

  # Exclude dependant items from independant matrix
  independant_matrix <- ordered_data[ , -which(names(ordered_data) %in% dependant_items)]
  dependant_matrix <- as.matrix(ordered_data[,dependant_items])

  # Create independant items matrices - training and testing
  indepTestData <- independant_matrix[testIndexes, ]
  indepTrainData <- independant_matrix[-testIndexes, ]

  # Create dependant matrices - training and testing
  #####
  if (length(testIndexes) == 1) {
    depTestData <- t(as.matrix(dependant_matrix[testIndexes, ]))
  } else {
    depTestData <- as.matrix(dependant_matrix[testIndexes, ])
  }
  #####
  #depTestData <- as.matrix(dependant_matrix[testIndexes, ])
  depTrainData <- as.matrix(dependant_matrix[-testIndexes, ])
  colnames(depTrainData) <- colnames(depTestData) <- dependant_items

  lm_prediction_list <- sapply(dependant_items, predict_lm_matrices, depTrainData = depTrainData,
                               indepTrainData = indepTrainData,
                               indepTestData = indepTestData,
                               endogenous_items = endogenous_items)
  in_sample_matrix[trainIndexes,] <- matrix(unlist(lm_prediction_list[(1:length(lm_prediction_list))[1:length(lm_prediction_list)%%2==1]]), ncol = length(dependant_items), nrow = nrow(depTrainData), dimnames = list(rownames(depTrainData),dependant_items))
  out_sample_matrix[testIndexes,] <- matrix(unlist(lm_prediction_list[(1:length(lm_prediction_list))[1:length(lm_prediction_list)%%2==0]]), ncol = length(dependant_items), nrow = nrow(depTestData), dimnames = list(rownames(depTestData),dependant_items))

  return(list(in_sample_matrix, out_sample_matrix))
}

# Function to calculate HPD quantiles from a matrix of predictions
calculate_hpd_quantiles <- function(mmVariables, testData, bootmatrix, PIprobs) {
  # subset matrix
  matrix <- bootmatrix[1:(nrow(testData)*length(mmVariables)), ]
  # calculate quantiles
  prediction_intervals <- t(apply(matrix,1, function(x) TeachingDemos::emp.hpd(x,conf = PIprobs)))

  # # rename the rows and columns and list items
  named_prediction_intervals <- lapply((1:length(mmVariables) * nrow(testData)), collect_pis, testData, prediction_intervals,PIprobs)
  names(named_prediction_intervals) <- mmVariables
  return(named_prediction_intervals)
}

# Function to collect prediction intervals into list object
collect_pis <- function(x, testData, prediction_intervals,PIprobs) {
  PI <- prediction_intervals[(x - nrow(testData)+1):x,1:2]
  rownames(PI) <- rownames(testData)
  colnames(PI) <- c(paste("Lower_",PIprobs*100,"%_bound", sep = ""),paste("Upper_",PIprobs*100,"%_bound", sep = ""))
  return(PI)
}

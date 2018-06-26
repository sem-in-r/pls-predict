# Function to organize order of endogenous constructs from most exogenous forwards
construct_order <- function(smMatrix) {
  depends_on <- function(latents) {
    ret <- c()
    for (latent in latents) {
      ret <- c(ret,smMatrix[smMatrix[,"source"] == c(latent), "target"])
    }
    unique(ret)
  }
  
  has_score <- function(latents, list) {
    ret <- c()
    for (latent in latents) {
      next_val <- all(smMatrix[smMatrix[,"target"]==latent,"source"] %in% list)
      ret <- c(ret,next_val)
    }
    ret
  }
  
  # get purely endogenous and purely exogenous
  only_endogenous <- setdiff(unique(smMatrix[,2]), unique(smMatrix[,1]))
  only_exogenous <- setdiff(unique(smMatrix[,1]), unique(smMatrix[,2]))
  # get ltVariables 
  ltVariables <- unique(c(smMatrix[,1],smMatrix[,2]))
  
  exogenous_latent <- setdiff(ltVariables, only_endogenous)
  scores_list <- only_exogenous
  while (!setequal(exogenous_latent,scores_list)) { 
    scores_list <- c(scores_list,setdiff(depends_on(scores_list)[has_score(depends_on(scores_list), scores_list)], scores_list))
  }
  
  final_list <- setdiff(scores_list,only_exogenous)
  
return(c(final_list,only_endogenous))
  
}

predict_EA <- function(smMatrix, path_coef, construct_scores) {
  order <- construct_order(smMatrix)
  only_exogenous <- setdiff(unique(smMatrix[,1]), unique(smMatrix[,2]))
  return_matrix <- construct_scores
  return_matrix[,setdiff(colnames(return_matrix),only_exogenous)] <- 0
  for (latent in order) {
    return_matrix[,latent] <- return_matrix %*% path_coef[,latent]
    
  }  
 return_matrix 
}

predict_DA <- function(smMatrix, path_coef, construct_scores) {
  return_matrix <- construct_scores%*%path_coef
  return_matrix
}
######## WTF: ################################


# function to get measurement mode of a latent (first item)
measure_mode <- function(latent,mmMatrix) {
  mmMatrix[mmMatrix[,"latent"]==latent,"type"][1]
}

# function to get all the items of a given measurement mode for a given latent
items_per_mode <- function(latent, mode,mmMatrix) {
  latentmatrix <- mmMatrix[mmMatrix[,"latent"]==latent,c("measurement","type")]
  if(class(latentmatrix) == "matrix") {
    return(latentmatrix[latentmatrix[,"type"] == mode,"measurement"])
  }
  if (class(latentmatrix) == "character") {
    latentmatrix = t(as.matrix(latentmatrix))
    return(latentmatrix[latentmatrix[,"type"] == mode,"measurement"])
  }
}

# function to subset and return the mmMatrix for a latent
mmMatrix_per_latent <- function(latent, mmMatrix) {
  latentmatrix <- mmMatrix[mmMatrix[,"latent"]==latent,c("latent","measurement","type")]
  if(class(latentmatrix) == "matrix") {
    return(latentmatrix)
  }
  if (class(latentmatrix) == "character") {
    latentmatrix = t(as.matrix(latentmatrix))
    return(latentmatrix)
  }
}

# This function is not used!!
# Function to estimate r_squared for endogenous constructs
estimate_Rsquared <- function(smMatrix,fscores) {
  #Calculate R Squared
  #Get smMatrix
  modelMatrix <- data.frame(smMatrix)
  #Get endogenous composites
  uniquetarget <- as.character(unique(modelMatrix$target))
  #Get composite scores
  valuesMatrix <- fscores
  #Calculate Linear Models
  lmmodels <- lapply(uniquetarget, function(x) {lm(as.formula(paste(x,"~ .", sep = "")),
                                                   data = data.frame(valuesMatrix[,colnames(valuesMatrix) %in%
                                                                                    c(x,as.character(modelMatrix$source[which(modelMatrix$target==x)]))]))})
  #Initialize matrix holder for Rsquared values
  rSquared <- matrix(,nrow=1,ncol=length(uniquetarget),byrow =TRUE,dimnames = list(1,uniquetarget))

  # Iterate and extract every R^2 value
  for (i in 1:length(lmmodels)) {
    rSquared[,i] <- summary(lmmodels[[i]])$r.squared
  }
  return(rSquared)
}

# Function to create a named vector of path coefficients
transform_to_named_vector <- function(results,independant) {
  coefficients <- as.vector(results)
  if(!is.null(rownames(results))) {
    names(coefficients)<-rownames(results)
  } else if (!is.null(names(results))) {
    names(coefficients)<-names(results)
  } else {
    names(coefficients)<-independant
  }
  return(coefficients)
}

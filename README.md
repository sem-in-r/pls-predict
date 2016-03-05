# PLSpredict

## Introduction
PLSpredict is a library of tools to perform Partial Least Squares Path Modelling and Prediction in R.   
This readme serves to explain the working of the PLSpredict package and to provide an example as to the intended use. To that end, we have provided a sample dataset, AnimData.csv, to which we will refer in the text.

## Theory
The popularity of PLS chiefly derives from the its non-parametric estimation of complex models - not requiring the onerous distributional and other constraints of traditional parametric methods.

## Features 
### simplePLS()
A function which takes as arguments the training dataset, the structural model in a matrix format and the measurement model in matrix format. This function takes the supplied data and generates the PLSPM object consisting of the: 
- normalized measures of exogenous factors
- measurement weights
- scores of exogenous factors
- path coefficients
- scores of endogenous factors
- error of endogenous factors
- factor loadings and
- normalized measures of endogenous factors

### PLSpredict()
A function which takes as arguments the pls model generated from the simplePLS() function and the test data on which to make the predictions. This function returns the testdata, the predicted values for the test data and the residuals for the predictions.

### AnimData.csv
A sample dataset with which to illustrate the use of the package.

## Walk Through
### Preparation of the data
We will be using the dataset AnimData provided in the simplePLS package to illustrate the usage of the function. The dataset is first sliced into a training set and testing set.

``` 
Anime=read.csv("./data/AnimData.csv",header=T)
set.seed(123)
index=sample.int(dim(Anime)[1],83,replace=F)
trainData=Anime[-index,]
testData=Anime[index,]
```
### Creation of the simplePLS model:
Initially the PLS model consisting of a structural model and measurement model has to be defined.  

#### Structural model.
We have defined the structural model as consisting of 2 exogenous antecedent factors: Perceived Visual Complexity and Arousal and 1 endogenous outcome factor: Approach/Avoidance. The model takes this form:

<INSERT IMAGE OF STRUCTURAL MODEL HERE>

In R our model is described by creating a 2 by 2 matrix and initializing this as matrix object smMatrix. The source column identifies the antecedent factor and the target column the outcome factor. This matrix is limited to 2 column width, but can have any number of antecedent and outcome factor combinations to accurately describe the proposed structural model. Indeed, one can even model interactions and moderations in this way.
 
``` 
#Create the Matrix of the Structural Model
smMatrix <- matrix(c("Perceived Visual Complexity", "Aproach/Avoidance",
                     "Arousal","Aproach/Avoidance"),nrow=2,ncol=2,byrow =TRUE,
                   dimnames = list(1:2,c("source","target")))
```

<INSERT TABLE OF STRUCTURAL MATRIX HERE>

#### Measurement Model
We have defined the measurement model as follows:

<INSERT IMAGE OF MEASUREMENT MODEL HERE>

In R our model is described by creating a 13 by 3 matrix and initializing it as matrix object mmMatrix. This matrix is limited to 3 column width, but can have any number of measurement model combinations to accurately describe the proposed measurement model. The Latent column identifies the factor to which the variable from the dataset will be applied, the Measurement column identifies the variable applied to that factor and the Type column identifies whether the factor is (R)eflective (Endogenous) or (F)ormative (Exogenous). The Type of the variables must be consistent throughout for each factor (ie a factor cannot have some reflective and some formative variables), but any number of combinations of Formative and Reflective factors can be defined to fully and accurately describe the proposed model.

``` 
#Create the Matrix of the Measurement Model
mmMatrix <- matrix(c("Perceived Visual Complexity","VX.0","F",
                     "Perceived Visual Complexity","VX.1","F",
                     "Perceived Visual Complexity","VX.2","F",
                     "Perceived Visual Complexity","VX.3","F",
                     "Perceived Visual Complexity","VX.4","F",
                     "Arousal","Aro1","F",
                     "Arousal","Aro2","F",
                     "Arousal","Aro3","F",
                     "Arousal","Aro4","F",
                     "Aproach/Avoidance","AA.0","R",
                     "Aproach/Avoidance","AA.1","R",
                     "Aproach/Avoidance","AA.2","R",
                     "Aproach/Avoidance","AA.3","R"),nrow=13,ncol=3,byrow =TRUE,
                   dimnames = list(1:13,c("latent","measurement","type")))
```

<INSERT TABLE OF STRUCTURAL MATRIX HERE>

With the data imported and both the structural and measurement models defined, the simplePLS function can be called:
``` 
plsModel<-simplePLS(Anime,smMatrix,mmMatrix,9)
```
This new plsModel object is now the model of the PLSPM that we have defined and contains:
1. The original means of each variable (for predictive purposes)  
2. The original SD of each variable (for predictive purposes)  
3. The structural model (in matrix form as described above)  
4. The measurement model (in matrix form as described above)  
5. The latent variables   
6. The measurement variables  
7. Outer loadings for reflective or endogenous constructs  
8. Outer weights for formative or exogenous constructs  
9. Path coefficients between exogenous and endogenous constructs  

### Prediction on the simplePLS model	 

After training the simplePLS object on the training dataset, we need use PLSpredict() using the simplePLS model and the test data set. This returns the testdata, the predicted values for the test data and the residuals for the predictions.
```
predTest <- PLSpredict(plsModel,testData)
```
An investigation of predTest yields the following header information


## In Development
- Modify simplePLS() to include k-fold Cross-validation
- Calculation and inclusion of performance metrics, R-squared, RMSE, Q-squared
- Modify PLSpredict() to include calculation and return of Prediction Intervals  

## Authors
- Galit shmueli
- Soumya Ray
- Juan Manuel Velasquez Estrada
- Suneel Babu Chatla

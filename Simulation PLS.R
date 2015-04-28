
#Load our Algorithm
source("./lib/simplePLS.R")

#Load training Data
trainData<-dat

#Create the Matrix of the Structural Model
smMatrix <- matrix(c("Latent Variable 1", "Latent Variable 3",
                     "Latent Variable 2","Latent Variable 3"),nrow=2,ncol=2,byrow =TRUE,
                   dimnames = list(1:2,c("source","target")))

#Create the Matrix of the Measurement Model
mmMatrix <- matrix(c("Latent Variable 1","y1","R",
                     "Latent Variable 1","y2","R",
                     "Latent Variable 1","y3","R",
                     "Latent Variable 1","y4","R",
                     "Latent Variable 2","y5","R",
                     "Latent Variable 2","y6","R",
                     "Latent Variable 2","y7","R",
                     "Latent Variable 2","y8","R",
                     "Latent Variable 3","y9","R",
                     "Latent Variable 3","y10","R",
                     "Latent Variable 3","y11","R",
                     "Latent Variable 3","y12","R"),nrow=12,ncol=3,byrow =TRUE,
                   dimnames = list(1:12,c("latent","measurement","type")))

#Call PLS-PM Function
plsModel<-simplePLS(trainData,smMatrix,mmMatrix,9)

prevplsModel<-plsModel

plsModel$iterations
plsModel$path_coef
plsModel$outer_loadings
plsModel$outer_weights

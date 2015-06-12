#Creating Simulated Data

#Requiere Simsem Library
require(simsem)

#Seed to the random process
set.seed(123)

#Creat Loadings Matrix according to the model (Cols=LV, Rows=Measurements)

#Set "NA" to the intersection between the Latent Variables and it corresponding meassurement
loading <- matrix(0, 10, 3)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:10, 3] <- NA

#Create the LV (simsem) Parameter
#LV: Factor loading matrix from endogenous factors to Y indicators (need to be a
#    matrix or a list of matrices).
LY <- bind(loading, 0.7)

# Create parameters for latent variablesa

#Create a latent variables correlation matrix
latent.cor <- matrix(NA, 3, 3)
diag(latent.cor) <- 1

#Assign the correlations between the latent variables and create the RPS (simsem) parameter
#RPS: Residual correlation matrix among endogenous factors (need to be a symmetric
#     matrix or a list of symmetric matrices).
RPS <- binds(latent.cor, 0.5)

#Assign the measurement error correlation matrix among the measurements for the RTE (simsem) parameter
#RTE: Measurement error correlation matrix among Y indicators (need to be a symmetric
#     matrix or a list of symmetric matrices).
RTE <- binds(diag(10))

#VY: Total variance of indicators (need to be a vector or a list of vectors). NOTE: Either
#    measurement error variance or indicator variance is specified. Both cannot
#    be simultaneously specified.
VY <- bind(rep(NA,10),3)

# Define the path of the model indicating the distribution of the data to be generated
path <- matrix(0, 3, 3)
path[3, 1:2] <- NA
path.start <- matrix(0, 3, 3)
path.start[3, 1] <- "rnorm(1,0.6,0.05)"
path.start[3, 2] <- "runif(1,0.3,0.5)"

#Create the matrix of regression coefficients BI (simsem) parameter
#BE: Regression coefficient matrix among endogenous factors (need to be a matrix or
#    a list of matrices).
BE <- bind(path, path.start)

#Run model and generate data
semModel <- model(BE=BE,LY = LY, RPS = RPS, RTE = RTE, modelType = "SEM")
semData <- generate(semModel,300)

#Asign names to the variables
colnames(semData)=c("x11","x12","x13",
                    "x21","x22","x23",
                    "x31","x32","x33","x34")

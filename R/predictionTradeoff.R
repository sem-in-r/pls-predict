# Function to caclulate RMSE of PLS-PM Model and RMSE of LM Model and 
# Calculate the difference
setwd("c:/Users/nicho/Dropbox/GITrepos/pls-predict/")

Anime=read.csv("./data/AnimData.csv",header=T)
set.seed(123)

#Identify variables to be tested
uniqueTarget <- unique(smMatrix[,2])
items <- mmMatrix[mmMatrix[, "latent"] == uniqueTarget,2]


## Calculate RMSE of the PLS Model
# Calculate PLS Predictions
PLSModel <- PLSpredict(Anime, Anime, smMatrix, mmMatrix)
PLSModel <- PLSpredict(russett, russett, smMatrix, mmMatrix)

# Extract values
PLSresiduals <- PLSModel$residuals[,items]

# Initialize PLS RMSE holder
PLSRMSE <- matrix(,nrow=1,ncol=length(items),byrow =TRUE,dimnames = list(1,items))

# RMSE calculation for each variable of interest
# Iterate over no of residuals columns
for(i in 1:length(items)){
  #Calculate SMSE
  PLSRMSE[i] <- sqrt(mean(PLSresiduals[,i]^2))
}


# Calculate RMSE of the LM Model

# Initialize LM RMSE holder
LMRMSE <- matrix(,nrow=1,ncol=length(items),byrow =TRUE,dimnames = list(1,items))

# Calculate LM predictions
LMmodel1 <- lm(AA.0 ~ VX.0 + VX.1 + VX.2 + VX.3 + VX.4 + Aro1 + Aro2 + Aro3+ Aro4, Anime)
LMmodel2 <- lm(AA.1 ~ VX.0 + VX.1 + VX.2 + VX.3 + VX.4 + Aro1 + Aro2 + Aro3+ Aro4, Anime)
LMmodel3 <- lm(AA.2 ~ VX.0 + VX.1 + VX.2 + VX.3 + VX.4 + Aro1 + Aro2 + Aro3+ Aro4, Anime)
LMmodel4 <- lm(AA.3 ~ VX.0 + VX.1 + VX.2 + VX.3 + VX.4 + Aro1 + Aro2 + Aro3+ Aro4, Anime)

# Calculate the RMSE
LMRMSE[1] <- sqrt(mean(LMmodel1$residuals^2))
LMRMSE[2] <- sqrt(mean(LMmodel2$residuals^2))
LMRMSE[3] <- sqrt(mean(LMmodel3$residuals^2))
LMRMSE[4] <- sqrt(mean(LMmodel4$residuals^2))
LMRMSE[5] <- sqrt(mean(LMmodel5$residuals^2))
LMRMSE[6] <- sqrt(mean(LMmodel6$residuals^2))

# Calculate LM predictions
LMmodel1 <- lm(death ~ farm + gini + rent + gnpr + labo, russett)
LMmodel2 <- lm(demostab ~ farm + gini + rent + gnpr + labo, russett)
LMmodel3 <- lm(dictator ~ farm + gini + rent + gnpr + labo, russett)
LMmodel4 <- lm(ecks ~ farm + gini + rent + gnpr + labo, russett)
LMmodel5 <- lm(inst ~ farm + gini + rent + gnpr + labo, russett)
LMmodel6 <- lm(demoinst ~ farm + gini + rent + gnpr + labo, russett)

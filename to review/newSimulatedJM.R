
#Clear Environment
rm (list=ls())

#Load library to get random samples of multivariate normal distribution
require(mnormt)

#Seed to the random process
set.seed(123)

#Four Variables Mean 2 SD =2
Sigma <- matrix(c(4,2,2,2,2,4,2,2,2,2,4,2,2,2,2,4),4,4)
mu <- c(2, 2, 2, 2)
x1<-rmnorm(300,mu,Sigma)
w1<-c(1,1,1,1)

#Create an error term of 1/4 the sd of the variables and add all 
#the values to creat latent variable 1
e1<-rnorm(300,mean=0,sd=(1.5*0.25))
fc1<-(x1%*%w1)
fc1<-scale(fc1,center=TRUE,scale=TRUE)
fc1<-fc1+e1

#Four Variables Mean 0, SD =1
Sigma <- matrix(c(1,0.5,0.5,0.5,0.5,1,0.5,0.5,0.5,0.5,1,0.5,0.5,0.5,0.5,1),4,4)
mu <- c(0, 0, 0, 0)
x2<-rmnorm(300,mu,Sigma)
w2<-c(1,1,1,1)
#Create an error term of 1/4 the sd of the variables and add all 
#the values to creat latent variable 1
e2<-rnorm(300,mean=0,sd=(1*0.25))
fc2<-(x2%*%w2)
fc2<-scale(fc2,center=TRUE,scale=TRUE)
fc2<-fc2+e2

#get the third factor score by agregating the previous one plus error from normal distribuiton
# with mean 0 and standard deviation of 0.25
e3<-rnorm(300,mean=0,sd=(0.75))
fc3<-fc1+fc2
fc3<-scale(fc3,center=TRUE,scale=TRUE)
fc3<-fc3+e3


#produce some weight out of the randomness
w3<-rnorm(4,mean=0.5,sd=(2))

#Create the variables
y3<-fc3%*%w3

y3<-y3+5


#We produce a fourth term to simulate fc3 as a mediator.
#We then create a new error term
e4<-rnorm(300,mean=0,sd=(0.35))
fc4<-fc3
fc4<-scale(fc4,center=TRUE,scale=TRUE)
fc4<-fc4+e4

#produce some weight out of the randomness
w4<-rnorm(4,mean=0.5,sd=(2))

#Create the variables
y4<-fc4%*%w4

y4<-y4+3

#Create the data
data<-cbind(x1,x2,y3,y4)
colnames(data)<-c("x11","x12","x13","x14","x21","x22","x23","x24","x31","x32","x33","x34","y1","y2","y3","y4")

write.csv(data,"data_meeting_11_jm.csv")

head(data)

source("./lib/simplePLS.R")
source("./lib/graphUtils.R")

#Prepare the random sampling
set.seed(123)
index=sample.int(dim(data)[1],300,replace=F)
trainData=data[index[1:200],]
holdData=data[index[201:300],]

smMatrix <- matrix(c("Latent Variable 1", "Latent Variable 3",
                     "Latent Variable 2","Latent Variable 3"),nrow=2,ncol=2,byrow =TRUE,
                   dimnames = list(1:2,c("source","target")))

#Create the Matrix of the Measurement Model
mmMatrix <- matrix(c("Latent Variable 1","x11","R",
                     "Latent Variable 1","x12","R",
                     "Latent Variable 1","x13","R",
                     "Latent Variable 1","x14","R",
                     "Latent Variable 2","x21","R",
                     "Latent Variable 2","x22","R",
                     "Latent Variable 2","x23","R",
                     "Latent Variable 2","x24","R",
                     "Latent Variable 3","y1","R",
                     "Latent Variable 3","y2","R",
                     "Latent Variable 3","y3","R",
                     "Latent Variable 3","y4","R"),nrow=12,ncol=3,byrow =TRUE,
                   dimnames = list(1:12,c("latent","measurement","type")))

#Call PLS-PM Function
plsModel<-simplePLS(trainData,smMatrix,mmMatrix)

#Call Prediction Function
predHold <- PLSpredict(plsModel,holdData)

## fitting neural net model
require(nnet)

X=trainData[,c(1:8)]
Y=trainData[,c(9:12)]

# Model with two hidden nodes
net1=nnet(X,Y,size=2,linout =T)
nnPredData=data.frame(predict(net1,holdData))

#Calculate NN Residuals
nnPredResiduals<-holdData[,c("y1","y2","y3","y4")]-nnPredData[,c("y1","y2","y3","y4")]

#Set the panels
par(mfrow=c(2,2))

#Find scales
xmax<-ceiling(max(c(holdData[,"y1"],
                    holdData[,"y2"],
                    holdData[,"y3"],
                    holdData[,"y4"])))

ymax<-ceiling(max(c(predHold$predictedMeasurements[,"y1"],
                    predHold$predictedMeasurements[,"y2"],
                    predHold$predictedMeasurements[,"y3"],
                    predHold$predictedMeasurements[,"y4"],
                    nnPredData[,"y1"],
                    nnPredData[,"y2"],
                    nnPredData[,"y3"],
                    nnPredData[,"y4"])))

xmin<-floor(min(c(holdData[,"y1"],
                  holdData[,"y2"],
                  holdData[,"y3"],
                  holdData[,"y4"])))

ymin<-floor(min(c(predHold$predictedMeasurements[,"y1"],
                  predHold$predictedMeasurements[,"y2"],
                  predHold$predictedMeasurements[,"y3"],
                  predHold$predictedMeasurements[,"y4"],
                  nnPredData[,"y1"],
                  nnPredData[,"y2"],
                  nnPredData[,"y3"],
                  nnPredData[,"y4"])))

#PLS: Actual vs Predicted (y1)
y<-predHold$predictedMeasurements[,"y1"]
x<-holdData[,"y1"]
z<-nnPredData[,"y1"]
title="Actual vs Predicted (y1)"
xlabel=paste("Mean: Act=",
             signif(mean(x),digits=4),
             "PLS=",
             signif(mean(y),digits=4),
             "NN=",
             signif(mean(z),digits=4),
             "\n SD: Act=",
             signif(sd(x),digits=4),
             "PLS=",
             signif(sd(y),digits=4),
             "NN=",
             signif(sd(z),digits=4))
ylabel="Predicted"
graphScatterplot(x,y,z,title,xlabel,ylabel,xmax=xmax,ymax=ymax,xmin=xmin,ymin=ymin)
points(x,z, col= "black")


#PLS: Actual vs Predicted (y2)
y<-predHold$predictedMeasurements[,"y2"]
x<-holdData[,"y2"]
z<-nnPredData[,"y2"]
title="Actual vs Predicted (y2)"
xlabel=paste("Mean: Act=",
             signif(mean(x),digits=4),
             "PLS=",
             signif(mean(y),digits=4),
             "NN=",
             signif(mean(z),digits=4),
             "\n SD: Act=",
             signif(sd(x),digits=4),
             "PLS=",
             signif(sd(y),digits=4),
             "NN=",
             signif(sd(z),digits=4))
ylabel="Predicted"
graphScatterplot(x,y,z,title,xlabel,ylabel,xmax=xmax,ymax=ymax,xmin=xmin,ymin=ymin)
points(x,z, col= "black")

#PLS: Actual vs Predicted (y3)
y<-predHold$predictedMeasurements[,"y3"]
x<-holdData[,"y3"]
z<-nnPredData[,"y3"]
title="Actual vs Predicted (y3)"
xlabel=paste("Mean: Act=",
             signif(mean(x),digits=4),
             "PLS=",
             signif(mean(y),digits=4),
             "NN=",
             signif(mean(z),digits=4),
             "\n SD: Act=",
             signif(sd(x),digits=4),
             "PLS=",
             signif(sd(y),digits=4),
             "NN=",
             signif(sd(z),digits=4))
ylabel="Predicted"
graphScatterplot(x,y,z,title,xlabel,ylabel,xmax=xmax,ymax=ymax,xmin=xmin,ymin=ymin)
points(x,z, col= "black")

#PLS: Actual vs Predicted (y3)
y<-predHold$predictedMeasurements[,"y4"]
x<-holdData[,"y4"]
z<-nnPredData[,"y4"]
title="Actual vs Predicted (y4)"
xlabel=paste("Mean: Act=",
             signif(mean(x),digits=4),
             "PLS=",
             signif(mean(y),digits=4),
             "NN=",
             signif(mean(z),digits=4),
             "\n SD: Act=",
             signif(sd(x),digits=4),
             "PLS=",
             signif(sd(y),digits=4),
             "NN=",
             signif(sd(z),digits=4))
ylabel="Predicted"
graphScatterplot(x,y,z,title,xlabel,ylabel,xmax=xmax,ymax=ymax,xmin=xmin,ymin=ymin)
points(x,z, col= "black")


#Set the panels
par(mfrow=c(2,2))

title<-"PLS vs NN Residuals"

graphCombinedResiduals("y1",nnPredResiduals,predHold$residuals,title,c(-4,4),c(0,1.5),10,"NN","PLS")

graphCombinedResiduals("y2",nnPredResiduals,predHold$residuals,title,c(-4,4),c(0,1.5),10,"NN","PLS")

graphCombinedResiduals("y3",nnPredResiduals,predHold$residuals,title,c(-4,4),c(0,1.5),10,"NN","PLS")

graphCombinedResiduals("y4",nnPredResiduals,predHold$residuals,title,c(-4,4),c(0,1.5),10,"NN","PLS")

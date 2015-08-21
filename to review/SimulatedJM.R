rm (list=ls())

require(mnormt)

# covariance parameters
Sigma <- matrix(c(10,3,3,2), ncol = 2)
Sigma

# generate the sample
y <- rmnorm(n = 1000, varcov = Sigma)
var(y)

# scatterplot of a random bivariate normal sample with mean
# vector zero and covariance matrix 'Sigma'
par(pty = "s")
plot(y, xlab = "", ylab = "")
title("bivariate normal sample", font.main = 1)

head(y)
Sigma=matrix(c(1,0.5,0.5,1),4,2,byrow = T)

x<-matrix(c(4,2,0.6,4.2,2.1,0.59,3.9,2,0.58,4.3,2.1,0.62,4.1,2.2,0.63),5,3,byrow = T)

mean(x)

cov(x)

sd(x[,1])

cov()


Sigma <- matrix(c(4,-3,-3,9),2,2)
y <- rmnorm(n = 1000, varcov = Sigma)

cov(y)

cov(y[,1],y[,1])


sd(y[,2])


Sigma <- matrix(c(4,2,2,4),2,2)

sigma <- matrix(c(1, 0.9, -0.3, 0.9, 1, -0.4, -0.3, -0.4, 1), ncol = 3)

mu <- c(10, 5, -3)

rmnorm(1000,mu,sigma)



Sigma <- matrix(c(4,2,2,4),2,2)
mu <- c(2, 2)
y<-rmnorm(1000,mu,Sigma)
plot(y, xlab = "", ylab = "")


#HERE STARTS THE REAL SIMULATION

set.seed(123)

#Four Variables Mean 2 SD =2
Sigma <- matrix(c(4,2,2,2,2,4,2,2,2,2,4,2,2,2,2,4),4,4)
mu <- c(2, 2, 2, 2)
x1<-rmnorm(300,mu,Sigma)
w1<-c(1,1,1,1)
e1<-rnorm(300,mean=0,sd=(1.5*0.25))
fc1<-(x1%*%w1)
fc1<-scale(fc1,center=TRUE,scale=TRUE)
fc1<-fc1+e1

#Four Variables Mean 0, SD =1
Sigma <- matrix(c(1,0.5,0.5,0.5,0.5,1,0.5,0.5,0.5,0.5,1,0.5,0.5,0.5,0.5,1),4,4)
mu <- c(0, 0, 0, 0)
x2<-rmnorm(300,mu,Sigma)
w2<-c(1,1,1,1)
e2<-rnorm(300,mean=0,sd=(1*0.25))
fc2<-(x2%*%w2)
fc2<-scale(fc2,center=TRUE,scale=TRUE)
fc2<-fc2+e2

#get the third factor score
e3<-rnorm(300,mean=0,sd=(0.25))
fc3<-fc1+fc2
fc3<-scale(fc3,center=TRUE,scale=TRUE)
fc3<-fc3+e3


#Four Variables Mean 0, SD =1
Sigma <- matrix(c(1,0.5,0.5,0.5,0.5,1,0.5,0.5,0.5,0.5,1,0.5,0.5,0.5,0.5,1),4,4)
mu <- c(1, 1, 1, 1)
y<-rmnorm(300,mu,Sigma)

w3<-cov(fc3,y)

y3<-fc3%*%w3
colnames(y3)

data<-cbind(x1,x2,y3)
colnames(data)<-c("x11","x12","x13","x14","x21","x22","x23","x24","y1","y2","y3","y4")

head(data)

write.csv(data,"data.csv")



source("./lib/simplePLS.R")
source("./lib/graphUtils.R")

#Prepare the random sampling
set.seed(123)
index=sample.int(dim(data)[1],300,replace=F)
trainData=data[index[1:200],]
holdData=data[index[201:300],]

#Create the Matrix of the Structural Model
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
graphScatterplot(x,y,z,title,xlabel,ylabel,xmax=0.25,ymax=0.25,xmin=-0.25,ymin=-0.25)
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

graphCombinedResiduals("y1",nnPredResiduals,predHold$residuals,title,c(-2,2),c(0,10),10,"NN","PLS")

graphCombinedResiduals("y2",nnPredResiduals,predHold$residuals,title,c(-2,2),c(0,10),10,"NN","PLS")

graphCombinedResiduals("y3",nnPredResiduals,predHold$residuals,title,c(-2,2),c(0,10),10,"NN","PLS")

graphCombinedResiduals("y4",nnPredResiduals,predHold$residuals,title,c(-2,2),c(0,10),10,"NN","PLS")

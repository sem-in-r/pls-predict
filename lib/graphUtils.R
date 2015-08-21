#graphUtils
#Author: Juan Manuel Velasquez Estrada
#Creation: April 2015
#Description: This library contains funcitons designed to compare datasets from different 
#sources but same amount of observations, its original purpose is to help compare predictions
#between PLS-PM and other prediction methods.

#Function that graphs histogram for residual, receives the name of a predicted measurement
graphResiduals <- function(measurement,residuals,title,xrange){
  statData <- paste("Mean:",signif(mean(residuals[,measurement]),digits=4), "Standard Deviation:",signif(sd(residuals[,measurement]),digits=4))
  hist(residuals[,measurement],xlim=xrange, prob=TRUE, xlab= statData, main = paste(title,measurement))
  lines(density(residuals[,measurement],adjust=2),col="red",lwd=1)
}  

#Graph to produce a scatterplot comparing two datasets
graphScatterplot <- function(x,y,z,title,xlabel,ylabel,adjust=0,xmax=0,ymax=0,xmin=0,ymin=0){
  plot(x-adjust,y,
       main =title,
       xlab=xlabel,
       ylab=ylabel,
       col="blue",
       xlim = c(floor (min(c(x,xmin))),ceiling (max(c(x,xmax)))),
       ylim = c(floor (min(c(y,ymin))),ceiling (max(c(y,ymax)))))
       #abline(lm(y~x), col="red")
  q<-seq(from = 1,to=100)
  w<-seq(from = 1,to=100)
  abline(lm(q~w), col="darkgreen")
  points(mean(z),mean(z), col= "red",pch=5)
  points(mean(x),mean(x), col= "red",pch=2)
  points(mean(y), mean(y), col= "red",pch=3)
}  


#Function that graphs histogram for residual, receives the name of a predicted measurement
graphCombinedResiduals <- function(measurement,residualsx,residualsy,title,xrange,yrange,breaks,xlab,ylab,xcolor=rgb(1,0,0,0.5),ycolor=rgb(0,0,1,0.5)){
  statData <- paste(xlab,"(Dashed) Avg Error:",signif(mean(residualsx[,measurement]),digits=4), "RMSE:",signif(sd(residualsx[,measurement]),digits=4),"\n",
                    ylab,"(Solid) Avg Error:",signif(mean(residualsy[,measurement]),digits=4), "RMSE:",signif(sd(residualsy[,measurement]),digits=4))
  #hist(residualsx[,measurement], prob=TRUE,xlim=xrange, breaks =10, ylim=yrange,xlab= statData, main = paste(title,measurement),col=xcolor)
  #hist(residualsy[,measurement], prob=TRUE, col=ycolor, add=T, breaks =10)
  plot(density(residualsx[,measurement],adjust=1),col=xcolor,lwd=2,lty=2,xlab= statData, main = paste(title))  
  lines(density(residualsy[,measurement],adjust=1),col=ycolor,lwd=1,lty=1)  
}  

#Function that graphs histogram for residual, receives the name of a predicted measurement
newgraphResiduals <- function(measurement,residuals,title,xrange,yrange){
  statData <- paste("Mean:",signif(mean(residuals[,measurement]),digits=4), "Standard Deviation:",signif(sd(residuals[,measurement]),digits=4))
  hist(residuals[,measurement], prob=TRUE,xlim=xrange, ylim=yrange,xlab= statData, main = paste(title,measurement),col=rgb(1,0,0,0.5))
  lines(density(residuals[,measurement],adjust=1),col="red",lwd=1)  
}  


#Graph to produce a scatterplot comparing two datasets
graphScatterplot42 <- function(x,y,title,xlabel,ylabel,adjust=0,xmax=0,ymax=0,xmin=0,ymin=0){
  plot(x-adjust,y,
       main =title,
       xlab=xlabel,
       ylab=ylabel,
       col=rgb(0,0,0,0.5),
       xlim = c(floor (min(c(x,xmin))),ceiling (max(c(x,xmax)))),
       ylim = c(floor (min(c(y,ymin))),ceiling (max(c(y,ymax)))))
  #abline(lm(y~x), col="red")
  q<-seq(from = 1,to=100)
  w<-seq(from = 1,to=100)
  abline(lm(q~w), col="darkgreen")
  points(mean(x),mean(x), col= "red",pch=2)
  points(mean(y), mean(y), col= "red",pch=3)
}  

#Graph to produce a scatterplot comparing two datasets
dualGraphScatterplot <- function(x,y,z,title,xlabel,ylabel,adjust=0,xmax=0,ymax=0,xmin=0,ymin=0){
  plot(x-adjust,y,
       main =title,
       xlab=xlabel,
       ylab=ylabel,
       col=rgb(0.25,0.25,0.25,0.5),
       xlim = c(floor (min(c(x,xmin))),ceiling (max(c(x,xmax)))),
       ylim = c(floor (min(c(y,ymin))),ceiling (max(c(y,ymax)))))
  par(new=T)
  plot(x+adjust,z,
       main =title,
       xlab=xlabel,
       ylab=ylabel,
       col=rgb(0,0,0,0.5),
       xlim = c(floor (min(c(x,xmin))),ceiling (max(c(x,xmax)))),
       ylim = c(floor (min(c(z,ymin))),ceiling (max(c(z,ymax)))),
       pch=3)
  q<-seq(from = 1,to=100)
  w<-seq(from = 1,to=100)
  abline(lm(q~w), col="darkgreen")
  #points(mean(z),mean(z), col= "red",pch=5)
  #points(mean(x),mean(x), col= "red",pch=2)
  #points(mean(y), mean(y), col= "red",pch=3)
}  

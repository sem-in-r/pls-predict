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
graphScatterplot <- function(x,y,z,title,xlabel,ylabel){
  plot(x,y,
       main =title,
       xlab=xlabel,
       ylab=ylabel,
       col="blue",
       xlim = c(0,round (max(c(x,y)))),
       ylim = c(0,round (max(c(x,y)))))
       #abline(lm(y~x), col="red")
  q<-seq(from = 1,to=100)
  w<-seq(from = 1,to=100)
  abline(lm(q~w), col="darkgreen")
  points(mean(z),mean(z), col= "black",pch=5)
  points(mean(x),mean(x), col= "red",pch=2)
  points(mean(y), mean(y), col= "darkgreen",pch=3)
}  


#Function that graphs histogram for residual, receives the name of a predicted measurement
graphCombinedResiduals <- function(measurement,residualsx,residualsy,title,xrange,yrange,breaks,xlab,ylab){
  statData <- paste("Red M(",xlab,"):",signif(mean(residualsx[,measurement]),digits=4), "SD(LM):",signif(sd(residualsx[,measurement]),digits=4),"\n",
                    "Blue M(",ylab,"):",signif(mean(residualsy[,measurement]),digits=4), "SD(PLS):",signif(sd(residualsy[,measurement]),digits=4))
  hist(residualsx[,measurement], prob=TRUE,xlim=xrange, ylim=yrange,xlab= statData, main = paste(title,measurement),col=rgb(1,0,0,0.5))
  hist(residualsy[,measurement], prob=TRUE, col=rgb(0,0,1,0.5), add=T)
  lines(density(residualsx[,measurement],adjust=1),col="red",lwd=1)  
  lines(density(residualsy[,measurement],adjust=1),col="blue",lwd=1)  
}  

#Function that graphs histogram for residual, receives the name of a predicted measurement
newgraphResiduals <- function(measurement,residuals,title,xrange,yrange){
  statData <- paste("Mean:",signif(mean(residuals[,measurement]),digits=4), "Standard Deviation:",signif(sd(residuals[,measurement]),digits=4))
  hist(residuals[,measurement], prob=TRUE,xlim=xrange, ylim=yrange,xlab= statData, main = paste(title,measurement),col=rgb(1,0,0,0.5))
  lines(density(residuals[,measurement],adjust=1),col="red",lwd=1)  
}  

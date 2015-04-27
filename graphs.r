plsPredictTest <- NULL
plsResidualsTest <- NULL

lmPredictTest <- NULL
lmResidualsTest <- NULL

# PLS model: Actual vs Predicted Scatterplot

#Set the panels
par(mfrow=c(2,2))

#PLS: Actual vs Predicted (AA.0)
y<-plsPredictTest[,"AA.0"]
x<-crossTestData[,"AA.0"]
z<-lmPredictTest[,"AA.0"]
title="Actual vs Predicted (AA.0)"
xlabel=paste("Mean: Act=",
             signif(mean(x),digits=4),
             "PLS=",
             signif(mean(y),digits=4),
             "LM=",
             signif(mean(z),digits=4),
             "\n SD: Act=",
             signif(sd(x),digits=4),
             "PLS=",
             signif(sd(y),digits=4),
             "LM=",
             signif(sd(z),digits=4))
ylabel="Predicted"
graphScatterplot(x,y,z,title,xlabel,ylabel)
points(x+0.1,z, col= "black")

#PLS: Actual vs Predicted (AA.1)
y<-plsPredictTest[,"AA.1"]
x<-crossTestData[,"AA.1"]
z<-lmPredictTest[,"AA.1"]
title="Actual vs Predicted (AA.1)"
xlabel=paste("Mean: Act=",
             signif(mean(x),digits=4),
             "PLS=",
             signif(mean(y),digits=4),
             "LM=",
             signif(mean(z),digits=4),
             "\n SD: Act=",
             signif(sd(x),digits=4),
             "PLS=",
             signif(sd(y),digits=4),
             "LM=",
             signif(sd(z),digits=4))
ylabel="Predicted"
graphScatterplot(x,y,z,title,xlabel,ylabel)
points(x+0.1,z, col= "black")

#PLS: Actual vs Predicted (AA.2)
y<-plsPredictTest[,"AA.2"]
x<-crossTestData[,"AA.2"]
z<-lmPredictTest[,"AA.2"]
title="Actual vs Predicted (AA.2)"
xlabel=paste("Mean: Act=",
             signif(mean(x),digits=4),
             "PLS=",
             signif(mean(y),digits=4),
             "LM=",
             signif(mean(z),digits=4),
             "\n SD: Act=",
             signif(sd(x),digits=4),
             "PLS=",
             signif(sd(y),digits=4),
             "LM=",
             signif(sd(z),digits=4))
ylabel="Predicted"
graphScatterplot(x,y,z,title,xlabel,ylabel)
points(x+0.1,z, col= "black")

#PLS: Actual vs Predicted (AA.3)
y<-plsPredictTest[,"AA.3"]
x<-crossTestData[,"AA.3"]
z<-lmPredictTest[,"AA.3"]
title="Actual vs Predicted (AA.3)"
xlabel=paste("Mean: Act=",
             signif(mean(x),digits=4),
             "PLS=",
             signif(mean(y),digits=4),
             "LM=",
             signif(mean(z),digits=4),
             "\n SD: Act=",
             signif(sd(x),digits=4),
             "PLS=",
             signif(sd(y),digits=4),
             "LM=",
             signif(sd(z),digits=4))
ylabel="Predicted"
graphScatterplot(x,y,z,title,xlabel,ylabel)
points(x+0.1,z, col= "black")


# PLS vs Linear Regression: Joint Histogram

#Set the panels
par(mfrow=c(2,2))

title<-"PLS vs LM Residuals"
graphCombinedResiduals("AA.0",lmResidualsTest,plsResidualsTest,title,c(-6,6),c(0,0.4),10,"LM","PLS")

graphCombinedResiduals("AA.1",lmResidualsTest,plsResidualsTest,title,c(-6,6),c(0,0.4),10,"LM","PLS")

graphCombinedResiduals("AA.2",lmResidualsTest,plsResidualsTest,title,c(-6,6),c(0,0.4),10,"LM","PLS")

graphCombinedResiduals("AA.3",lmResidualsTest,plsResidualsTest,title,c(-6,6),c(0,0.4),10,"LM","PLS")

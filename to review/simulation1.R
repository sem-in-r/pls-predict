##############################################################
############# Data Simulation#################################
##############################################################

## Only reflective for now


rm(list=ls())

require(simsem)

loading <- matrix(0, 16, 4)
loading[1:4, 1] <- NA
loading[5:8, 2] <- NA
loading[9:12, 3] <- NA
loading[13:16, 4] <- NA

LY <- bind(loading, 0.7)

## Latent variables

latent.cor <- matrix(NA, 4, 4)
diag(latent.cor) <- 1
RPS <- binds(latent.cor, 0.5)
RTE <- binds(diag(16))
VY <- bind(rep(NA,16),4)

# Regression 
path <- matrix(0, 4, 4)
path[3, 1:2] <- NA
path[4, 3] <- NA
path.start <- matrix(0, 4, 4)
path.start[3, 1] <- "rnorm(1,0.6,0.05)"
path.start[3, 2] <- "runif(1,0.3,0.5)"
path.start[4, 3] <- "rnorm(1,-0.3,0.5)"
BE <- bind(path, path.start)



CFA.Model <- model(BE=BE,LY = LY, RPS = RPS, RTE = RTE, modelType = "SEM")
dat <- generate(CFA.Model,300)


colnames(dat)=c("x11","x12","x13","x14",
                "x21","x22","x23","x24",
                "x31","x32","x33","x34",
                "y1","y2","y3","y4")
head(dat)

write.csv(dat,"data_meeting_11_simsem.csv")

## data partition
set.seed(1)
train.index=sample.int(500,400)
train=dat[train.index,]
hold=dat[-train.index,]

## fitting neural net model
require(nnet)

X=train[,c(1:8)]
Y=train[,c(9:12)]

# Model with two hidden nodes
net1=nnet(X,Y,size=2,linout =T)
summary(net1)
hold.pred=data.frame(predict(net1,hold))
head(hold.pred)
plot(hold$y1,hold.pred$y1)
abline(lm(hold.pred$y1~hold$y1))
cor(hold$y1,hold.pred$y1)














# require(mnormt)
# 
# # 3 latent variable from multinormal distribution
# n=100
# mu=rep(0,2)
# Sigma=matrix(c(1,0.5,0.5,1),2,2,byrow = T)
# set.seed(123)
# eta.exo=rmnorm(n,mu,Sigma)
# set.seed(1)
# e.str=rnorm(n,0,sqrt(0.5))  ## to make sure the eta.endo has variance 1
# eta.endo=0.5*eta.exo[,1]-0.5*eta.exo[,2]+e.str  # (same model as what we
# # were using)
# cor(cbind(eta.exo,eta.endo))  # cross check
# 
# # 4 reflective items for each latent
# set.seed(1)
# e=rnorm(n,0,0.3)
# # items for latent 1
# i11=1+0.7*eta.exo[,1]+e
# i12=1+0.5*eta.exo[,1]+e
# i13=-1+0.6*eta.exo[,1]+e
# i14=-1+0.7*eta.exo[,1]+e
# 
# # items for latent 2
# i21=1+0.7*eta.exo[,2]+e
# i22=1+0.5*eta.exo[,2]+e
# i23=-1+0.6*eta.exo[,2]+e
# i24=-1+0.7*eta.exo[,2]+e
# 
# # items for latent 3 (eta.endo: dependent variable)
# i31=1+0.7*eta.endo+e
# i32=1+0.5*eta.endo+e
# i33=-1+0.6*eta.endo+e
# i34=-1+0.7*eta.endo+e
# 
# data=data.frame(cbind(i11,i12,i13,i14,i21,i22,i23,i24,i31,i32,i33,i34))


<<<<<<< HEAD
##############################################################
############# Data Simulation#################################
##############################################################

## Only reflective for now


rm(list=ls())
require(mnormt)

# 3 latent variable from multinormal distribution
n=183
mu=rep(0,2)
Sigma=matrix(c(1,0.5,0.5,1),2,2,byrow = T)
set.seed(123)
eta.exo=rmnorm(n,mu,Sigma)
set.seed(1)
e.str=rnorm(n,0,sqrt(0.5))  ## to make sure the eta.endo has variance 1
eta.endo=0.5*eta.exo[,1]-0.5*eta.exo[,2]+e.str  # (same model as what we
# were using)
cor(cbind(eta.exo,eta.endo))  # cross check

# 4 reflective items for each latent
set.seed(1)
e=rnorm(n,0,0.3)
# items for latent 1
i11=1+0.7*eta.exo[,1]+e
i12=1+0.5*eta.exo[,1]+e
i13=-1+0.6*eta.exo[,1]+e
i14=-1+0.7*eta.exo[,1]+e

# items for latent 2
i21=1+0.7*eta.exo[,2]+e
i22=1+0.5*eta.exo[,2]+e
i23=-1+0.6*eta.exo[,2]+e
i24=-1+0.7*eta.exo[,2]+e

# items for latent 3 (eta.endo: dependent variable)
i31=1+0.7*eta.endo+e
i32=1+0.5*eta.endo+e
i33=-1+0.6*eta.endo+e
i34=-1+0.7*eta.endo+e

data=data.frame(cbind(i11,i12,i13,i14,i21,i22,i23,i24,i31,i32,i33,i34))

write.csv(data, "simulated.csv")
=======
##############################################################
############# Data Simulation#################################
##############################################################

## Only reflective for now


rm(list=ls())
require(mnormt)

# 3 latent variable from multinormal distribution
n=183
mu=rep(0,2)
Sigma=matrix(c(1,0.5,0.5,1),2,2,byrow = T)
set.seed(123)
eta.exo=rmnorm(n,mu,Sigma)
set.seed(1)
e.str=rnorm(n,0,sqrt(0.5))  ## to make sure the eta.endo has variance 1
eta.endo=0.3*eta.exo[,1]+0.7*eta.exo[,2]+e.str  # (same model as what we
# were using)
cor(cbind(eta.exo,eta.endo))  # cross check

# 4 reflective items for each latent
set.seed(1)
e=rnorm(n,0,0.3)
# items for latent 1
i11=1+0.7*eta.exo[,1]+e
i12=1+0.5*eta.exo[,1]+e
i13=-1+0.6*eta.exo[,1]+e
i14=-1+0.7*eta.exo[,1]+e

# items for latent 2
i21=1+0.7*eta.exo[,2]+e
i22=1+0.5*eta.exo[,2]+e
i23=-1+0.6*eta.exo[,2]+e
i24=-1+0.7*eta.exo[,2]+e

# items for latent 3 (eta.endo: dependent variable)
i31=1+0.7*eta.endo+e
i32=1+0.5*eta.endo+e
i33=-1+0.6*eta.endo+e
i34=-1+0.7*eta.endo+e

cor(eta.exo[,1],eta.exo[,2])
sd(eta.exo[,2])

data=data.frame(cbind(i11,i12,i13,i14,i21,i22,i23,i24,i31,i32,i33,i34))

write.csv(data[1:100,], "newSimulation.csv")
>>>>>>> cdefbf73a7167d457dc15b08531af68ee4deae35

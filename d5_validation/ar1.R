path <- 'J:/git/SAM_TMB/d5_validation'

try(setwd(path),silent=TRUE)

load("cpue.RData")

library(TMB)
compile("ar1.cpp")
dyn.load(dynlib("ar1"))

data = list(y = y)
par = list(logSigma = -2, phiTrans = 1, gamma = rep(0,length(y)))

obj <- MakeADFun(data,par,random = "gamma",DLL = "ar1")
fit <- nlminb(obj$par,obj$fn,obj$gr)

sdr <- sdreport(obj)
estX <- summary(sdr,'random')
C <- solve(obj$env$spHess(obj$env$last.par.best,random=TRUE))
gamma.star <- MASS::mvrnorm(1,estX[,1],C)

rep <- obj$report()

# gamma[i] <- phi*gamma[i-1]+normal dist with sd=sd
# (gamma[i] <- phi*gamma[i-1])/sd, this is N(0,1)
# This is what the next line is doing.
res <- (gamma.star[-1]-rep$phi*gamma.star[-length(gamma.star)])/rep$sd

# the process is supposed to look like the process we assumed, here a normal distribution.
qqnorm(res)
abline(0,1)
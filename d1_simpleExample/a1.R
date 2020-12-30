library (TMB)

setwd("J:/git/SAM_TMB/simpleExample/")

compile('a1.cpp')
dyn.load(dynlib('a1'))

dat <- list()
param <- list(x=20)

obj <- MakeADFun(dat,param,DLL='a1')

fit <- nlminb(obj$par,obj$fn,obj$gr)

fit$par

fit$objective

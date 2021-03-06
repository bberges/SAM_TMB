library(TMB)

setwd("J:/git/SAM_TMB/intro/")

compile("nbin.cpp")
dyn.load(dynlib("nbin"))

dat <- list()
dat$Y <- c(13, 5, 28, 28, 15, 4, 13, 4,
           10, 17, 11, 13, 12, 17, 3)

par <- list()
par$logsize <- 0
par$p <- 0.5

obj <- MakeADFun(dat, par, DLL="nbin")
opt <- nlminb(obj$par, obj$fn, obj$gr)
summary(sdreport(obj))

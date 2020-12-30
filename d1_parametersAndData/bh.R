
setwd("J:/git/SAM_TMB/parametersAndData/")

dat<-read.table("bh.dat", header=TRUE)

SSB_pred <- seq(from=min(dat$SSB),to=max(dat$SSB),by=1000)

library(TMB)
compile("bh.cpp")
dyn.load(dynlib("bh"))

data <- list(SSB=dat$SSB,logR=dat$logR,SSB_pred=SSB_pred)
parameters <- list(
  logA=0,
  logB=0,
  logSigma=0
)

obj <- MakeADFun(data,parameters,DLL="bh")
opt <- nlminb(obj$par,obj$fn,obj$gr)
rep <- sdreport(obj)

plot(dat$SSB,dat$logR, main = "Beverton-Holt", cex.main = 2,cex.lab = 1.5,
     xlab = "SSB",ylab = "logR")
lines(SSB_pred, rep$value, lwd=3)
lines(SSB_pred, rep$value+2*rep$sd, lty="dotted")
lines(SSB_pred, rep$value-2*rep$sd, lty="dotted")

#Make sure you understand these two lines:
obj$fn(opt$par)
obj$gr(opt$par)

rl <- as.list(sdreport(obj),'Est',report=TRUE)

rlSd <- as.list(sdreport(obj), "Std", report = TRUE)


setwd("J:/git/SAM_TMB/simpleAssessment/")

load("fsa.RData") # gets "dat"

library(TMB)
compile("fsa.cpp")
dyn.load(dynlib("fsa"))

parameters <- list(
  logN1Y=rep(0,nrow(dat$M)),
  logN1A=rep(0,ncol(dat$M)-1),
  logFY=rep(0,ncol(dat$M)),
  logFA=rep(0,nrow(dat$M)),
  logVarLogCatch=0,
  logVarLogCatch1Y=0,
  logQ1=rep(0,length(unique(dat$age[dat$fleet==2]))),
  logQ2=rep(0,length(unique(dat$age[dat$fleet==2]))),
  logVarLogSurvey=0
)
obj <- MakeADFun(dat,parameters,DLL="fsa", map=list(logFA=factor(c(1:4,NA,NA,NA))), silent=TRUE)

opt <- nlminb(obj$par, obj$fn, obj$gr, control=list(iter.max=1000,eval.max=1000))
rep <- sdreport(obj)
ssb <- rep$value[names(rep$value)=="ssb"]
ssb.sd <- rep$sd[names(rep$value)=="ssb"]

plot(ssb, type="l", lwd=5, col="red", ylim=c(0,550000))
lines(ssb-2*ssb.sd, type="l", lwd=1, col="red")
lines(ssb+2*ssb.sd, type="l", lwd=1, col="red")

rl <- as.list(sdreport(obj),'Est',report=TRUE)
rlSd <- as.list(sdreport(obj), "Std", report = TRUE)





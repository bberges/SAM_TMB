rm(list=ls())

setwd("J:/git/SAM_TMB/d4_obsCatch")

load("Cobs.RData")
library(TMB)

compile("Cobs.cpp")
dyn.load(dynlib("Cobs"))

myAIC <- array(dim=c(2,1))

par <- list()
par$logsd <- rep(0, length(unique(Cobs$aux[,3])))

map = list()
map$logsd = as.factor(rep(1,length(par$logsd)))
obj <- MakeADFun(Cobs, par, DLL="Cobs",map = map)
fit <- nlminb(obj$par, obj$fn, obj$gr)
sdr<-sdreport(obj)

myAIC[1] <- 2*fit$obj + 2*length(fit$par)

matplot(rownames(Cobs$N), xtabs(log(Cobs$Cobs)~Cobs$aux[,1]+Cobs$aux[,3]), ylab="Log C", xlab="Year")
matplot(rownames(Cobs$N), xtabs(obj$report()$logPred~Cobs$aux[,1]+Cobs$aux[,3]), type="l", add=TRUE)


map <- list()
map$logsd <- as.factor(1:length(par$logsd))
obj2 <- MakeADFun(Cobs,par,DLL="Cobs",map = map)
fit2 <- nlminb(obj2$par, obj2$fn, obj2$gr)

myAIC[2] <- 2*fit2$obj + 2*length(fit2$par)
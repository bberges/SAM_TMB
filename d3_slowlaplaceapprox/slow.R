# first in clean R
data <- list(y=scan("rw.dat"))
parameters <- list(
  logSdRw=0,
  logSdObs=0,
  lam0=0,
  lam=rep(0,length(data$y))
  )

joint <- function(parameters, data){
  nlam <- length(parameters$lam)
  ans <- -dnorm(parameters$lam[1],parameters$lam0,exp(parameters$logSdRw),TRUE)
  ans <- ans + -sum(dnorm(parameters$lam[-1],parameters$lam[-nlam],exp(parameters$logSdRw),TRUE))
  ans <- ans + -sum(dnorm(data$y,parameters$lam,exp(parameters$logSdObs),TRUE))
  ans
}

LA <- function(f, parameters, data, random){
  ff <- function(u){parameters[[random]] <- u; f(parameters,data)}
  est <- nlminb(parameters[[random]],ff)
  lval <- est$obj
  u <<- est$par
  H <<- optimHess(u,ff)
  lval + 0.5 * log(det(H)) - length(u)/2 * log(2 * pi)
}

l <- function(th, random="lam"){parameters[!names(parameters)%in%random]<-th;LA(joint,parameters,data,random)}

init<-unlist(parameters[!names(parameters)%in%"lam"])
opt<-optim(init,l)

 # now in TMB

library(TMB)
compile("rw.cpp")
dyn.load(dynlib("rw"))
obj <- MakeADFun(data,parameters,random="lam",DLL="rw")
obj$fn()
obj$gr()
opt2<-nlminb(obj$par,obj$fn,obj$gr)

# not needed but convenient.  
pl <- obj$env$parList()
rep<-sdreport(obj, getJointPrecision=TRUE)
allsd<-sqrt(diag(solve(rep$jointPrecision)))
plsd <- obj$env$parList(par=allsd)
save(pl,plsd,file="rw.RData")

plot(u)
lines(pl$lam)

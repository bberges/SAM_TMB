library (TMB)

setwd("J:/git/SAM_TMB/preparation/")

dat <-read.table ("linreg.dat", header = TRUE )


compile ("linreg.cpp")
dyn.load(dynlib("linreg"))

data <- list (x= dat$x,y=dat$y)
parameters <- list (alpha =0,
                    beta =0,
                    logSigma =0)

obj <- MakeADFun (data , parameters ,DLL ="linreg")
obj$fn ()
obj$gr ()
system.time (opt <- nlminb ( obj $par , obj$fn ,obj$gr))
rep <- sdreport (obj)

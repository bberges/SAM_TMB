library(TMB)
compile("insect.cpp")
dyn.load(dynlib("insect"))

#For data we use the built-in InsectSprays
par <- list()
par$logAlpha=rep(0,nlevels(InsectSprays$spray))
obj <- MakeADFun(InsectSprays, par, DLL="insect")
opt <- nlminb(obj$par, obj$fn, obj$gr)



map1 <- list(logAlpha=factor(c(1,1,2,3,4,1)))
obj1 <- MakeADFun(InsectSprays, par, map=map1,DLL="insect")
opt1 <- nlminb(obj1$par, obj1$fn, obj1$gr)

#test the hypotesis
1-pchisq(2*(opt1$obj-opt$obj),2)

par2 <- list()
par2$logAlpha=c(log(15),log(15),0,0,0,log(15))
map2 <- list(logAlpha=factor(c(NA,NA,1,2,3,NA)))
obj2 <- MakeADFun(InsectSprays, par2, map=map2,DLL="insect")
opt2 <- nlminb(obj2$par, obj2$fn, obj2$gr)
#...
#obj2 <- ...
#opt2 <- ...

#test the hypotesis
1-pchisq(2*(opt2$obj-opt1$obj),1)
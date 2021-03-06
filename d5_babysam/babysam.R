path <- 'J:/git/SAM_TMB'
try(setwd(path),silent=TRUE)

library(TMB)
compile("babysam.cpp")
dyn.load(dynlib("babysam"))

load("dat.RData")

dat$srmode <- 0
dat$fcormode <- 2

dat$keyF <- c(0,1,2,3,4,5,6,7,7)

dat$keyQ <- rbind(c(NA,NA,NA,NA,NA,NA,NA,NA,NA),
                  c(NA, 0, 1, 2, 3, 4, 5, 5,NA),
                  c( 6, 7, 8, 9,10,10,NA,NA,NA))

dat$keySd <- rbind(c( 0, 0, 0, 0, 0, 0, 0, 0, 0),
                   c(NA, 1, 1, 1, 1, 1, 1, 1,NA),
                   c( 2, 2, 2, 2, 2, 2,NA,NA,NA))

dat$covType <- c(0,1,1) # 0=ID, 1=IGAR, 2=US ...

                    #  0-1 1-2 2-3 3-4 4-5 5-6 6-7 7-8 
dat$keyIGAR <- rbind(c( -1, -1, -1, -1, -1, -1, -1, -1),
                     c( NA, 0,  1,  2,  3,  4,  5, NA),
                     c( 6,  7,  8,  9,  9, NA, NA, NA))

dat$noParUS <- sapply(1:length(dat$fleetTypes),
                      function(f){
                        A<-sum(!is.na(dat$keySd[f,]))
                        ifelse(dat$covType[f]==2, (A*A-A)/2, 0)
                      })

par <- list()
par$logsdR <-0
par$logsdS <- 0
par$logsdF <- numeric(max(dat$keyF)+1)
par$rickerpar <- if(dat$srmode==1){c(1,1)}else{numeric(0)}
par$transRhoF <- if(dat$fcormode==0){numeric(0)}else{0.1}
par$bhpar <- if(dat$srmode==2){c(1,1)}else{numeric(0)}
par$logQ <- numeric(max(dat$keyQ, na.rm=TRUE)+1)
par$logsd <- numeric(max(dat$keySd, na.rm=TRUE)+1)
par$logIGARdist <- numeric(max(dat$keyIGAR, na.rm=TRUE)+1)
par$parUS <- numeric(sum(dat$noParUS))
par$logN <- matrix(0, nrow=length(dat$year), ncol=length(dat$age))
par$logF <- matrix(0, nrow=length(dat$year), ncol=max(dat$keyF)+1)
par$missing <- numeric(sum(is.na(dat$obs)))

obj <- MakeADFun(dat, par, random=c("logN", "logF", "missing"), DLL="babysam", map=list(logsdF=as.factor(rep(0,length(par$logsdF)))))



fit <- nlminb(obj$par, obj$fn, obj$gr, control=list(eval.max=1000, iter.max=1000))

sdr<-sdreport(obj)

library(TMB)
library(stockassessment)

setwd("J:/git/SAM_TMB/d3_SAM")

cn <- read.ices("testdata/cn.dat")
cw <- read.ices("testdata/cw.dat")
dw <- read.ices("testdata/dw.dat")
lf <- read.ices("testdata/lf.dat")
lw <- read.ices("testdata/lw.dat")
mo <- read.ices("testdata/mo.dat")
nm <- read.ices("testdata/nm.dat")
pf <- read.ices("testdata/pf.dat")
pm <- read.ices("testdata/pm.dat")
sw <- read.ices("testdata/sw.dat")
surveys <- read.ices("testdata/survey.dat")

dat <-setup.sam.data (  surveys = surveys,
                        residual.fleet =cn,
                        prop.mature =mo,
                        stock.mean.weight =sw,
                        catch.mean.weight =cw,
                        dis.mean.weight =dw,
                        land.mean.weight =lw,
                        prop.f=pf,
                        prop.m=pm,
                        natural.mortality =nm,
                        land.frac =lf)

conf <- defcon(dat)
conf$keyVarF[1,1:7] <- 0:6
conf$keyVarF[1,8:9] <- 6

conf$corFlag <- 2
par <- defpar (dat, conf)
par$itrans_rho <- 8 # value close to 1
map <- list()
map$itrans_rho <- as.factor(NA)
fit <- sam.fit(dat, conf, par,map=map)

modeltable(c(fit))

fselectivityplot(fit)

matplot(faytable(fit))

saveConf(conf, file ="model.cfg")
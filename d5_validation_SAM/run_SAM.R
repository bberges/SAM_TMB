library(TMB)
library(stockassessment)

setwd("J:/git/SAM_TMB/d5_validation_SAM")

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

conf <- defcon (dat)
par <- defpar (dat, conf)
fit <- sam.fit(dat, conf, par)

modeltable(c(fit))

retro.fit <- retro(fit = fit,year = c(2017,2016,2015,2014))

res <- residuals(fit)

plot(res)

fit.jit <- jit(fit, nojit=100)
fit.jit

sim <- simstudy(fit,nsim=10) # simulation of assessment

retro(fit,year = c(2017,2016))

fit2 <- sam.fit(dat, conf, par,sim.condRE = FALSE)

cc <- checkConsistency(fit2$obj , n =200)

saveConf(conf, file ="model.cfg")
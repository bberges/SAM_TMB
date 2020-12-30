library(stockassessment)

setwd("J:/git/SAM_TMB/intro/")
data(nscodData)
data(nscodConf)
data(nscodParameters)

fit <- sam.fit(nscodData,nscodConf,nscodParameters)

fc  <- forecast(fit,fscale=c(1,1,1,1))

ssbplot(fc)
fbarplot(fc)
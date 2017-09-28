
sa <-read.csv('http://math.mercyhurst.edu/~sousley/STAT_139/data/sahdd.csv', as.is = T);
head(sa)
attach(sa)

library(popbio)


logi.hist.plot(sa$tobacco,sa$chd,boxp=FALSE,type="hist",col="gray")


logi.hist.plot(sa$ldl,sa$chd,boxp=FALSE,type="hist",col="gray")

logi.hist.plot(sa$adiposity,sa$chd,boxp=FALSE,type="hist",col="gray")

logi.hist.plot(sa$famhist,sa$chd,boxp=FALSE,type="hist",col="gray")

logi.hist.plot(sa$typea,sa$chd,boxp=FALSE,type="hist",col="gray")

logi.hist.plot(sa$obesity,sa$chd,boxp=FALSE,type="hist",col="gray")

logi.hist.plot(sa$alcohol,sa$chd,boxp=FALSE,type="hist",col="gray")
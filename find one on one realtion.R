
sa <-read.csv('http://math.mercyhurst.edu/~sousley/STAT_139/data/sahdd.csv', as.is = T);
head(sa)
attach(sa)

library(popbio)
logi.hist.plot(bodysize,survive,boxp=FALSE,type="hist",col="gray")
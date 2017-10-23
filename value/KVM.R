library(mda)
library(caret)
eddat <- read.csv('http://math.mercyhurst.edu/~sousley/STAT_139/data/EDDat3.csv', as.is = T);
attach(eddat);
sapply(eddat, function(x) sum(is.na(x))/length(x))
sapply(eddat, function(x) sum(!is.na(x))/length(x))
colSums(is.na(eddat))
table(na.omit(eddat[,c(3,5,6,9,13,25,39,40,12,27,32)])$Grp)
eddat <- na.omit(eddat[,c(3,5,6,9,13,25,39,40,12,27,32)])
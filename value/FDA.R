library(mda)
library(caret)
eddat <- read.csv('http://math.mercyhurst.edu/~sousley/STAT_139/data/EDDat3.csv', as.is = T);
attach(eddat);
sapply(eddat, function(x) sum(is.na(x))/length(x))
sapply(eddat, function(x) sum(!is.na(x))/length(x))
colSums(is.na(eddat))
table(na.omit(eddat[,c(3,5,6,9,13,25,39,40,12,27,32)])$Grp)
eddat <- na.omit(eddat[,c(3,5,6,9,13,25,39,40,12,27,32)])


edat1 <- fda(Grp ~ i2+i3+i6+i10+i22+i36+i37+i9+i24+i29, data = eddat, prior = c(1/3,1/3,1/3), CV = TRUE, method =
               polyreg, degree = 3)


confusionMatrix(eddat$Grp, predict(edat1))

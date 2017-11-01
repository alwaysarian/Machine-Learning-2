install.packages("ks")
library(ks)

library(caret)


eddat <- read.csv('http://math.mercyhurst.edu/~sousley/STAT_139/data/EDDat3.csv', as.is = T);

attach(eddat);


sapply(eddat, function(x) sum(is.na(x))/length(x))

sapply(eddat, function(x) sum(!is.na(x))/length(x))

colSums(is.na(eddat))

table(na.omit(eddat[,c(3,5,6,9,13,25,39,40,12,27,32)])$Grp)

eddat <- na.omit(eddat[,c(3,5,6,9,13,25,39,40,12,27,32)])

outKNN <- KernelKnn(x, TEST_data = NULL, as.numeric(y), k = 7, regression = FALSE,
                    Levels = unique(y), method = 'euclidean')
outKNN
Classinto <- c(0,0,0)
for (i in seq(length(y)))
{
  Classinto[i] <- match(1,match(outKNN[i,], max(outKNN[i,])))
}
Classinto
confusionMatrix(Classinto,y)




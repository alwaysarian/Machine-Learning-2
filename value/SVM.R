library(class) #k-nearest neighbors
library(kknn) #weighted k-nearest neighbors
library(MASS) # contains the data
library(kernlab) #assist with SVM feature selection
library(caret) #select tuning parameters
library(e1071) #SVM

library(caret)


eddat <- read.csv('http://math.mercyhurst.edu/~sousley/STAT_139/data/EDDat3.csv', as.is = T);

attach(eddat);


sapply(eddat, function(x) sum(is.na(x))/length(x))

sapply(eddat, function(x) sum(!is.na(x))/length(x))

colSums(is.na(eddat))

table(na.omit(eddat[,c(3,5,6,9,13,25,39,40,12,27,32)])$Grp)

eddat <- na.omit(eddat[,c(3,5,6,9,13,25,39,40,12,27,32)])
#test <- eddat[-inTrain,]
require(kernlab) #already loaded
edata3 <- ksvm(as.factor(Grp) ~ i2+i3+i6+i10+i22+i36+i37+i9+i24+i29,
                 data=eddat,
                 kernel="rbfdot", kpar = 'automatic', cross = 0,
                 prob.model=TRUE )

confusionMatrix(eddat$Grp, predict(edata3, test) )





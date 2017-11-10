library(nnet)

eddat <- read.csv('http://math.mercyhurst.edu/~sousley/STAT_139/data/EDDat3.csv', as.is = T);
attach(eddat);
sapply(eddat, function(x) sum(is.na(x))/length(x))
sapply(eddat, function(x) sum(!is.na(x))/length(x))
colSums(is.na(eddat))
table(na.omit(eddat[,c(3,5,6,9,13,25,39,40,12,27,32)])$Grp)
eddat <- na.omit(eddat[,c(3,5,6,9,13,25,39,40,12,27,32)])

Accuracies <- c(0.00)
for (i in seq(10))
{
  
  inTrain <- createDataPartition(y = eddat$Grp, p = .70, list = FALSE)
  training <- eddat[inTrain,]
  testing <- eddat[-inTrain,]
  mnmfit <- multinom(Grp ~ i2+i3+i6+i10+i22+i36+i37+i9+i24+i29, data=training, trace=FALSE)

  Accuracies[i] <- confusionMatrix(testing$Grp, predict(mnmfit, newdata=testing))$overall["Accuracy"]

}
summary(Accuracies)




library(caret)


eddat <- read.csv('http://math.mercyhurst.edu/~sousley/STAT_139/data/EDDat3.csv', as.is = T);

attach(eddat);


sapply(eddat, function(x) sum(is.na(x))/length(x))

sapply(eddat, function(x) sum(!is.na(x))/length(x))

colSums(is.na(eddat))

table(na.omit(eddat[,c(3,5,6,9,13,25,39,40,12,27,32)])$Grp)

eddat <- na.omit(eddat[,c(3,5,6,9,13,25,39,40,12,27,32)])


require(asbio)
Accuracies <- c(0.00)
for (i in seq(1000))
{
  inTrain <- createDataPartition(y = eddat$Grp, p = .70, list = FALSE)
  training <- eddat[inTrain,]
  edata2 <- glm(as.matrix(eddat[,c(2:11)]), eddat[,"Grp"], data = eddat, prior = c(1/3,1/3,1/3),
                subset = -inTrain, CV = T, family = binomial())
  Accuracies[i] <- confusionMatrix(eddat[-inTrain,"Grp"], edata2$class)$overall["Accuracy"]
}
summary(Accuracies)
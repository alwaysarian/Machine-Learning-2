
Accuracies <- c(0.00)
for (i in seq(1000))
{
  inTrain <- createDataPartition(y = eddat$Grp, p = .70, list = FALSE)
  training <- eddat[inTrain,]
  edata2 <- lda(as.matrix(eddat[,c(2:11)]), eddat[,"Grp"], data = eddat, prior = c(1/3,1/3,1/3),
               subset = -inTrain, CV = T)
  Accuracies[i] <- confusionMatrix(eddat[-inTrain,"Grp"], edata2$class)$overall["Accuracy"]
}
summary(Accuracies)
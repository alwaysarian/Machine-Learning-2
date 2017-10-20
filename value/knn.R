library(caret)


eddat <- read.csv('http://math.mercyhurst.edu/~sousley/STAT_139/data/EDDat3.csv', as.is = T);

attach(eddat);


sapply(eddat, function(x) sum(is.na(x))/length(x))

sapply(eddat, function(x) sum(!is.na(x))/length(x))

colSums(is.na(eddat))

table(na.omit(eddat[,c(3,5,6,9,13,25,39,40,12,27,32)])$Grp)

eddat <- na.omit(eddat[,c(3,5,6,9,13,25,39,40,12,27,32)])

Accuracies <- c(0.00)

for (i in seq(20)) 
{
  inTrain <- createDataPartition(y = as.factor(eddat$Grp), p = .70, list = FALSE)
  training <- eddat[inTrain,]
  testing <- eddat[-inTrain,]
  knn4 <- train(as.factor(Grp) ~ i2+i3+i6+i10+i22+i36+i37+i9+i24+i29, data = training, method = "knn",
                preProcess = c("center", "scale"), tuneLength = 10,
                trControl = trainControl(method = "cv"))
  update(knn4, list(.k = 3))
  knn4_pred <- predict(knn4,newdata = testing)
  Accuracies[i] <- confusionMatrix(knn4_pred,as.factor(testing$Grp))$overall["Accuracy"]
}
summary(Accuracies)

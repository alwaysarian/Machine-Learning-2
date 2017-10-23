library(caret)


eddat <- read.csv('http://math.mercyhurst.edu/~sousley/STAT_139/data/EDDat3.csv', as.is = T);

attach(eddat);


sapply(eddat, function(x) sum(is.na(x))/length(x))

sapply(eddat, function(x) sum(!is.na(x))/length(x))

colSums(is.na(eddat))

table(na.omit(eddat[,c(3,5,6,9,13,25,39,40,12,27,32)])$Grp)

eddat <- na.omit(eddat[,c(3,5,6,9,13,25,39,40,12,27,32)])

Accuracies <- c(0.00)
for (i in seq(1000)) 
{
  inTrain <- createDataPartition(y = as.factor(eddat$Grp), p = .70, list = FALSE)
  train <- eddat[inTrain,]
  test <- eddat[-inTrain,]
  nb1 <- train(as.factor(Grp) ~ i2+i3+i6+i10+i22+i36+i37+i9+i24+i29, data = train, method = "nb",
               trControl = trainControl(method = "cv"),
               tuneGrid = data.frame(usekernel = TRUE, fL = 0.5, adjust = 5))
  bps <- predict(nb1, newdata = test)
  Accuracies[i] <- confusionMatrix(test$Grp,bps)$overall["Accuracy"]
}

summary(Accuracies)
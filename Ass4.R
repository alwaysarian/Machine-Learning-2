Howells <- read.csv('http://math.mercyhurst.edu/~sousley/STAT_139/data/Howells.csv', as.is = T);
attach(Howells);
# this time get ALL predictors
HBNMF <- Howells[which(Pop == 'NORSE' | Pop == 'BERG'),];
H4A <- na.omit(HBNMF[,c(5:61,63,67:80)])
H4A$PopSex <- as.factor(H4A$PopSex)
table(H4A$PopSex)

library(MASS)
library(caret)
Accuracies <- c(0.00)
for (i in seq(5))
{
  hghgh
  update(knn4, list(.lambda = 3))
  knn4_pred <- predict(knn4,newdata = testing)
  Accuracies[i] <- confusionMatrix(knn4_pred,testing$PopSex)$overall["Accuracy"]
}
summary(Accuracies)


plot(density(Accuracies))
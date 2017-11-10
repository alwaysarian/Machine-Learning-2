require(caret) #select tuning parameters
require(e1071) #SVM
set.seed(7); #replicable across our PCs? no.
Howells <- read.csv('http://math.mercyhurst.edu/~sousley/STAT_139/data/Howells.csv', as.is = T);
attach(Howells)
H4A <- Howells[which(Pop == 'NORSE' | Pop == 'BERG'),];
# many columns are Nas
H4A <- na.omit(H4A[,c(5:61,63,67:80)]);
H4A$PopSex <- as.factor(H4A$PopSex);
# standardize vars ALWAYS with NN
H4AS <- cbind(H4A["PopSex"], scale(H4A[2:72]) )




# time how long it takes
start_time <- Sys.time()
Accuracies <- c(0.00)
# suppress console output
sink('sinktest.txt')
for (i in seq(100))
{
  # sample
  inTrain <- createDataPartition(H4AS$PopSex, p = .75, list = FALSE)
  H4AS.nn4 <- avNNet(PopSex ~ ., data = H4AS, subset = inTrain, size = 4, rang = 0.5, decay = 1, maxit = 200, repeats = 10)
  Accuracies[i] <- confusionMatrix(H4AS$PopSex[-inTrain], predict(H4AS.nn4, H4AS[-inTrain,], type = "class"))$overall["Accuracy"]
  }
# turn output on again
sink()
summary(Accuracies)
acclen = length(na.omit(Accuracies))
plot(density(Accuracies), main = paste('Average Neural Network Model accuracies:',acclen, 'runs') )
# time to execute 28 seconds without console, 29.5 with
end_time <- Sys.time()
end_time - start_time
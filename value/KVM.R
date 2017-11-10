library(mda)
library(caret)
library(class) #k-nearest neighbors
library(kknn) #weighted k-nearest neighbors
library(MASS) # contains the data
library(kernlab) #assist with SVM feature selection
library(e1071) #SVM

eddat <- read.csv('http://math.mercyhurst.edu/~sousley/STAT_139/data/EDDat3.csv', as.is = T);
attach(eddat);
sapply(eddat, function(x) sum(is.na(x))/length(x))
sapply(eddat, function(x) sum(!is.na(x))/length(x))
colSums(is.na(eddat))
table(na.omit(eddat[,c(3,5,6,9,13,25,39,40,12,27,32)])$Grp)
eddat <- na.omit(eddat[,c(3,5,6,9,13,25,39,40,12,27,32)])

Accuracies <- c(0.00)
for (i in seq(2))
{

inTrain <- createDataPartition(y = as.factor(eddat$Grp), p = .70, list = FALSE)
train <- eddat[inTrain,]
test <- eddat[-inTrain,]
# book code, demo code from e1071
linear.tune = tune.svm(as.factor(Grp) ~ i2+i3+i6+i10+i22+i36+i37+i9+i24+i29, data=eddat, kernel="linear",
                       cost=c(0.001, 0.01, 0.1, 1,5,10))
poly.tune = tune.svm(as.factor(Grp)~i2+i3+i6+i10+i22+i36+i37+i9+i24+i29, data=eddat, kernel="polynomial",
                     degree=c(3,4,5), coef0=c(0.1,0.5,1,2,3,4))

summary(linear.tune)

summary(poly.tune)

best.linear = linear.tune$best.model
tune.test = predict(best.linear, newdata=test)

Accuracies[i] <- confusionMatrix(test$Grp, tune.test)$overall["Accuracy"]
}

summary(Accuracies)
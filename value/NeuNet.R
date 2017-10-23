library(caret)


eddat <- read.csv('http://math.mercyhurst.edu/~sousley/STAT_139/data/EDDat3.csv', as.is = T);

attach(eddat);


sapply(eddat, function(x) sum(is.na(x))/length(x))

sapply(eddat, function(x) sum(!is.na(x))/length(x))

colSums(is.na(eddat))

table(na.omit(eddat[,c(3,5,6,9,13,25,39,40,12,27,32)])$Grp)

eddat <- na.omit(eddat[,c(3,5,6,9,13,25,39,40,12,27,32)])


levels (eddat$Grp) <- c("0","1")
eddat$Grp <- as.numeric(as.character(eddat$Grp))
# sample
inTrain <- createDataPartition(eddat$Grp, p = .70, list = FALSE)
training <- eddat[inTrain,]
testing <- eddat[-inTrain,]
# let's try ALL predictors
formula <- Grp~i2+i3+i6+i10+i22+i36+i37+i9+i24+i29
fitALL <- neuralnet (formula, data = eddat, hidden = 10, algorithm = "rprop+")

pred2ALL <- compute (fitALL, testing[,c(2:11)] )
preds <- sign(pred2ALL$net.result)

table(sign(pred2ALL$net.result), sign(testing[,1]), dnn = c("Predicted", "Observed"))


for (i in seq(length(preds))) {
  preds[i] <- if (preds[i] == -1) 0 else 1 ;
}


confusionMatrix(preds[,1], testing[,1])



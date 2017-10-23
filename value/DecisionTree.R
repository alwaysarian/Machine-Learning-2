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
  inTest<-createDataPartition(eddat$Grp, p = .25, list = FALSE)
  require(rpart)
  edata1<-rpart(Grp~ ., data=eddat,method = "class",subset = inTest,
   parms= list(split = "gini",prior = c(1/3,1/3,1/3)),
   control = rpart.control(usesurrogate= 0, maxsurrogate= 0))
  
   Accuracies[i] <- confusionMatrix(eddat[inTest,"Grp"],predict(edata1,newdata= eddat[inTest,],type = "class"))$overall["Accuracy"]
    
}
summary(Accuracies)
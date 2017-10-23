library(nnet)

eddat <- read.csv('http://math.mercyhurst.edu/~sousley/STAT_139/data/EDDat3.csv', as.is = T);
attach(eddat);
sapply(eddat, function(x) sum(is.na(x))/length(x))
sapply(eddat, function(x) sum(!is.na(x))/length(x))
colSums(is.na(eddat))
table(na.omit(eddat[,c(3,5,6,9,13,25,39,40,12,27,32)])$Grp)
eddat <- na.omit(eddat[,c(3,5,6,9,13,25,39,40,12,27,32)])



mnmfit <- multinom(Grp ~ i2+i3+i6+i10+i22+i36+i37+i9+i24+i29,data=eddat, trace=FALSE)
summary(mnmfit)

# get predictions on a new data set
round(predict(mnmfit, newdata=eddat, "probs"),4)

predict(mnmfit, newdata=eddat, "class")

#Multinominal

confusionMatrix(predict(mnmfit, newdata=eddat, "class"), eddat$Grp)

# compare to ldfa and qdfa
ldfa4 <- lda(PopSex~GOL+NOL+BNL+BBH+XCB,data=HBNMF, CV = T, priors =
               c(1,1,1,1))
confusionMatrix(ldfa4$class, HBNMF$PopSex)
qdfa4 <- qda(PopSex~GOL+NOL+BNL+BBH+XCB,data=HBNMF, CV = T, priors =
               c(1,1,1,1))
confusionMatrix(qdfa4$class, HBNMF$PopSex)



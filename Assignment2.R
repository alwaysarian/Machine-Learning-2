#All required library
install.packages("caret")
library(caret)
install.packages("e1071")
library("e1071")


sa <-read.csv('http://math.mercyhurst.edu/~sousley/STAT_139/data/sahdd.csv', as.is = T);
head(sa)
attach(sa)

chdfit <- glm(as.factor(chd)~ sbp+tobacco+ldl+adiposity+famhist+typea+obesity+alcohol+age,data=sa,family=binomial())

summary(chdfit)

exp(coef(chdfit))

chdfac <- fitted(chdfit)
thresh <- 0.5
gchd <- cut(chdfac, breaks=c(-Inf, thresh, Inf), labels=c(0,1))

confusionMatrix(gchd, sa$chd) 

salda <- lda(as.factor(chd)~ sbp+tobacco+ldl+adiposity+famhist+typea+obesity+alcohol+age,data=sa, CV=T)
confusionMatrix(salda$class, sa$chd)

saqda <- qda(as.factor(chd)~ sbp+tobacco+ldl+adiposity+famhist+typea+obesity+alcohol+age,data=sa, CV=T)
confusionMatrix(saqda$class, sa$chd)
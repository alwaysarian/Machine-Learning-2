install.packages("caret")
install.packages("e1071")
install.packages("ks")
install.packages("KernelKnn")
install.packages("class")

#All necessary Library
library(caret)
library("e1071")
library(ks)
library(KernelKnn)
library("class")

sa <-read.csv('http://math.mercyhurst.edu/~sousley/STAT_139/data/sahdd.csv', as.is = T);
head(sa)
attach(sa)

x = na.omit(sa[c(2, 3, 4,5,7,8,9,10)])
y = sa$chd


#using knnmethod with k =1

kcres1 <- knn.cv(x, y, k = 1, prob = TRUE)
round(100*prop.table(table(kcres1,y),1),1)

confusionMatrix(kcres1,y)

#using knnmethod with k =5

kcres5 <- knn.cv(x, y, k = 5, prob = TRUE)
round(100*prop.table(table(kcres5,y),1),1)

confusionMatrix(kcres5,y)

#using knnmethod with k =11

kcres11 <- knn.cv(x, y, k = 11, prob = TRUE)
round(100*prop.table(table(kcres11,y),1),1)

confusionMatrix(kcres11,y)

#using knnmethod with k =19

kcres19 <- knn.cv(x, y, k = 19, prob = TRUE)
round(100*prop.table(table(kcres19,y),1),1)

confusionMatrix(kcres19,y)


#KPD

#normalize the data

sa <-read.csv('http://math.mercyhurst.edu/~sousley/STAT_139/data/sahdd.csv', as.is = T);
head(sa)
attach(sa)

x = na.omit(sa[c(2, 3, 4,5,7,8,9,10)])
y = sa$chd

levels(as.factor(y))

levels(as.factor(y))[1]

y[which( y == levels(as.factor(y))[2])] <- '2'

y[which( y == levels(as.factor(y))[1])] <- '1'

y <- as.numeric(y)

#using euclidean

outKNN <- KernelKnn(x, TEST_data = NULL, as.numeric(y), k = , regression = FALSE,
                    Levels = unique(y), method = 'euclidean', weights_function = 'logistic')

Classinto <- c(0,0,0)
for (i in seq(length(y)))
{
  Classinto[i] <- match(1,match(outKNN[i,], max(outKNN[i,])))
}

confusionMatrix(Classinto,y)

#using minkowski

outKNN <- KernelKnn(x, TEST_data = NULL, as.numeric(y), k = , regression = FALSE,
                    Levels = unique(y), method = 'minkowski', weights_function = 'logistic')

Classinto <- c(0,0,0)
for (i in seq(length(y)))
{
  Classinto[i] <- match(1,match(outKNN[i,], max(outKNN[i,])))
}

confusionMatrix(Classinto,y)


#using mahalanobis

outKNN <- KernelKnn(x, TEST_data = NULL, as.numeric(y), k = 3, regression = FALSE,
                    Levels = unique(y), method = 'mahalanobis', weights_function = 'logistic')

Classinto <- c(0,0,0)
for (i in seq(length(y)))
{
  Classinto[i] <- match(1,match(outKNN[i,], max(outKNN[i,])))
}

confusionMatrix(Classinto,y)



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>




out = KernelKnn(x, TEST_data = NULL, as.numeric(y), k = 5, regression = FALSE, Levels = unique(y), threads =
                  2, method = 'mahalanobis')



compare.kda.cv (na.omit(sa[2:10]),na.omit(sa$chd), by.group=T)
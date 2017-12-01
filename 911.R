require(rpart) #classification and regression trees (CART)
require(partykit) #treeplots
require(MASS) #breast and pima indian data
require(ElemStatLearn) #prostate data
require(randomForest) #random forests
require(gbm) #gradient boosting
require(caret) #tune hyper-parameters

data(prostate)

prostate$gleason = ifelse(prostate$gleason == 6, 0, 1)
pros.train = subset(prostate, train==TRUE)[,1:9]
pros.test = subset(prostate, train==FALSE)[,1:9]
# very simple call, many defaults
tree.pros = rpart(lpsa~., data=pros.train)
print(tree.pros$cptable)

print(tree.pros$cptable) 

# Predict test set using training model
party.pros.test = predict(tree.pros, newdata=pros.test)
rpart.resid = party.pros.test - pros.test$lpsa #calculate residuals
rpart.resid

# calculate RMSE - what we want to minimize
mean(rpart.resid^2)


# sample
inTrain <- createDataPartition(prostate$lpsa, p = .80, list = FALSE)
training <- prostate[inTrain,]
testing <- prostate[-inTrain,]
# fit training sample
PT <- train(training[,c(1,2,3,5:9)], training[,"lpsa"], method = "rpart", metric = "RMSE", trControl =
              trainControl(method = "cv"))
PT


PT$bestTune


# the cp changes a little every time, so FIRST let's get the mean of the best cp to use.
bestcps <- c(0.00)
for (i in seq(100))
{
  # sample
  inTrain <- createDataPartition(prostate$lpsa, p = .80, list = FALSE)
  training <- prostate[inTrain,]
  testing <- prostate[-inTrain,]
  # fit training sample
  PT <- train(training[,c(1,2,3,5:9)], training[,"lpsa"], method = "rpart", metric = "RMSE", trControl
              = trainControl(method = "cv"))
  bestcps[i] <- PT$bestTune
  print(i) # give feedback
}
median(as.numeric(bestcps))



# the median cp was 0.117201. Now we want RMSE
RMSEs <- c(0.00)
for (i in seq(100)) # 100 took 40 seconds on my office computer
{
  # sample
  inTrain <- createDataPartition(prostate$lpsa, p = .80, list = FALSE)
  training <- prostate[inTrain,]
  testing <- prostate[-inTrain,]
  # fit training sample
  PT <- train(training[,c(1,2,3,5:9)], training[,"lpsa"], method = "rpart", metric = "RMSE", trControl
              = trainControl(method = "cv"))
  PTT = predict(PT, newdata = testing)
  PTT.resid = PTT - testing$lpsa #calculate residuals
  RMSEs [i] <- mean(PTT.resid^2)
  print(i) # give feedback
}
median(as.numeric(RMSEs))

plot(density(as.numeric(RMSEs)))

prostate$train <- NULL
RFP = randomForest(lpsa~., data=prostate) # no need to worry about overfitting
print(RFP)

plot(RFP)
which.min(RFP$mse)

# plot importance
varImpPlot(RFP, main="Variable Importance Plot - PSA Score")
# get numbers
importance(RFP)


RFP = randomForest(lpsa~., data=prostate, ntree = 325)
print(RFP)


#>>>>>>>>>>>>>>>>>>>.

grid = expand.grid(.n.trees=seq(100,500, by=200), .interaction.depth=seq(1,4, by=1),
                   .shrinkage=c(.001,.01,.1), .n.minobsinnode=10)
grid

inTrain <- createDataPartition(prostate$lpsa, p = .80, list = FALSE)
training <- prostate[inTrain,]
testing <- prostate[-inTrain,]
PGBT = train(lpsa~., data=training, method="gbm", trControl = trainControl(method="cv"), tuneGrid=grid)
PGBT

PGBT = train(lpsa~., data=training, method="gbm", trControl = trainControl(method="cv"), tuneGrid=grid)
PGBT


GBPR = gbm(lpsa~., data = training, n.trees=300, interaction.depth=2, shrinkage=0.01, distribution="gaussian",n.core=2)
GBPP = predict(GBPR, newdata = testing, n.trees = 300)
# residuals
GBPres = GBPP - testing$lpsa
mean(GBPres^2)


plot(density(as.numeric(RMSEs)))


RMSEs <- c(0.00)
for (i in seq(100)) # 100 runs fast
{
  # sample
  inTrain <- createDataPartition(prostate$lpsa, p = .80, list = FALSE)
  training <- prostate[inTrain,]
  testing <- prostate[-inTrain,]
  # fit training sample
  GBPR = gbm(lpsa~., data = training, n.trees=300, interaction.depth=4, shrinkage=0.01, distribution="gaussian",
             n.cores = 4)
  GBPP = predict(GBPR, newdata = testing, n.trees = 700)
  # residuals
  RMSEs [i] <- mean((GBPP - testing$lpsa)^2)
  print(i) # give feedback
}
median(as.numeric(RMSEs))
#[1] 0.6543133
plot(density(as.numeric(RMSEs)))
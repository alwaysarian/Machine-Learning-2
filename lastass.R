#required packages

#install.packages("glmnet")
library(glmnet) # lasso and ridge regression
#install.packages("ISLR")
library(ISLR)
library(caret) #tune hyper-parameters
library(leaps) # subset selection


FStat <- read.csv('http://math.mercyhurst.edu/~sousley/STAT_139/data/FStat.csv')
colnames(FStat) 
WMStat <- FStat[which(FStat$PopSex == "WM"),]



WMStat <- WMStat[,c(7:18)]
WMStat <- na.omit(WMStat)

# Perform subset selection

reg.ss <- regsubsets(FSTAT~., WMStat)

reg.ss <- regsubsets(FSTAT~., WMStat, nvmax =11)
summary (reg.ss)

res.reg.ss <- summary(reg.ss)

res.reg.ss$rsq

plot(res.reg.ss$rss ,xlab=" Number of Variables ",ylab=" RSS", type = 'l')
plot(res.reg.ss$adjr2 ,xlab =" Number of Variables ", ylab=" Adjusted RSq",type = 'l')

bestset <- which.max(res.reg.ss$adjr2)
bestset

points (bestset, res.reg.ss$adjr2[bestset], col = "red",cex = 1.5, pch = 20)

plot(res.reg.ss$cp ,xlab =" Number of Variables ", ylab = "Cp", type = 'l')

cpmin <- which.min (res.reg.ss$cp )
cpmin

points (cpmin, res.reg.ss$cp [cpmin], col ="red",cex = 1.5, pch = 20)

bicmin <- which.min(res.reg.ss$bic)
bicmin

plot(res.reg.ss$bic ,xlab=" Number of Variables ", ylab = " BIC",type = 'l')
points (bicmin, res.reg.ss$bic[3], col =" red", cex = 2, pch = 20)

par(mfrow =c(2,2))

par(mfrow =c(1,1))
par(mfrow =c(2,2))
plot(reg.ss,scale ="r2")
plot(reg.ss,scale ="adjr2")
plot(reg.ss,scale ="Cp")
plot(reg.ss,scale ="bic")

bicmin
coef(reg.ss, bicmin)

# Forward selection

reg.fwd <- regsubsets (FSTAT~claxln+scapht+scapbr+humxln+radxln+ulnxln+sacaht+femxln+fembln+tibxln+fibxln, WMStat,  nvmax =11, method = "forward")

summary(reg.fwd)

which.min(summary(reg.fwd)$bic)

which.min(summary(reg.fwd)$adjr2)

which.min(summary(reg.fwd)$cp)

# best subsets using bicmin (3)
coef(reg.ss, 3)

# best 6 using forward stepwise
coef(reg.fwd, 3)

# best subsets with 7
coef(reg.ss, 4)

# best forward with 7
coef(reg.fwd, 4)


#choosing best predicition model






set.seed (7)
# not "stratified" but does not make sense here, simply random 50/50, T/F for every record
train <- sample(c(TRUE ,FALSE), nrow(WMStat),rep = TRUE)
test <- (!train)
# use best subset selection on training set
regfit.ss = regsubsets(FSTAT~., data = WMStat[train,], nvmax =11)
# set up "x" model matrix
test.mat <- model.matrix(FSTAT~., data = WMStat[test,])
test.mat


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#notworking
val.errors =rep(NA ,11)
for(i in 2:11)
{
  coefi <- coef(regfit.ss, id=i)
  pred <- test.mat[,names(coefi)]%*% coefi # matrix multiplication
  val.errors[i] <- mean(( WMStat$FSTAT[test]-pred)^2)
}
val.errors

which.min(val.errors) 
#till here

coef(regfit.ss,3)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#Let's check if this workds

predict.regsubsets = function(object, newdata, id,...)
{
  form = as.formula(object$call[[2]]) # extract model ("call")
  mat = model.matrix (form, newdata)
  coefi = coef(object, id=id)
  xvars = names(coefi)
  mat[,xvars] %*% coefi
}

regfit.ss = regsubsets(FSTAT~claxln+scapht+scapbr+humxln+radxln+ulnxln+sacaht+femxln+fembln+tibxln+fibxln, data=WMStat, nvmax =11)

coef(regfit.ss,3)

#need to do model slection






# Ridge Regression

x <- model.matrix (FSTAT~.,WMStat )[,-1]
y <- WMStat$FSTAT

#>>>>>>>>>>>>>>>>>
grid <- 10^seq (10,-2, length =100)
grid

ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid)
coef(ridge.mod) # 20 predictors, 100 lambdas

ridge.mod$lambda [50]

coef(ridge.mod)[,50]
#>>>>>>>>>>>>>>>>>>>>>>>>>>




set.seed(7)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train )
y.test <- y[test]
# use the grid again
ridge.mod <- glmnet(x[train,], y[train], alpha =0, lambda =grid, thresh = 1e-12)
# predict using glmnet : note the use of newx
ridge.pred <- predict(ridge.mod, s = 4, newx = x[test ,])
mean((ridge.pred - y.test)^2)

ridge.pred


mean((mean(y[train])- y.test)^2)

ridge.pred <- predict(ridge.mod, s=1e10, newx=x[test,])
mean((ridge.pred - y.test)^2)


ridge.pred <- predict(ridge.mod, s = 0, newx = x[test,])
mean((ridge.pred - y.test)^2)

set.seed (7)
cv.out <- cv.glmnet(x[train,], y[train], alpha = 0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam


ridge.pred <- predict(ridge.mod, s = bestlam, newx = x[test,])
mean((ridge.pred - y.test)^2)

# in this case, it got worse, in contrast to chapter results!
# ridge again
rreg2 <- glmnet(x, y, alpha = 0)
predict(rreg2, type = "coefficients", s = bestlam)[1:12,]


#The Lasso

lasso.mod <- glmnet(x[train,], y[train], alpha = 1, lambda = grid)
plot(lasso.mod)


#Let's cross-validate to get the best lambda
set.seed(7)
cv.out <- cv.glmnet(x[train,], y[train], alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s = bestlam, newx=x[test,])
mean((lasso.pred - y.test)^2)


bestlam

log(bestlam)


#Let's use a lambda of 2.8
lasso.pred <- predict(lasso.mod, s = -.8, newx=x[test,])
mean((lasso.pred - y.test)^2) # before was 23.14108


#Let's use a lambda of 2.4
lasso.pred <- predict(lasso.mod, s = .3, newx=x[test,])
mean((lasso.pred - y.test)^2) # before was 23.14108
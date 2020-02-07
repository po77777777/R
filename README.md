# kaggle house price
## Using multiple regression


### Linear regression 

``` R
train1.lm <- subset(train1,select=-MSSubClass) 
test1.lm <- subset(test1,select=-MSSubClass) 
lm.pred <- lm(log(SalePrice)~.,data=train1.lm) 
pred.lm <- exp(predict(lm.pred,test1.lm)) 
par(mfrow=c(2,2))
plot(lm.pred)
pred.lm <- as.vector(pred.lm) 
csv.function(pred.lm,name="lm.csv")
```


### Subset selection methods

#### Best subset

```R
best.subset <- regsubsets(log(SalePrice)~.,data=train1,nvmax=3,really.big=TRUE)
best.summary <- summary(best.subset) par(mfrow=c(2,2))
# RSS plot
plot(best.summary$rss ,xlab="Number of Variables ", ylab="RSS", type="l")
# adjr2 plot
adjr2.best <- which.max(best.summary$adjr2)
plot(best.summary$adjr2 ,xlab="Number of Variables ", ylab="Adjusted RSq", type="l") points(adjr2.best,best.summary$adjr2[adjr2.best], col = "red", cex = 2, pch = 20)
pred.best <- exp(predict.regsubsets(best.subset,test1,id=adjr2.best)) csv.function(pred.best,name="best.csv")
# Cp plot
cp.best <- which.min(best.summary$cp)
plot(best.summary$cp ,xlab="Number of Variables ", ylab="Cp", type="l") points(cp.best,best.summary$cp[cp.best], col = "red", cex = 2, pch = 20)
# BIC plot
bic.best <- which.min(best.summary$bic)
plot(best.summary$bic ,xlab="Number of Variables ", ylab="BIC", type="l") points(bic.best,best.summary$bic[bic.best], col = "red", cex = 2, pch = 20)
```

#### Forward subset 

```R
forward.subset <- regsubsets(log(SalePrice)~.,data=train1,nvmax=200,method="forward",really.big=TRUE)

for.summary <- summary(forward.subset) par(mfrow=c(2,2))
# RSS plot
plot(for.summary$rss ,xlab="Number of Variables ", ylab="RSS", type="l")
# adjr2 plot
adjr2.for <- which.max(for.summary$adjr2)
plot(for.summary$adjr2 ,xlab="Number of Variables ", ylab="Adjusted RSq", type="l") points(adjr2.for,for.summary$adjr2[adjr2.for], col = "red", cex = 2, pch = 20)
# Cp plot
cp.for <- which.min(for.summary$cp)
plot(for.summary$cp ,xlab="Number of Variables ", ylab="Cp", type="l") points(cp.for,for.summary$cp[cp.for], col = "red", cex = 2, pch = 20)
# BIC plot
bic.for <- which.min(for.summary$bic)
plot(for.summary$bic ,xlab="Number of Variables ", ylab="BIC", type="l") points(bic.for,for.summary$bic[bic.for], col = "red", cex = 2, pch = 20)

pred.forward <- exp(predict.regsubsets(forward.subset,test1,id=bic.for)) csv.function(pred.forward,name="forward.csv")

c(adjr2.for,cp.for,bic.for)
```

#### Shrinkage methonds

Ridge 

```R
grid <- 10^seq(10,-2,length=100)
###model
ridge.mod <- glmnet(train.X,log(train.y),alpha=0,lambda=grid)
###cross-validation
cv.ridge <- cv.glmnet(train.X,log(train.y),alpha=0,nfolds = 10) 
plot(cv.ridge)
```


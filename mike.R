gbmCV <- function(x, y, folds, tree_inc, dist="adaboost", id=1, bf=.5, sh=.001, singlefold=F) {
  
  getDev <- function(p, y) -mean(ifelse(y==1, log(p), log(1-p)))
  getDevmod <- function(p, y) getDev(pmin(pmax(p, .001), .999), y)
  getMse <- function(p, y) mean((y-p)^2)
  getMisclass <- function(p, y) mean(abs((p>.5)-y))
  getAUC <- function(p, y) auc(y,p)
  
  
  dev <- vector()
  devmod <- vector()
  mse <- vector()
  misclass <- vector()
  auc1 <- vector()
  cv_num <- if (singlefold) 1 else length(folds)
  for (i in 1:cv_num) {
    train_id <- Reduce(union, folds[-i])
    test_id <- folds[[i]]
    xtrain <- x[train_id,,drop=F]
    ytrain <- y[train_id]
    xtest <- x[test_id,,drop=F]
    ytest <- y[test_id]
    recover()
    logist_fit <- glm(ytrain ~ xtrain, family = "binomial") #might need to adjust formula code here
    # what are options for missingness
    ## predicted probability of class 1
    pred <- predict(logist_fit, newdata=xtest, type="response") # type = "response" 
    # subset xtest to be same length as logist_fit
    dev[i] <- getDev(pred, ytest)
    devmod[i] <- getDevmod(pred, ytest)
    mse[i] <- getMse(pred, ytest)
    misclass[i] <- getMisclass(pred, ytest)
    auc1[i] <- getAUC(pred, ytest)
  }
  
  cbind(dev, devmod, mse, misclass, auc=auc1, best_tree)
} # end gbmCV

  getDev <- function(p, y) -mean(ifelse(y==1, log(p), log(1-p)), na.rm=TRUE)
  getDevmod <- function(p, y) getDev(pmin(pmax(p, .001), .999), y)
  getMse <- function(p, y) mean((y-p)^2, na.rm=TRUE)
  getMisclass <- function(p, y) mean(abs((p>.5)-y), na.rm=TRUE)
  getAUC <- function(p, y) auc(y,p)
  
glmcv <- function(x, # df of covariates
                  y, # response vector
                  cv_num=5, # folds for cross validation
                  family = "binomial" # passed to glm()
                  ) 
  {
  dev <- vector()
  devmod <- vector()
  mse <- vector()
  misclass <- vector()
  auc1 <- vector()
  folds<- caret::createFolds(which(!is.na(y)), k = cv_num, list=TRUE, returnTrain=FALSE)
  for (i in 1:cv_num) {
    train_id <- Reduce(union, folds[-i])
    test_id <- folds[[i]]
    xtrain <- x[train_id,,drop=FALSE]
    ytrain <- y[train_id]
    xtest <- x[test_id,,drop=FALSE]
    ytest <- y[test_id]
    train_df <- data.frame(ytrain, xtrain)
    test_df <- data.frame(ytest, xtest)

    logist_fit <- glm(ytrain~ sex + age + highchol2011 + smokever2011 + diabetes2011 + highbp2011, family = family, data = xtrain) #might need to adjust formula code here
    # what are options for missingness
    ## predicted probability of class 1
    pred <- predict(logist_fit, newdata=test_df, type="response") # type = "response" 
    # subset xtest to be same length as logist_fit
    dev[i] <- getDev(pred, ytest)
    devmod[i] <- getDevmod(pred, ytest)
    mse[i] <- getMse(pred, as.numeric(ytest))
    misclass[i] <- getMisclass(pred, as.numeric(ytest))
    auc1[i] <- getAUC(pred, ytest)
  }
  
  cbind(dev, devmod, mse, misclass, auc=auc1)
} # end glmCV

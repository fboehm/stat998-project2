gbmCV <- function(x, y, folds, tree_inc, dist="adaboost", id=1, bf=.5, sh=.001, singlefold=F) {
  
  getDev <- function(p, y) -mean(ifelse(y==1, log(p), log(1-p)), na.rm=TRUE)
  getDevmod <- function(p, y) getDev(pmin(pmax(p, .001), .999), y)
  getMse <- function(p, y) mean((y-p)^2, na.rm=TRUE)
  getMisclass <- function(p, y) mean(abs((p>.5)-y), na.rm=TRUE)
  getAUC <- function(p, y) auc(y,p)
  
  gbmInit2 <- function(y, x, tree_inc) {
    tree_ct <- tree_inc
    gbm_fit <- gbm(y~., data=x, distribution=dist, n.tree=tree_inc,
                   shrinkage=sh, interaction.depth=id, bag.fraction=bf)
    best_tree <- gbm.perf(gbm_fit, method="OOB")
    while (best_tree/tree_ct>.99) {
      tree_ct <- tree_ct + tree_inc
      gbm_fit <- gbm.more(gbm_fit, n.new.trees=tree_inc)
      best_tree <- gbm.perf(gbm_fit, method="OOB")
    }
    list(gbm_fit=gbm_fit, best_tree=best_tree)
  }
  
  best_tree <- vector()
  dev <- vector()
  devmod <- vector()
  mse <- vector()
  misclass <- vector()
  auc1 <- vector()
  for (i in 1:cv_num) {
    train_id <- Reduce(union, folds[-i])
    test_id <- folds[[i]]
    xtrain <- x[train_id,,drop=F]
    ytrain <- y[train_id]
    xtest <- x[test_id,,drop=F]
    ytest <- y[test_id]
    init <- gbmInit2(ytrain, xtrain, tree_inc)
    gbm_fit <- init$gbm_fit
    best_tree[i] <- init$best_tree
    ## predicted probability of class 1
    pred <- predict(gbm_fit, newdata=xtest, n.trees=best_tree[i], type="response")
    dev[i] <- getDev(pred, ytest)
    devmod[i] <- getDevmod(pred, ytest)
    mse[i] <- getMse(pred, ytest)
    misclass[i] <- getMisclass(pred, ytest)
    auc1[i] <- getAUC(pred, ytest)
  }
  
  cbind(dev, devmod, mse, misclass, auc=auc1, best_tree)
} # end gbmCV

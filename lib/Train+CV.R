###########################################################################
####### This file (Train.R) is consistent of six training models.   #######
####### They are GBM, SVM with Linear Kernel, SVM with RBF Kernel,  #######
####### Random Forest, xgboost, and DeepBoost.                      #######

###########################################################################

###### Given the feature extracted from HOG in the "feature.R" file, ######
###### the BEST classifier is the DeepBoost.                         ######
###### The training error is about 0.044,                            ######
###### and the test error is about 0.005.                            ######

###########################################################################

###### Overall, our best model is feature HOG + DeepBoost.           ######

###########################################################################

#load requied packages

library("caret")
library("gbm")
library("randomForest")
library("plyr")
library("xgboost")
library("fastAdaboost")
library("e1071")
library("deepboost")

setwd("your locate path")
X <- read.csv("../data/sift_features.csv")
X <- t(X)

labels <- read.csv("../data/labels.csv")
labels <- labels[,1]
#dat_train<-X
dat_train<-dat
#dat_train<-cbind(X,dat)
#dat_train<-H
#dat_train<-cbind(P,H)
label_train <- labels


#Train <- function(dat_train, label_train){
  
  n <- nrow(dat_train)
  
  r <- sample(1:n, n)
  
  #r<-sample(1:n,n*0.8)
    
  dat_train <- dat_train[r,]
  
  label_train <- label_train[r]
  
  data.all <- as.data.frame(cbind(dat_train, label_train))
  
  colnames(data.all)[ncol(data.all)] <- "Label"
  
  data.all$Label <- as.factor(data.all$Label)
  
  control <- trainControl(method = 'cv', number = 5)  #use 5-fold cross validation
  
  inTrain <- createDataPartition(y = data.all$Label, p=0.75, list=FALSE)
  
  training <- data.all[inTrain, ]
  
  testing <- data.all[-inTrain, ]
  

  ##### GBM ######
 gbm.tuned<-gbm(Label~., data=training, interaction.depth = gbmfit$bestTune$interaction.depth,
                n.trees = gbmfit$bestTune$n.trees, shrinkage = gbmfit$bestTune$shrinkage,
                n.minobsinnode = gbmfit$bestTune$n.minobsinnode,distribution = "bernoulli" )
  
  
  gbm.test<-gbm(Label~., data=testing, interaction.depth = gbmfit$bestTune$interaction.depth,
               n.trees = gbmfit$bestTune$n.trees,shrinkage = gbmfit$bestTune$shrinkage,
               n.minobsinnode = gbmfit$bestTune$n.minobsinnode, distribution = "bernoulli")
  
  #Train error
  train.err.gbm <- sum(predict(gbm.tuned, training) != training$Label)/nrow(training)
  train.err.gbm
  
  #Test error
  test.err.gbm <- sum(predict(gbm.test, testing) != testing$Label)/nrow(testing)
  test.err.gbm
  
  
  ###### Random Forest ######
  rfGrid <- expand.grid(mtry = 2^(5:10) )
  
  rffit <- train(Label~., data = training,
                 method = "rf", trControl = control, tuneGrid = rfGrid) #parameter tuning
  
  
  rf.tuned<-randomForest(Label~., data=training, mtry=rffit$bestTune$mtry)
  rf.test<-randomForest(Label~., data=testing,mtry=rffit$bestTune$mtry)
  
  #Train error
  train.err.rf <- sum(predict(rf.tuned, training) != training$Label)/nrow(training)
  train.err.rf

  #Test error
  test.err.rf <- sum(predict(rf.tuned, testing) != testing$Label)/nrow(testing)
  test.err.rf
  

  ###### SVM with Linear Kernel ######

  svmGrid.linear <- expand.grid(C= 2^c(0:5))
  svmfit.linear <- train(Label~., data = training,
                         method = "svmLinear", trControl = control, tuneGrid = svmGrid.linear, preProc = c("center","scale")
  )
  
  svm.linear.tuned<-svm(Label~., data=training, kernel="linear",cost=svmfit.linear$bestTune$C)
  svm.linear.test<-svm(Label~., data=testing, kernel="linear",cost=svmfit.linear$bestTune$C)
  
  #Train error
  train.err.svm.l <- sum(predict(svm.linear.tuned, training) != training$Label)/nrow(training)
  train.err.svm.l
  
  #Test error
  test.err.svm.l <- sum(predict(svm.linear.test, testing) != testing$Label)/nrow(testing)
  test.err.svm.l
  
  
  
  ###### SVM with RBF Kernel ######

  svmGrid <- expand.grid(sigma= 2^c(-25, -20, -15,-10, -5, 0), C= 2^c(0:5))
  svmfit <- train(Label~., data = training,
                  method = "svmRadial", trControl = control, tuneGrid = svmGrid, preProc = c("center","scale")
  )
  
  svm.tuned<-svm(Label~., data=training, kernel="radial",gamma=svmfit$bestTune$sigma,cost=svmfit$bestTune$C)
  svm.test<-svm(Label~., data=testing, kernel="radial",gamma=svmfit$bestTune$sigma,cost=svmfit$bestTune$C)
  
  #Train error
  train.err.svm <- sum(predict(svm.tuned, training) != training$Label)/nrow(training)
  train.err.svm
  #Test error
  test.err.svm <- sum(predict(svm.test, testing) != testing$Label)/nrow(testing)
  test.err.svm
  
  
 
  
  ###### xgBoost ######
  xgbfit <- train(Label~., data = training,
                  method = "xgbLinear", trControl = control
  )
  
  xgb.tuned<-xgboost(data=as.matrix(training[,-ncol(training)]),
                         label=as.matrix(training[,ncol(training)]),
                         missing = NA, weight = NULL,
                         nrounds=xgbfit$bestTune$nrounds,
                         lambda=xgbfit$bestTune$lambda,
                         alpha=xgbfit$bestTune$alpha,
                         eta=xgbfit$bestTune$eta)
  xgb.test<-xgboost(data=as.matrix(testing[,-ncol(testing)]),
                    label=as.matrix(testing[,ncol(testing)]),
                    missing = NA, weight = NULL,
                    nrounds=xgbfit$bestTune$nrounds,
                    lambda=xgbfit$bestTune$lambda,
                    alpha=xgbfit$bestTune$alpha,
                    eta=xgbfit$bestTune$eta)
  
  #Train error
  train.err.xgboost <- sum(predict(xgb.tuned, as.matrix(training)) != training$Label)/nrow(training)
  train.err.xgboost
  #Test error
  test.err.xgboost <- sum(predict(xgb.test, testing) != testing$Label)/nrow(testing)
  test.err.xgboost
  
  
  ###### DeepBoost ######
  dbfit <- train(Label~., data = training,
                  method = "deepboost", trControl = control, verbose = FALSE)
  
  db.tuned<-deepboost(Label~., data=training, num_iter=dbfit$bestTune$num_iter, 
                                              tree_depth=dbfit$bestTune$tree_depth,
                                              beta=dbfit$bestTune$beta,
                                              lambda=dbfit$bestTune$lambda,
                                              loss_type = "l")
  db.test<-deepboost(Label~., data=testing, num_iter=dbfit$bestTune$num_iter, 
                                            tree_depth=dbfit$bestTune$tree_depth,
                                            beta=dbfit$bestTune$beta,
                                            lambda=dbfit$bestTune$lambda,
                                            loss_type = "l")
  
  #Train error
  train.err.db <- sum(predict(db.tuned, training) != training$Label)/nrow(training)
  train.err.db
  #Test error
  test.err.db <- sum(predict(db.test, testing) != testing$Label)/nrow(testing)
  test.err.db
 
  
  #Summarize training and test error for all models
  training.error<-list(train.err.gbm,train.err.rf,train.err.svm.l, train.err.svm,
                   train.err.xgboost,train.err.db) 
  test.error<-list(test.err.gbm,test.err.rf,test.err.svm.l, test.err.svm,
                       test.err.xgboost,test.err.db)
  training.err.plot<-plot(unlist(training.error))
  test.err.plot<-plot(unlist(test.error))
 return()
  
}

save(xgbfit, file="../output/xgbfit_sift5000.RData")
save(svmfit, file="../output/svmfit_sift5000.RData")
save(svmfit.linear, file="../output/svmfitlinear_sift5000.RData")
save(rffit, file="../output/rffit_sift5000.RData")
save(gbmfit, file="../output/gbmfit_sift5000.RData")

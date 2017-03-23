<<<<<<< Updated upstream
library("caret")
library("gbm")
library("randomForest")
library("plyr")
library("xgboost")
library("fastAdaboost")
library("deepboost")

load("../output/HOG.RData")

X <- read.csv("../data/sift_features.csv")
X <- t(X)

labels <- read.csv("../data/labels.csv")
labels <- labels[,1]
#dat_train<-X
dat_train<-H
#dat_train<-cbind(P,H)
label_train <- labels


#Train <- function(dat_train, label_train){
  
  n <- nrow(dat_train)
  
  r <- sample(1:n, n)
  
  dat_train <- dat_train[r,]
  
  label_train <- label_train[r]
  
  data.all <- as.data.frame(cbind(dat_train, label_train))
  
  colnames(data.all)[ncol(data.all)] <- "Label"
  
  data.all$Label <- as.factor(data.all$Label)
  
  control <- trainControl(method = 'cv', number = 5)  #use 5-fold cross validation
  
  inTrain <- createDataPartition(y = data.all$Label, p=0.75, list=FALSE)
  
  training <- data.all[inTrain, ]
  
  testing <- data.all[-inTrain, ]
  
  ### GBM
  gbmGrid <- expand.grid(interaction.depth = (1:5) * 2,n.trees = (1:10)*25,shrinkage = .1,
                         n.minobsinnode = 10)
  
  gbmfit <- train(Label~., data = training,
                  method = "gbm", trControl = control, verbose = FALSE,
                  bag.fraction = 0.5, tuneGrid = gbmGrid
  )
  
  err.gbm <- sum(predict(gbmfit, testing) != testing$Label)/nrow(testing)
  
  ### Random Forest
  rfGrid <- expand.grid(mtry = 2^(5:10) )
  
  rffit <- train(Label~., data = training,
                 method = "rf", trControl = control, tuneGrid = rfGrid
  ) 
  
  err.rf <- sum(predict(rffit, testing) != testing$Label)/nrow(testing)
  
  
  ### SVM Linear
  svmGrid.linear <- expand.grid(C= 2^c(0:5))
  svmfit.linear <- train(Label~., data = training,
                         method = "svmLinear", trControl = control, tuneGrid = svmGrid.linear, preProc = c("center","scale")
  )
  
  err.svm.linear <- sum(predict(svmfit.linear, testing) != testing$Label)/nrow(testing)
  
  ### SVM RBF
  svmGrid <- expand.grid(sigma= 2^c(-25, -20, -15,-10, -5, 0), C= 2^c(0:5))
  svmfit <- train(Label~., data = training,
                  method = "svmRadial", trControl = control, tuneGrid = svmGrid, preProc = c("center","scale")
  )
  
  err.svm <- sum(predict(svmfit, testing) != testing$Label)/nrow(testing)
  
  ### xgBoost
  xgbfit <- train(Label~., data = training,
                  method = "xgbLinear", trControl = control
  )
  
  err.xgb <- sum(predict(xgbfit, testing) != testing$Label)/nrow(testing)
  
  ### DeepBoost
  dbfit <- train(Label~., data = training,
                  method = "deepboost", trControl = control, verbose = FALSE
  )
  err.db <- sum(predict(dbfit, testing) != testing$Label)/nrow(testing)
  
#}

### save(gbmfit, file="../output/gbmfit_sift5000.RData")
=======
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

###### Overall, our best model is feature HOG + classifier DeepBoost.######

###########################################################################

### Author: Project 3, Group 7
### ADS Spring 2017

#load requied packages

library("caret")
library("gbm")
library("randomForest")
library("plyr")
library("xgboost")
library("fastAdaboost")
library("e1071")
library("deepboost")

setwd("your local path")
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


Train <- function(dat_train, label_train){
  
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
  gbmGrid <- expand.grid(interaction.depth = (1:5) * 2,n.trees = (1:10)*25,shrinkage = .1,
                         n.minobsinnode = 10)
  
  gbmfit <- train(Label~., data = training,
                  method = "gbm", trControl = control, verbose = FALSE,
                  bag.fraction = 0.5, tuneGrid = gbmGrid) #parameter tuning
  
 gbm.tuned<-gbm(Label~., data=training, interaction.depth = gbmfit$bestTune$interaction.depth,
                n.trees = gbmfit$bestTune$n.trees, shrinkage = gbmfit$bestTune$shrinkage,
                n.minobsinnode = gbmfit$bestTune$n.minobsinnode,distribution = "bernoulli" )
  
  
  gbm.test<-gbm(Label~., data=testing, interaction.depth = gbmfit$bestTune$interaction.depth,
               n.trees = gbmfit$bestTune$n.trees,shrinkage = gbmfit$bestTune$shrinkage,
               n.minobsinnode = gbmfit$bestTune$n.minobsinnode, distribution = "bernoulli")
  
  #Train error
  train.err.gbm <- sum(predict(gbm.tuned, training,n.trees=gbmfit$bestTune$n.trees) != training$Label)/nrow(training)
  train.err.gbm
  
  #Test error
  test.err.gbm <- sum(predict(gbm.test, testing,n.trees=gbmfit$bestTune$n.trees) != testing$Label)/nrow(testing)
  test.err.gbm
  
  
  ###### Random Forest ######
  rfGrid <- expand.grid(mtry = 2^(5:10) )
  
  rffit <- train(Label~., data = training,
                 method = "rf", trControl = control, tuneGrid = rfGrid) #parameter tuning
  
  
  rf.tuned<-randomForest(Label~., data=training, mtry=rffit$bestTune$mtry)
  rf.test<-randomForest(Label~., data=testing,mtry=rffit$bestTune$mtry)
  
  #Train error
  train.err.rf <- sum(rf.tuned$predicted != training$Label)/nrow(training)
  train.err.rf

  #Test error
  test.err.rf <- sum(rf.test$predicted != testing$Label)/nrow(testing)
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
                  method = "xgbLinear", trControl = control)
  
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
  xgb.tra.pre<-predict(xgb.tuned, newdata=as.matrix(training[,-ncol(training)]),label=as.matrix(training[,ncol(training)]))
  xgb.tra.class<-ifelse(xgb.tra.pre>0.5,1,0)
  train.err.xgboost <- sum(xgb.class != training$Label)/nrow(training)
  train.err.xgboost
  #Test error
  xgb.test.pre<-predict(xgb.test, newdata=as.matrix(testing[,-ncol(training)]),label=as.matrix(testing[,ncol(testing)]))
  xgb.test.class<-ifelse(xgb.test.pre>0.5,1,0)
  test.err.xgboost <- sum(xgb.test.class != testing$Label)/nrow(testing)
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
 
  
  #Summarize training and test errors for all models
  training.error.c<-c(train.err.gbm,train.err.rf,train.err.svm.l, train.err.svm,
                   train.err.xgboost,train.err.db) 
  names(training.error.c)<-c("GBM","Random Forest","SVM with Linear Kernel",
                           "SVM with RBF","xgBoost","DeepBoost")
  training.error<-list(training.error.c)
  names(training.error)<-"Training Errors"

  
  
  test.error.c<-c(test.err.gbm,test.err.rf,test.err.svm.l, test.err.svm,
                       test.err.xgboost,test.err.db)
  names(test.error.c)<-c("GBM","Random Forest","SVM with Linear Kernel",
                           "SVM with RBF","xgBoost","DeepBoost")
  test.error<-list(test.error.c)
  names(test.error)<-"Test Errors"
  
  
  #training.err.plot<-plot(unlist(training.error))
  #test.err.plot<-plot(unlist(test.error))

   return(c(training.error,test.error))
  
}

save(xgbfit, file="../output/xgbfit_sift5000.RData")
save(svmfit, file="../output/svmfit_sift5000.RData")
save(svmfit.linear, file="../output/svmfitlinear_sift5000.RData")
save(rffit, file="../output/rffit_sift5000.RData")
save(gbmfit, file="../output/gbmfit_sift5000.RData")
save(dbfit, file="../output/gbmfit_sift5000.RData")
>>>>>>> Stashed changes

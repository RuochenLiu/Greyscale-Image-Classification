library("caret")
library("gbm")
library("randomForest")
library("plyr")
library("xgboost")
library("fastAdaboost")
<<<<<<< HEAD
library("e1071")
=======
library("deepboost")
>>>>>>> origin/master

X <- read.csv("../data/sift_features.csv")
X <- t(X)

labels <- read.csv("../data/labels.csv")
labels <- labels[,1]
#dat_train<-X
<<<<<<< HEAD
dat_train<-dat
#dat_train<-cbind(X,dat)
=======
dat_train<-H
#dat_train<-cbind(P,H)
>>>>>>> origin/master
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
  
<<<<<<< HEAD
  ##### GBM ######
=======
  ### GBM
>>>>>>> origin/master
  gbmGrid <- expand.grid(interaction.depth = (1:5) * 2,n.trees = (1:10)*25,shrinkage = .1,
                         n.minobsinnode = 10)
  
  gbmfit <- train(Label~., data = training,
                  method = "gbm", trControl = control, verbose = FALSE,
                  bag.fraction = 0.5, tuneGrid = gbmGrid
  )
  
  gbm.tuned<-gbm(Label~., data=training, interaction.depth = gbmfit$bestTune$interaction.depth,
                n.trees = gbmfit$bestTune$n.trees, shrinkage = gbmfit$bestTune$shrinkage,
                n.minobsinnode = gbmfit$bestTune$n.minobsinnode,distribution = "bernoulli" )
  
<<<<<<< HEAD
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
=======
  ### Random Forest
  rfGrid <- expand.grid(mtry = 2^(5:10) )
  
  rffit <- train(Label~., data = training,
                 method = "rf", trControl = control, tuneGrid = rfGrid
  ) 
>>>>>>> origin/master
  
  #Test error
  test.err.rf <- sum(predict(rf.test, testing) != testing$Label)/nrow(testing)
  test.err.rf
  
  
<<<<<<< HEAD
  ###### SVM Linear Kernel ######
=======
  ### SVM Linear
>>>>>>> origin/master
  svmGrid.linear <- expand.grid(C= 2^c(0:5))
  svmfit.linear <- train(Label~., data = training,
                         method = "svmLinear", trControl = control, tuneGrid = svmGrid.linear, preProc = c("center","scale")
  )
  
  svm.linear.tuned<-svm(Label~., data=training, kernel="linear",cost=svmfit.linear$bestTune$C)
  svm.linear.test<-svm(Label~., data=testing, kernel="linear",cost=svmfit.linear$bestTune$C)
  
<<<<<<< HEAD
  #Train error
  train.err.svm.l <- sum(predict(svm.linear.tuned, training) != training$Label)/nrow(training)
  train.err.svm.l
  
  #Test error
  test.err.svm.l <- sum(predict(svm.linear.test, testing) != testing$Label)/nrow(testing)
  test.err.svm.l
  
  
  
  ###### SVM RBF Kernel ######
=======
  ### SVM RBF
>>>>>>> origin/master
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

save(xgbfit, file="../output/xgbfit_sift5000.RData")
save(svmfit, file="../output/svmfit_sift5000.RData")
save(svmfit.linear, file="../output/svmfitlinear_sift5000.RData")
save(rffit, file="../output/rffit_sift5000.RData")
save(gbmfit, file="../output/gbmfit_sift5000.RData")

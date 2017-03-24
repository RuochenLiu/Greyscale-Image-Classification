###########################################################################
####### This file (Train.R) is consistent of six training models.   #######
####### They are GBM, SVM with Linear Kernel, SVM with RBF Kernel,  #######
####### Random Forest, xgboost, and DeepBoost.                      #######

###########################################################################

###### Given the feature extracted from HOG in the "feature.R" file, ######
###### the BEST classifier is .                                      ######
###### The training error is about ,                                 ######
###### and the test error is about .                                 ######

###########################################################################

###### Overall, our best model is feature HOG + classifier DeepBoost.######

###########################################################################

### Author: Project 3, Group 7
### ADS Spring 2017

Train <- function(dat_train, label_train){
  
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
  

  ##### GBM ######
  gbmGrid <- expand.grid(interaction.depth = (1:5) * 2,n.trees = (1:10)*25,shrinkage = .1,
                         n.minobsinnode = 10)
  
  gbmfit <- train(Label~., data = training,
                  method = "gbm", trControl = control, verbose = FALSE,
                  bag.fraction = 0.5, tuneGrid = gbmGrid) #parameter tuning
  
  train.gbm <- sum(predict(gbmfit, training) != training$Label)/nrow(training)
  
  test.gbm <- sum(predict(gbmfit, testing) != testing$Label)/nrow(testing)
  
  ###### Random Forest ######
  rfGrid <- expand.grid(mtry = 2^(1:5) )
  
  rffit <- train(Label~., data = training,
                 method = "rf", trControl = control, tuneGrid = rfGrid) #parameter tuning
  
  
  train.rf <- sum(predict(rffit, training) != training$Label)/nrow(training)
  
  test.rf <- sum(predict(rffit, testing) != testing$Label)/nrow(testing)
  

  ###### SVM with Linear Kernel ######

  svmGrid.linear <- expand.grid(C= 2^c(0:5))
  
  svm.linear <- train(Label~., data = training,
                         method = "svmLinear", trControl = control, tuneGrid = svmGrid.linear, preProc = c("center","scale")
  )
  
  train.svmlinear <- sum(predict(svm.linear, training) != training$Label)/nrow(training)
  
  test.svmlinear <- sum(predict(svm.linear, testing) != testing$Label)/nrow(testing)
  
  
  
  ###### SVM with RBF Kernel ######

  svmGrid <- expand.grid(sigma= 2^c(-25, -20, -15,-10, -5, 0), C= 2^c(0:5))
  
  svmfit <- train(Label~., data = training,
                  method = "svmRadial", trControl = control, tuneGrid = svmGrid, preProc = c("center","scale")
  )
  
  train.svm <- sum(predict(svmfit, training) != training$Label)/nrow(training)
  
  test.svm <- sum(predict(svmfit, testing) != testing$Label)/nrow(testing)
  
  
 
  
  ###### xgBoost ######
  xgbfit <- train(Label~., data = training,
                  method = "xgbLinear", trControl = control)
  
  train.xgb <- sum(predict(xgbfit, training) != training$Label)/nrow(training)
  
  test.xgb <- sum(predict(xgbfit, testing) != testing$Label)/nrow(testing)
  
  
  
  
  ###### DeepBoost ######
  dbfit <- train(Label~., data = training,
                  method = "deepboost", trControl = control, verbose = FALSE)
  
  train.db <- sum(predict(dbfit, training) != training$Label)/nrow(training)
  
  test.db <- sum(predict(dbfit, testing) != testing$Label)/nrow(testing)
 
  
  
  
  #Summarize training and test errors for all models

  #training.err.plot<-plot(unlist(training.error))
  #test.err.plot<-plot(unlist(test.error))
  
  models <- list(GBM = gbmfit, RF = rffit, SVML = svm.linear, SVM = svmfit, XGB = xgbfit, DB = dbfit)
  
  train.error <- c(train.gbm, train.rf, train.svmlinear, train.svm, train.xgb, train.db)
  
  test.error <- c(test.gbm, test.rf, test.svmlinear, test.svm, test.xgb, test.db)
  
  errors <- data.frame(Train = train.error,Test = test.error)
  
  rownames(errors) <- c("GBM","Random Forest","Linear SVM", "SVM with RBF","xgBoost","DeepBoost")

  return(list(models = models, errors = errors))
  
}

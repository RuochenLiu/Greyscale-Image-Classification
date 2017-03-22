library("caret")
library("gbm")
library("randomForest")
library("plyr")
library("xgboost")
library("fastAdaboost")

X <- read.csv("/Users/xuehan/Desktop/spr2017-proj3-group7/data/sift_features.csv")
X <- t(X)
labels <- read.csv("/Users/xuehan/Desktop/spr2017-proj3-group7/data/labels.csv")
labels <- labels[,1]
dat_train<-X
label_train <- labels


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
  
  # GBM
  gbmGrid <- expand.grid(interaction.depth = (1:5) * 2,n.trees = (1:10)*25,shrinkage = .1,
                         n.minobsinnode = 10)
  
  gbmfit <- train(Label~., data = training,
                  method = "gbm", trControl = control, verbose = FALSE,
                  bag.fraction = 0.5, tuneGrid = gbmGrid
                  ) #parameter tuning
  
  err.gbm <- sum(predict(gbmfit, testing) != testing$Label)/nrow(testing)
  
  # Random Forest
  rfGrid <- expand.grid(mtry = 2^(5:10) )
  
  rffit <- train(Label~., data = training,
                 method = "rf", trControl = control, tuneGrid = rfGrid
                  ) #parameter tuning
  
  err.rf <- sum(predict(rffit, testing) != testing$Label)/nrow(testing)

 
  # SVM Linear Kernel
  svmGrid <- expand.grid(C= 2^c(0:5))
  svmfit <- train(Label~., data = training,
                  method = "svmLinear", trControl = control, tuneGrid = svmGrid, preProc = c("center","scale")
  ) #parameter tuning
  
  err.svm <- sum(predict(svmfit, testing) != testing$Label)/nrow(testing)
  
  # SVM RBF
  svmGrid <- expand.grid(sigma= 2^c(-25, -20, -15,-10, -5, 0), C= 2^c(0:5))
  svmfit <- train(Label~., data = training,
                 method = "svmRadial", trControl = control, tuneGrid = svmGrid, preProc = c("center","scale")
                  ) #parameter tuning
  
  err.svm <- sum(predict(svmfit, testing) != testing$Label)/nrow(testing)
  
  # xgBoost
  xgbfit <- train(Label~., data = training[1:100,],
                 method = "xgbLinear", trControl = control
                 ) #parameter tuning
  
  err.xgb <- sum(predict(xgbfit, testing) != testing$Label)/nrow(testing)
  
}

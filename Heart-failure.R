library(tidyverse)
library(corrplot)

data <- read_csv('../input/heart-failure-clinical-data/heart_failure_clinical_records_dataset.csv')
data%>%head()

summary(data)

#Data Visualize
ggplot(data)+
  geom_bar(aes(DEATH_EVENT, fill = DEATH_EVENT), position = 'identity')+
  scale_fill_brewer()+
  theme_dark()+
  labs(title='Count of Death event')
dt <- cor(data)
corrplot(dt, method = 'color')
data$DEATH_EVENT <- as.factor(data$DEATH_EVENT)
ggplot(data)+
  geom_density(aes(age, fill = DEATH_EVENT), alpha = 0.4)+
  scale_fill_grey()

# The mean value of each columns separate by death event
death_data <- filter(data, DEATH_EVENT == 1)
notdeath_data <- filter(data, DEATH_EVENT == 0)
mean_death_data <- list()
mean_notdeath_data <- list()
clst <- list()
for(i in 2: ncol(data) - 1){
  a <- sum(death_data[[i]]) / nrow(death_data[i])
  mean_death_data <- append(mean_death_data, a)
  b <- sum(notdeath_data[[i]]) / nrow(notdeath_data[i])
  mean_notdeath_data <- append(mean_notdeath_data, b)
  clst <- append(clst, names(data[i]))
}
mean_data <- as.matrix(rbind(clst, mean_death_data, mean_notdeath_data))
mean_data

#death event vs each column plot
lst = list(age = data$age, ejection_fraction=data$ejection_fraction,
         serum_creatinine = data$serum_creatinine, serum_sodium = data$serum_sodium, time = data$time)
lstcolor = list('red','blue','purple','lightblue','lightgreen')
n = 1
for(i in lst){
  print(ggplot(data)+
          geom_boxplot(aes(DEATH_EVENT, i), fill = lstcolor[n], alpha = 0.5)+
          ylab(label = names(lst)[n]))
    n = n + 1
  # Sys.sleep(2) how long to wait for next plot
}

# 6 Prediction Model
set.seed(1)
train <- sample(1:nrow(data),210)
train_data <- data[train,]
test_data <- data[-train,]
# (1)support vector machine
library(kernlab)
library(pROC)
svm_model <- ksvm(DEATH_EVENT~., data = train_data,kernel='vanilladot') # linear kernel
svm_model
svm_pred <- predict(svm_model, test_data)
table(svm_pred, test_data$DEATH_EVENT)
agree <- svm_pred == test_data$DEATH_EVENT
svm_acc <- prop.table(table(agree))#accuracy
svm_acc
#ROC curve
svm_roc <- roc(test_data$DEATH_EVENT,as.numeric(svm_pred))
plot(svm_roc, print.auc = TRUE, auc.polygon = TRUE, grid = c(0.1, 0.2), auc.polygon.col = 'skyblue', max.auc.polygon = TRUE, print.thres = TRUE, main =' ROC curve with SVM model')

# (2)decision tree 
# Plot the tree and X-val error to see if we need to prune the tree
library(rpart)
library(rpart.plot)
tree_model <- rpart(DEATH_EVENT~., data = train_data)
rpart.plot(tree_model)
tree_pred <- predict(tree_model, test_data, type = 'class')
plotcp(tree_model)
dt_acc <- sum(diag(table(test_data$DEATH_EVENT, tree_pred))) / sum(table(test_data$DEATH_EVENT, tree_pred))
dt_acc

# (3)K nearest neighbor
# For not letting a column take too much weight , scale the data.
# Set k as root of training data row.
library(class)
library(gmodels)
train_data_label <- as.factor(train_data[,13]$DEATH_EVENT)
train_data_knn <- scale(train_data[1:12])
test_data_label <- as.factor(test_data[,13]$DEATH_EVENT)
test_data_knn <- scale(test_data[1:12])
knn_pred <- knn(train = train_data_knn, test = test_data_knn, cl = train_data_label, k = 13)
CrossTable(x = test_data_label, y = knn_pred,prop.chisq = FALSE)
knn_acc <- sum(test_data_label == knn_pred) / length(test_data_label)
knn_acc

# (4)neural network
# For binary classificationl , use sigmoid as output activation function and binary_crossentropy as loss
library(keras)
nn_train_data <- as.matrix(scale(data[train,][1:12]))
nn_test_data <- as.matrix(scale(data[-train,][1:12]))
nn_train_data_label <- to_categorical(data[train,][13])
nn_test_data_label <- to_categorical(data[-train,][13])
nnmodel <- keras_model_sequential()%>%
  layer_dense(32, activation = 'relu', input_shape = c(12))%>%
  layer_dense(16, activation = 'relu')%>%
  layer_dense(16, activation = 'relu')%>%
  layer_dense(2, activation = 'sigmoid')
nnmodel%>%compile(
  optimizer = 'rmsprop',
  loss = 'binary_crossentropy',
  metrics = c('accuracy')
)
nnmodel%>%fit(nn_train_data,nn_train_data_label,epochs = 20, batch_size = 10)
nn_acc<-nnmodel%>%evaluate(nn_test_data, nn_test_data_label)
nn_acc[[2]]

# (5)random forest
library(randomForest)
library(caret)
rf_model <- randomForest(DEATH_EVENT ~., data = train_data)
rf_pred <- predict(rf_model, train_data)
confusionMatrix(rf_pred, train_data$DEATH_EVENT)
rf_test <- predict(rf_model, test_data)
rf_cM <- confusionMatrix(rf_test, test_data$DEATH_EVENT)
rf_acc <- sum(diag(rf_cM$table)) / sum(rf_cM$table)
rf_acc
#Look at the importance of each columns and visualize Error of the trees
oob.err.data <- data.frame(
  Trees = rep(1:nrow(rf_model$err.rate), 3), 
  Type = rep(c("OOB","Not Death","Death"), each = nrow(rf_model$err.rate)),
  Error = c(rf_model$err.rate[,"OOB"], rf_model$err.rate[,"0"], rf_model$err.rate[,"1"]))
round(importance(rf_model), 2)
ggplot(data = oob.err.data, aes(x = Trees, y= Error)) + geom_line(aes(color = Type))

# (6)XGBoost
#Set the params , kfold ,rounds ... 
#Choose the best rounds
#Visualize the performance of data
library(xgboost)
train_data$DEATH_EVENT <- ifelse(as.numeric(train_data$DEATH_EVENT) == 2, 1, 0)
test_data$DEATH_EVENT <- ifelse(as.numeric(test_data$DEATH_EVENT) == 2, 1, 0)
dtrain <- xgb.DMatrix(data = as.matrix(train_data[1: 12]), label = train_data$DEATH_EVENT)
dtest <- xgb.DMatrix(data = as.matrix(test_data[1: 12]), label = test_data$DEATH_EVENT)
xgb.params <- list(colsample_bytree = 0.5, #col sampling 
                 subsample = 0.5, #row sampling 
                 booster = 'gbtree',
                 max_depth = 2, # max depth of tree
                 eta = 0.03, # learning rate
                 eval_metrics = 'logloss',
                 objective = 'binary:logistic', # for binary classification
                 gamma = 0
                )
cv.model <- xgb.cv(params = xgb.params,
                 data = dtrain,
                 nfold = 5,
                 nrounds = 200,
                 early_stopping_rounds = 30, # stop if there's overfitting before 30 rounds
                 print_every_n = 20
                )
tmp = cv.model$evaluation_log
ggplot(tmp, aes(x=1:nrow(tmp)))+
  geom_point(aes(y = tmp$train_logloss_mean,color = 'train'))+
  geom_point(aes(y = tmp$test_logloss_mean,color = 'test'))+
  labs(title = 'Avg.Performance in CV', x = "nround", y = "logloss")
best.nrounds = cv.model$best_iteration
xgb.model <- xgb.train(data = dtrain, params = xgb.params,nrounds = best.nrounds)
# xgb.plot.tree(model = xgb.model) this is for visualize the model , but the model is too long 
xgb_pred <- predict(xgb.model, dtest)
xgb_pred <- ifelse(xgb_pred > 0.5, 1, 0)
xgb_cM <- confusionMatrix(as.factor(xgb_pred), as.factor(test_data$DEATH_EVENT))
xgb_acc <- sum(diag(xgb_cM$table)) / sum(xgb_cM$table)
xgb_acc

#Visualize accuracy of all models
model_data <- data.frame(
  accuracy = c(svm_acc[[2]], dt_acc, knn_acc, nn_acc[[2]], rf_acc,xgb_acc),
           model = c('SVM', 'DT', 'Knn', 'NN', 'RF', 'Xgb'))

model_data$accuracy <- (100 * round(model_data$accuracy, 3))
model_data%>%ggplot()+
  geom_bar(aes(model, accuracy, fill = accuracy), stat = 'identity')+
  geom_text(aes(model, accuracy, label = accuracy), position = position_dodge(0.9), vjust = 0)

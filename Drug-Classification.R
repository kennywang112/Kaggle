library(keras)
library(tidyverse)

data<-read_csv('../input/drug-classification/drug200.csv')
summary(data)

data%>%head(5)
table(data['Sex'])
table(data['BP'])
table(data['Cholesterol'])
table(data['Drug'])

# Data Visualize
# 1.Witch drugs in every ages take
# 2.Check the Blood Pressure Levels in each drugs
data%>%ggplot()+geom_bar(aes(Age,fill = Drug), position = 'identity')+labs(title = 'Drugs in each ages')
data%>%group_by(Drug,BP)%>%summarize(count = n())%>%ggplot()+ 
  geom_col(aes(Drug,BP, fill = BP), position = 'dodge')+
  labs(title = 'BP of each Drugs')

# In each Drugs, the Sodium to potassium Ration in Blood in every Ages shows that DrugY is the highest
ggplot(data=data,aes(Age,Na_to_K))+stat_smooth(method=loess,aes(color=Drug))+labs(title='Sodium to potassium Ration in Blood in each Drugs at every Ages')

# Blood Pressure Levels and Cholesterol in ages 15~30 , 31~45 , 46~60 , 61~74
new_data<-data
summary(data$Age)
new_data$Age <- case_when(
  data$Age > 60&data$Age <= 74 ~ '60~74',
  data$Age > 45&data$Age <= 60 ~ '46~60',
  data$Age > 30&data$Age <= 45 ~ '31~45',
  data$Age >= 15&data$Age <= 30 ~'15~30'
)
new_data%>%group_by(Age, BP)%>%summarize(count = n())%>%ggplot()+
  geom_col(aes(Age, count, fill = BP), position = 'dodge')+
  labs(title = 'Blood pressure in each age')
new_data%>%group_by(Age, Cholesterol)%>%summarize(count = n())%>%ggplot()+
  geom_col(aes(Age, count, fill = Cholesterol),position='dodge')+
  labs(title = 'Cholesterol in each age')

# Plot the Sodium to potassium Ration in Blood with each Sex and BP
data%>%ggplot()+
  geom_boxplot(aes(Sex,Na_to_K,fill = Sex), position = 'identity')+
  labs(title = 'Sodium to potassium Ration in Blood at each Sex')
data%>%ggplot()+
  geom_boxplot(aes(BP, Na_to_K, fill = BP), position = 'identity')+
  labs(title = 'Sodium to potassium Ration in Blood at each BP')

# Decision Tree Model
library(rpart)
library(rpart.plot)
# Use 'Drug~.' for all columns and plot the tree and Xerror, see if we need to prune the tree
train_tree <- data[1:140,]
test_tree <- data[141:200,]
m <- rpart(Drug~.,data=train_tree)
rpart.plot(m)
p <- predict(m, test_tree,type = 'class')
plotcp(m)
sum(diag(table(test_tree$Drug, p)))/sum(table(test_tree$Drug, p)) # Accuracy

# Neutral Network Model
# Change all the values for computing tensor
# Change columns to all numeric and scale
# To classification code, change the labels data with to_categorical

data_label <- data[6]
data_d <- data[-6]
for(i in 1: length(data[[1]])){
  if(data_label$Drug[i] == 'drugA') {data_label$Drug[i] <- '1'}
  else if(data_label$Drug[i] == 'drugB') {data_label$Drug[i] <- '2'}
  else if(data_label$Drug[i] == 'drugC') {data_label$Drug[i] <- '3'}
  else if(data_label$Drug[i] == 'drugX') {data_label$Drug[i] <- '4'}
  else data_label$Drug[i] <- '5'
  if(data_d$Sex[i] == 'F') {data_d$Sex[i] <- '0'}
  else data_d$Sex[i] <- '1'
  if(data_d$BP[i] == 'HIGH') {data_d$BP[i] <- '0'}
  else if(data_d$BP[i] == 'NORMAL') {data_d$BP[i] <- '1'}
  else data_d$BP[i] <- '2'
  if(data_d$Cholesterol[i] == 'HIGH') {data_d$Cholesterol[i] <- '0'}
  else data_d$Cholesterol[i] <- '1'
}

# Process data for tensor calculate
data_d$Sex <- as.numeric(data_d$Sex)
data_d$BP <- as.numeric(data_d$BP)
data_d$Cholesterol <- as.numeric(data_d$Cholesterol)
data_zscore <- as.matrix(scale(data_d))
train_data <- data_zscore[1: round(length(data_d[[1]]) * 0.7),]
test_data <- data_zscore[round(length(data_d[[1]]) * 0.7 + 1): length(data[[1]]),]
train_label <- to_categorical(data_label[1: round(length(data[[1]]) * 0.7),])
test_label <- to_categorical(data_label[round(length(data[[1]]) * 0.7 + 1): length(data[[1]]),])

# We have six different Drugs ,the output unit will set as 6 and softmax as activation function 
# For multiple probability distribution , use categorical_crossentropy as loss

# Set the model , for six classifications (drugA~DrugY) use softmax and in the compile , use categorical_crossentropy as loss 

model <- keras_model_sequential()%>%
  layer_dense(16, activation = 'relu', input_shape = c(5))%>%
  layer_dense(16, activation = 'relu')%>%
  layer_dense(16, activation = 'relu')%>%
  layer_dense(6, activation = 'softmax')
model%>%compile(
  optimizer = 'rmsprop',
  loss = 'categorical_crossentropy',
  metrics = c('accuracy')
)
history<-model%>%fit(
  train_data, train_label, epochs = 50, batch_size = 10
)
plot(history)

model%>%evaluate(test_data, test_label)# neural network model accuracy

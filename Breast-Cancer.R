# https://www.kaggle.com/datasets/fatemehmehrparvar/breast-cancer-prediction
library(keras)
library(tidyverse)
library(reticulate)
# install_tensorflow()

data <- read_csv('../Kaggle_Data/breast-cancer-dataset.csv')%>%select(-`S/N`)
data%>%colnames()
data%>%head()
data%>%dim()
data%>%summary()
data%>%glimpse()
# All columns unique
for (i in 1: ncol(data)) {
  print(data[,i]%>%unique())
}
# Data visualization
data%>%
  group_by(`Diagnosis Result`)%>%
  summarize(count = n())%>%
  ggplot()+geom_bar(aes(`Diagnosis Result`, count, fill = `Diagnosis Result`), stat = 'identity')+
  labs(title = 'Diagnosis Result')
data%>%
  filter(Year != "#")%>%
  ggplot()+geom_bar(aes(Year, fill = `Diagnosis Result`), position = 'dodge')+
  labs(title = 'Diagnosis Result in each Year')
data%>%
  filter(Breast != "#")%>%
  ggplot()+geom_bar(aes(Breast, fill = `Diagnosis Result`), position = 'dodge')+
  labs(title = 'Diagnosis Result in each Breast')
data%>%
  filter(`Breast Quadrant` != "#")%>%
  ggplot()+geom_bar(aes(`Breast Quadrant`, fill = `Diagnosis Result`), position = 'dodge')+
  labs(title = 'Diagnosis Result in each Breast Quadrant')

data%>%
  ggplot()+geom_density(aes(Age, fill = `Diagnosis Result`), alpha = 0.4)+
  labs(title = 'Diagnosis Result in each Age')
data%>%
  ggplot()+geom_bar(aes(Menopause, fill = `Diagnosis Result`), position = 'dodge')+
  labs(title = 'Diagnosis Result in each Menopause')
data%>%
  filter(`Inv-Nodes` != '#')%>%
  ggplot()+geom_bar(aes(`Inv-Nodes`, fill = `Diagnosis Result`), position = 'dodge')+
  labs(title = 'Diagnosis Result in each Inv-Nodes')
data%>%
  filter(Metastasis != '#')%>%
  ggplot()+geom_bar(aes(Metastasis, fill = `Diagnosis Result`), position = 'dodge')+
  labs(title = 'Diagnosis Result in each Metastasis')
data%>%
  filter(History != '#')%>%
  ggplot()+geom_bar(aes(History, fill = `Diagnosis Result`), position = 'dodge')+
  labs(title = 'Diagnosis Result in each History')

# Data Preprocessing
case_data <- data%>%
  mutate(
    Year = case_when(
    Year == "2019" ~ 0,
    Year == "2020" ~ 1,
    Year == "#" ~ 2
    ),
    Breast = case_when(
      Breast == "Left" ~ 0,
      Breast == "Right" ~ 1,
      Breast == "#" ~ 2
    ),
    `Breast Quadrant` = case_when(
      `Breast Quadrant` == "Lower inner" ~ 0,
      `Breast Quadrant` == "Lower outer" ~ 1,
      `Breast Quadrant` == "Upper inner" ~ 2,
      `Breast Quadrant` == "Upper outer" ~ 3,
      `Breast Quadrant` == "#" ~ 4
    ),
    `Diagnosis Result` = case_when(
      `Diagnosis Result` == "Malignant" ~ 0,
      `Diagnosis Result` == "Benign" ~ 1
    ),
    # History = case_when(
    #   History == "#" ~ "0",
    #   TRUE ~ History
    # ),
    # Metastasis = case_when(
    #   Metastasis == "#" ~ "0",
    #   TRUE ~ Metastasis
    # ),
    # `Inv-Nodes` = case_when(
    #   `Inv-Nodes` == "#" ~ "0",
    #   TRUE ~ `Inv-Nodes`
    # ),
    # `Tumor Size (cm)` = case_when(
    #   `Tumor Size (cm)` == "#" ~ "0",
    #   TRUE ~ `Tumor Size (cm)`
    # ),
  )
case_data <- case_data[!grepl("#", case_data$History), , drop = FALSE]

case_data%>%ggplot()+geom_boxplot(aes(as.character(`Diagnosis Result`), as.numeric(`Tumor Size (cm)`)))+
  labs(title = 'Tumor Size (cm) in each Diagnosis Result')
# 找出離群值
case_data%>%filter(as.numeric(`Tumor Size (cm)`) > 10)
case_data <- case_data%>%filter(as.numeric(`Tumor Size (cm)`) != 14)

# Data split
set.seed(123)
train_ratio <- 0.8
data_matrix <- as.matrix(case_data)
train_indices <- sample(1:nrow(data_matrix), size = round(train_ratio * nrow(data_matrix)))

train_data <- data_matrix[train_indices, ][,1:9]%>%apply(2, as.numeric)
test_data <- data_matrix[-train_indices, ][,1:9]%>%apply(2, as.numeric)

train_label <- to_categorical(data_matrix[train_indices, ][,10])
test_label <- to_categorical(data_matrix[-train_indices, ][,10])

# Neural Network Model
model <- keras_model_sequential()%>%
  layer_dense(8, activation = 'relu', input_shape = c(9))%>%
  layer_dense(8, activation = 'relu')%>%
  layer_dense(8, activation = 'relu')%>%
  layer_dense(2, activation = 'sigmoid')
model%>%compile(
  optimizer = 'rmsprop',
  loss = 'binary_crossentropy',
  metrics = c('accuracy')
)
summary(model)
# model to function
model_func <- function() {
  model <- keras_model_sequential()%>%
    layer_dense(8, activation = 'relu', input_shape = c(9))%>%
    layer_dense(8, activation = 'relu')%>%
    layer_dense(8, activation = 'relu')%>%
    layer_dense(2, activation = 'sigmoid')
  model%>%compile(
    optimizer = 'rmsprop',
    loss = 'binary_crossentropy',
    metrics = c('accuracy')
  )
  return(model)
}
model <- model_func()

history <- model%>%fit(
  train_data, train_label, epochs = 20, batch_size =  10
)
plot(history)
model%>%evaluate(test_data, test_label)

indices_list <- list()
for (i in colnames(data)) {
  
  indices <- grep("#", data[[i]])
  print(indices)

  if (length(indices) > 0) {
    indices_list <- c(indices_list, list(indices))
  }
}
all_unusual <- unlist(indices_list)%>%unique()
unusual_data <- data.frame()
for (i in all_unusual) {
  unusual_data <- rbind(unusual_data, data[i, ])
}
unusual_data

# Cross validation
k <- 5
indices <- sample(1:nrow(train_data))
folds <- cut(1:nrow(train_data), breaks = k, labels = FALSE)
num_epochs <- 20
all_scores <- c()
all_loss_histories <- NULL
for (i in 1:k) {
  cat('processing fold #', i, '\n')
  
  val_indices <- which(folds == i, arr.ind = TRUE)
  val_data <- train_data[val_indices, ]
  val_label <- train_label[val_indices]

  model <- model_func()
  history <- model%>%fit(
    train_data, train_label, epochs = num_epochs, batch_size =  10, verbose = 0
  )
  results <- model%>%evaluate(test_data, test_label, verbose = 0)
  print(history)
  all_scores <- c(all_scores, results[2])
  loss_history <- history$metrics$loss
  all_loss_histories <- rbind(all_loss_histories, loss_history)
}
all_scores%>%mean()

avg_loss_history <- data.frame(
  epoch = seq(1, length(all_loss_histories)),
  validation_loss = apply(all_loss_histories, 2, mean)
)
avg_loss_history





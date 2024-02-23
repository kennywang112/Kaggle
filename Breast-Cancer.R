# https://www.kaggle.com/datasets/fatemehmehrparvar/breast-cancer-prediction
library(keras)
library(tidyverse)

data <- read_csv('C:/Users/USER/Desktop/KaggleData/breast-cancer-dataset.csv')
data%>%head()
data%>%dim()
data%>%summary()
data%>%glimpse()
# All columns unique
for (i in 1:ncol(data)){
  print(data[,i]%>%unique())
}
data <- data%>%select(-`S/N`)
# neural network
model <- keras_model_sequential()%>%
  layer_dense(16,activation = 'relu',input_shape = c(7))%>%
  layer_dense(16,activation = 'relu')%>%
  layer_dense(2, activation = 'sigmoid')
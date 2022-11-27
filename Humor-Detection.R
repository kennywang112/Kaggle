library(keras)
library(tidyverse)
library(ggplot2)
library(wordcloud)
library(tm)

#Data Visualize
#Plot the proportion of either question and humor
data<-read_csv('../input/200k-short-texts-for-humor-detection/dataset.csv')
ques_data<-data%>%filter(str_detect(text,regex('\\?')))
notques_data<-data%>%filter(!str_detect(text,regex('\\?')))
m<-ques_data%>%group_by(humor)%>%summarize(count=n())%>%mutate(category='question')
k<-notques_data%>%group_by(humor)%>%summarize(count=n())%>%mutate(category='not question')
sm_category<-rbind(m,k)%>%mutate(percent=round(count/sum(count),2))
sm_category[1]<-ifelse(sm_category$humor=='TRUE','humor','not humor')
sm_category2<-unite(sm_category,class,humor,category,sep = ',')
pie(x=sm_category2$percent,
    labels = paste(sm_category2$class, sep = " ",
                   sm_category2$percent, "%"))
sm_category%>%group_by(category)%>%summarize(humor=humor,p=count/sum(count))

#Length of the sentences
data<-data%>%mutate(chrlength=' ')
data[[1]]<-gsub("[[:punct:]]",'',data[[1]])
m<-data[[1]]
data[[3]]<-as.integer(lengths(gregexpr("\\W+", m)) + 1)
ggplot(data)+geom_bar(aes(chrlength))+labs(title='Length of the sentences')

#Humor and Not Humor length of the sentences
ggplot(data)+geom_boxplot(aes(humor,chrlength))+labs(title='Humor and Not Humor length of the sentences')

#Sentence before and after cleaning using Text Mining package
data_humor<-data%>%filter(humor=='TRUE')
data_nothumor<-data%>%filter(humor=='FALSE')
data_corpus_true <- VCorpus(VectorSource(data_humor$text))
data_corpus_false <- VCorpus(VectorSource(data_nothumor$text))
clean_fum<-function(x){
  y<-tm_map(x, content_transformer(tolower))
  y<-tm_map(y, removeNumbers)
  y<-tm_map(y, removeWords, stopwords()) 
  y<-tm_map(y, removePunctuation) 
  y<-tm_map(y, stemDocument)
  y<-tm_map(y, stripWhitespace)
  return(y)
}
data_corpus_true_clean<-clean_fum(data_corpus_true)
data_corpus_false_clean<-clean_fum(data_corpus_false)
lapply(data_corpus_true[1:3], as.character)
lapply(data_corpus_true_clean[1:3], as.character)

#Word Cloud of humor/not humor
humor <- subset(data, humor == "TRUE")
nothumor<- subset(data, humor == "FALSE")
pal = brewer.pal(8,"Dark2")
wordcloud(humor$text, max.words = 40, scale = c(7, 1),colors = pal)
text(x=0.2, y=0.8, "Humor Words")
wordcloud(nothumor$text, max.words = 40, scale = c(4, 1),colors = pal)
text(x=0.2, y=0.9, "Not Humor Words")

#Process And Train Data
#There are some factor that affect accuracy below:
#1.The length of the joke for changing data into index
#2.Input and output dimension in embedding layer
#3.Some unimportant punctuation in the joke
#4.Add dropout layer to prevent overfitting
data<-file.path('../input/200k-short-texts-for-humor-detection/dataset.csv')
text<-read.csv(data)[[1]]
#procesing data
text<-gsub("([?])|[[:punct:]]", "\\1", text)
train_text<-text[1:round(length(text)*0.7)]
test_text<-text[round(length(text)*0.7+1):length(text)]
label<-read.csv(data)[2]
label[1]<-ifelse(label$humor=='True','1','0')
label<-as.numeric(label$humor)
train_label<-label[1:round(length(label)*0.7)]
test_label<-label[round(length(label)*0.7+1):length(label)]

#For the visualize above, we know that the max length of the sentences is around 18
#we can set the maxlen as 18 so that we can have a nice 2D tensor
maxlen<-18#consider the sentences that the length of word max 18
training_samples<-98000#set up the training samples
validation_samples<-42000#set up the validation samples
max_words<-10000#consider top 10000 words in the jokes
tokenizer<-text_tokenizer(num_words = max_words)%>%fit_text_tokenizer(train_text)#generate index
sequences<-texts_to_sequences(tokenizer,train_text)
data_seq<-pad_sequences(sequences,maxlen = maxlen)#change list to  (samples,maxlen) 2-dimensional tensor
label<-as.array(label)
#prepare the train and validation data
indices<-sample(1:nrow(data_seq))
training_indices<-indices[1:training_samples]
validation_indices<-indices[(training_samples+1):(training_samples+validation_samples)]
x_train<-data_seq[training_indices,]
y_train<-label[training_indices]
x_val<-data_seq[validation_indices,]
y_val<-label[validation_indices]
head(data_seq)

#set the LSTM model
  layer_embedding(input_dim=10000,output_dim =64)%>%
  layer_dropout(rate = 0.5)%>%
  layer_lstm(units = 64)%>%
  layer_dense(units = 1,activation = 'sigmoid')
model%>%compile(
  optimizer='rmsprop',loss='binary_crossentropy',metrics=c('acc')
)
summary(model)
history<-model%>%fit(
  x_train,y_train,epochs=20,batch_size=32,validation_data=list(x_val,y_val)
)
plot(history)

#data test 
test_sequence<-texts_to_sequences(tokenizer,test_text)
x_test<-pad_sequences(test_sequence,maxlen = maxlen)
y_test<-as.array(test_label)
model%>%evaluate(x_test,y_test)

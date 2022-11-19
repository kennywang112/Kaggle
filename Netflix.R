#https://www.kaggle.com/datasets/shivamb/netflix-shows

#package import
library(tidyverse)
library(ggplot2)
library(patchwork)
library(lubridate)
library(stringr)

#data import
data<-read_csv('../netflix.csv') 
data<-data%>%mutate(date_added=mdy(date_added))#change the date that easy to observe

#filter Movie & Tv Show
type_movie<-data%>%filter(type=='Movie')
type_tv<-data%>%filter(type=='TV Show')
ggplot()+geom_bar(data,mapping = aes(type,fill=type))+scale_fill_manual(values = c("#9E9AC8", "#6A51A3")) #plot the different type of the movie using bar plot

#Rating of movies and top directors using bar plot
data%>%filter(rating!='NA',!str_detect(rating,regex('min')))%>%
  mutate(rating=rating%>%fct_infreq()%>%
           fct_rev())%>%ggplot()+geom_bar(aes(rating,fill=rating))+
guides(fill=F)+labs(title='Ratings of movie')
dir<-data%>%count(director,sort=TRUE)%>%filter(director!='NA',n>10)
ggplot(dir)+geom_bar(mapping=aes(x=reorder(director,n,FUN=median),n,fill=n),stat='identity')+
guides(fill=F)+coord_flip()+labs(title='Top directors',x='director',y='count')

#types(Movie TV Show) after 1975 and 2006 using bar plot and density plot 
data_year_1975<-data%>%filter(release_year>1975)
data%>%filter(release_year>'2016')%>%ggplot()+geom_bar(mapping=aes(date_added,fill=type))
ggplot(data_year_1975)+geom_bar(mapping=aes(release_year,fill=type),position='dodge')
ggplot(data_year_1975)+geom_density(mapping=aes(release_year,fill=type),alpha = 0.5)

#A function that we can separate country , cast and listed_in 
#function parameter
#data: choose data to process
#c: column in data
#n: location of column
#countsum: restrict the min sum of data for top plot

sql<-data%>%select(country, type, title, listed_in,cast)#select a new data
fun<-function(data,c,n,countsum){
  nasql<-data%>%filter(!is.na(c)==TRUE)
  coun<-nasql%>%select(names(data[n]))
  mx<-max(str_count(coun[[1]], ','))
  lst<-as.character(c(seq(from=1,to=mx+1,by=1)))
  sepcoun<-coun%>%separate(names(sql[n]),into=lst,',',convert=TRUE)
  to_list<-sepcoun%>%unlist()
  to_tibble<-tibble(clm=to_list)
  ctr<-to_tibble%>%count(clm)%>%filter(n>countsum,clm!='NA')
}

#plots using function
Country_US<-data%>%filter(str_detect(country,regex('United States')))
Country_India<-data%>%filter(str_detect(country,regex('India')))
country<-fun(sql,sql$country,1,100)
ggplot(country)+geom_bar(aes(x=reorder(clm,n,FUN=median),n,fill=n),stat = 'identity')+xlab('country')+
  ylab('count')+coord_flip()+guides(fill=F)#united states has the biggest amount in the data,and then is india
  
#listed in bar plot
list_in<-fun(sql,sql$listed_in,4,300)
list_in$clm<-str_trim(list_in$clm,side = 'left')
list_in<-list_in%>%group_by(clm)%>%summarize_all(sum)
ggplot(list_in)+geom_bar(aes(x=reorder(clm,n,FUN=median),n,fill=clm),stat = 'identity')+
  xlab('listed_in')+ylab('count')+coord_flip()+guides(fill=F)
 
#cast in bar plot
cast<-fun(sql,sql$cast,5,20)
ggplot(cast)+geom_bar(aes(x=reorder(clm,n,FUN=median),n,fill=clm),stat = 'identity')+
  xlab('cast')+ylab('count')+coord_flip()+guides(fill=F)

#US cast
us_cast<-fun(Country_US,Country_US$cast,5,12)
usplot<-ggplot(us_cast)+geom_bar(aes(x=reorder(clm,n,FUN=median),n,fill=clm),stat = 'identity')+
  xlab('cast')+ylab('count')+coord_flip()+guides(fill=F)
#India cast
india_cast<-fun(Country_India,Country_India$cast,5,15)
indiaplot<-ggplot(india_cast)+geom_bar(aes(x=reorder(clm,n,FUN=median),n,fill=clm),stat = 'identity')+
  xlab('cast')+ylab('count')+coord_flip()+guides(fill=F)
usplot/indiaplot

lstin<-list_in%>%mutate(percent=n/sum(n))%>%arrange(desc(n))
lstin
#plot the percent of listed in using polar plot (pie chart)
ggplot(lstin)+
  geom_bar(mapping=aes(reorder(clm,percent,FUN=median),percent,fill=percent),stat='identity',show.legend = FALSE,width = 1)+
  theme(aspect.ratio = 1)+labs(x=NULL,y=NULL)+coord_polar()+labs(title='Listed in')

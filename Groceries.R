library(tidyverse)
library(arules)

data <- read_csv("../Kaggle_Data/Groceries_dataset.csv")
data%>%head()
data%>%dim()
data$itemDescription%>%unique()

# Count all unique data that is more than 500
data%>%group_by(itemDescription)%>%
  summarise(n = n())%>%filter(n > 500)%>%
  ggplot()+geom_col(aes(reorder(itemDescription, n), n))+coord_flip()+
  labs(x = 'itemDescription')

# Arrange the data by Member_number and then Date
data <- data%>%arrange(Member_number, as.Date(Date, format = "%d-%m-%Y"))
data
data%>%group_by(Member_number)%>%summarise(n = n())%>%arrange(desc(n))
data%>%mutate(Date = as.Date(Date, format = "%d-%m-%Y"))%>% 
  mutate(Month = format(Date, "%Y-%m"))%>%
  group_by(Month)%>%summarise(Transactions = n())%>%
  ggplot(aes(Month, Transactions))+geom_col()+
  coord_flip()

# Loop to match unique itemDescription
for (desc in data$itemDescription%>%unique()) {
  data[, desc] <- as.integer(data$itemDescription == desc)
}
new_data <- data%>%select(-itemDescription)
new_data

new_data <- new_data%>%
  group_by(Member_number, Date)%>%
  summarise(across(everything(), sum))%>%
  arrange(Member_number, as.Date(Date, format = "%d-%m-%Y"))
mt_data <- new_data[, -c(1, 2)]%>%as.matrix()
mt_data

# supp : 出現頻率至少為1%的項目集
# conf : 至少為2%
# maxlen : 最多10個項目
rule <- mt_data%>%apriori(parameter = list(supp = 0.01, conf = 0.02, maxlen = 10))
# rule <- mt_data%>%apriori()
rule%>%length()
# rule%>%head()%>%inspect()
# rule%>%head()%>%quality()

rule%>%sort(by = "support")%>%head(10)%>%inspect()
rule%>%sort(by = "confidence")%>%head(10)%>%inspect()

library(arulesViz)
#Heat map
plot(rule)
#Balloon plot
plot(rule, method = "grouped")

plot(rule, method = "graph", engine = "html")

# Parallel coordinates plot ()
plot(rule, method = "paracoord", control = list(reorder = TRUE))

library(shiny)
library(shinythemes)
ruleExplorer(rule)

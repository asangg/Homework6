library(readxl)
library(tidyverse)  
library(ggplot2)
library("stringr")
setwd("C:/Users/alexs/Documents/plan372-sp23")
data = read_csv("TS3_Raw_tree_data.csv")

#create city state column
data[,c("city", "state")]=str_match(data$City, "^([[:alnum:]]+),\\s*([[:alnum:]]+)$")[,2:3]

#Find out how many states there are, 13
unique(data$state)


#counts how many trees there are in each state
statedata<-group_by(data, state)%>%
  summarize(number=n())

statedata=na.omit(statedata)

#plot the data
ggplot(statedata, aes(x = state, y = number)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=number), vjust=-0.3, size=3.5)+
  theme_minimal()

#question 2
ncsc = data%>%
  filter(state=="NC"| state=="SC")
unique(ncsc$city) #Charlotte and Charleston

#Question 3
#Separate each genus into its own column
#Calculate the average

ncsc[,c("genus")]= str_match(ncsc$ScientificName, "\\w+") #extracts the first word and creates new column


#calculates the average for each genus
a=group_by(ncsc, genus)%>%
  summarize(Average=mean(`AvgCdia (m)`))%>%
  arrange(-Average)

#Extra credit tree age
b=group_by(ncsc, genus)%>%
  summarize(Average_Age=mean(Age))%>%
  arrange(-Average_Age)

#Ulmus
c=group_by(ncsc, genus)%>%
  summarize(Average_Age=mean(Age),Average_Crown=mean(`AvgCdia (m)`))%>%
  arrange(Average_Age)

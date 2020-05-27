

library(RCurl)

get_states<- function(){
   URL <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
   data <- read.csv(text = URL, check.names = F)
   pivot_longer(data,cols=12:dim(data)[2],names_to = "Date") ->d
   d<-d[,c(7,12,13)]
   names(d)<-c("State","Date","NCases")
   d %>% group_by(State,Date) %>% summarise(NCases=sum(NCases)) -> d
   d$Date<-as.Date(d$Date,format="%m/%d/%y")
  d %>% group_by(State) ->d
   d -> confirmed
   
   URL <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")
   data <- read.csv(text = URL, check.names = F)
   pivot_longer(data,cols=13:dim(data)[2],names_to = "Date") ->d
   d<-d[,c(7,13,14)]
   names(d)<-c("State","Date","NDeaths")
   d %>% group_by(State,Date) %>% summarise(NDeaths=sum(NDeaths)) -> d
   d$Date<-as.Date(d$Date,format="%m/%d/%y")
   d %>% group_by(State) %>% arrange(Date)  ->d
   d -> deaths
   deaths %>% left_join (confirmed) ->d
pop<-read.csv("State_pops.csv")
library(tidyverse)
d %>% left_join(pop) -> d
na.omit(d)
}


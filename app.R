#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
require(lubridate)
require(RCurl)
library(dygraphs)
library(xts)
library(lubridate)
library(mapview)
library(sf)
library(leaflet)
library(DT)
library(easynls)
library(deSolve)


## Create a SIR function with death as possible outcome

sir <- function(time, state, parameters) {
  
  with(as.list(c(state, parameters)), {
    
    dS <- -beta * S * I
    dI <-  beta * S * I - gamma * I
    dR <- gamma * I * (1-v)
    dD <- gamma * I * v
    
    return(list(c(dS, dI, dR,dD)))
  })
}

run_sir<-function(beta=0.5, gamma= 0.1, days=100, S=0.9,I=0.1, v=0.1, R=0, D=0)
{
  parameters <- c(beta = beta,gamma=gamma, v=v)
  init<- c(S = S, I = I,R = R, D=D)
  times<- seq(0, days, by = 1)
  out <- ode(y = init, times = times, func = sir, parms = parameters)
  d <- as.data.frame(out)        
  d
}



# The WDI package provides direct access to the 
# data  here 
# http://datatopics.worldbank.org/world-development-indicators/

tryObserve <- function(x) {
  x <- substitute(x)
  env <- parent.frame()
  observe({
    tryCatch(
      eval(x, env),
      error = function(e) {
        showNotification(paste("Error: ", e$message), type = "error")
      }
    )
  })
}



library(WDI)
wd <- WDI(country="all", indicator=c( "SP.POP.TOTL", "SP.POP.65UP.TO.ZS"), start=2018, end=2018, extra = TRUE)
wd<-wd[c(1,4,5)]
names(wd)[2:3] <-c("pop","p_over_65")

## A small function to ensure that the data tables have download buttons

dt<-function(d) {DT::datatable(d, 
                           filter = "top",                         
                           extensions = c('Buttons'), options = list(
                             dom = 'Blfrtip',
                             buttons = c('copy', 'csv', 'excel'), colReorder = TRUE
                           ))}

## The jh_data function was cobbled together in parts while tying to scrape and reshape the data from the gthub repository
# Johns Hopkins have changed the format twice since they started publishing it.
# No guarantee that they won't again, so not worth making the function more elegant!

jh_data <- function(){
  
  URL <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
  data <- read.csv(text = URL, check.names = F)
  
  pivot_longer(data,cols=5:dim(data)[2],names_to = "Date") ->d
  names(d)<-c("Province","Country","Lat","Long","Date","NCases")
  d$Date<-as.Date(d$Date,format="%m/%d/%y")
  
  Confirmed<-d
  
  URL <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
  data <- read.csv(text = URL, check.names = F)
  
  pivot_longer(data,cols=5:dim(data)[2],names_to = "Date") ->d
  names(d)<-c("Province","Country","Lat","Long","Date","NCases")
  
  d$Date<-as.Date(d$Date,format="%m/%d/%y")
  
  Deaths<-d
  
  URL <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
  data <- read.csv(text = URL, check.names = F)
  
  pivot_longer(data,cols=5:dim(data)[2],names_to = "Date") ->d
  names(d)<-c("Province","Country","Lat","Long","Date","NCases")
  
  d$Date<-as.Date(d$Date,format="%m/%d/%y")
  Recovered<-d
  
  
  Confirmed %>% group_by(Country, Date) %>% summarise(NCases=sum(NCases)) -> confirmed_country
  Deaths %>% group_by(Country, Date) %>% summarise(NDeaths=sum(NCases)) -> deaths_country
  Recovered %>% group_by(Country, Date) %>% summarise(NRecovered=sum(NCases)) -> recovered_country
  
  confirmed_country %>% left_join(deaths_country,by = c("Country", "Date")) %>% left_join(recovered_country, by = c("Country", "Date")) -> by_country
  by_country%>%arrange(Date) %>% mutate(New_cases = NCases - lag(NCases, default = first(NCases)), NActive=NCases-NDeaths-NRecovered) -> by_country
  by_country
}


df<-jh_data()

# Good idea to periodically save the data, ready for the day that JHs site goes down for ever!

#save(df,file=sprintf("df%s.rda",Sys.Date() ))
df %>% mutate(Daily_deaths = NDeaths - lag(NDeaths, default = first(NDeaths))) %>% arrange(NDeaths) -> df

df %>% group_by(Country) %>% summarise( max=max(NDeaths)) %>% arrange(-max) %>% mutate(Country = factor(Country, Country)) -> tmp
c_options<-levels(tmp$Country)

df %>% mutate(Daily_deaths = NDeaths - lag(NDeaths, default = first(NDeaths))) %>% arrange(Daily_deaths) -> df

## The jh_iso file is just a list of countries and their corresonding codes, used for merging
## Some still need correction 
load("jh_iso.rda")
## Some corrections of the jh_isos 
jh_iso$iso2c[jh_iso$Country=="Serbia"] <-"CS"
jh_iso$iso2c[jh_iso$Country=="Holy See"] <-NA
jh_iso$iso2c[jh_iso$Country=="France"]<-"FR"
jh_iso$iso2c[jh_iso$Country=="Canada"]<-"CA"
df<-merge(df,jh_iso)
df<-merge(wd,df)
df$p_over_65<-round(df$p_over_65,1)

library(zoo)
df %>% mutate (pop_over_65 = round(pop*p_over_65/100,1)) %>% mutate(NDeathsp65 = round( NDeaths/(pop_over_65/100000),0)) %>% arrange(Country,Date) %>% mutate(deaths_7day_mean=round(rollapply(Daily_deaths,7,mean,align='right',fill=NA),1)) ->df
d<-df
df2<-df[,c(1,4,5,6,7,9,10,11,12,13, 14)] 
df2 %>% filter(Date==max(Date)) ->dd



### Test




# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Investigating matches to the SIR model"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Country choice",
        h4("Select a country from the list"),
        h5("If the country does not appear in the drop down menu press backspace and type a few letters to find it."),
        h6("The figures show the running average of the reported daily incidence of death with Covid or confirmed new case."),
        selectInput("country", "Country", c_options, selected = 'United Kingdom', multiple = FALSE,
                    selectize = TRUE, width = NULL, size = NULL),
        sliderInput("ndays", "Number of days for running average:",
                    min = 1, max = 20, value =7, step=1
        ),
        checkboxInput("lg", "Logged y axis", value = FALSE, width = NULL),
        
        sliderInput("sdate",
                    "Start date:",
                    min = as.Date("2020-01-01","%Y-%m-%d"),
                    max = as.Date("2020-04-01","%Y-%m-%d"),
                    value=as.Date("2020-03-01"),
                    timeFormat="%Y-%m-%d")
      ),tabPanel("SIR",
                 sliderInput("beta",
                             "Beta",
                             min = 0,
                             max = 1,
                             value = 0.5,
                             step=0.01),
                 
                 sliderInput("gdays",
                             "Days infectious (1/gamma)",
                             min = 1,
                             max = 20,
                             value = 4),
                 sliderInput("ifr",
                             "Incidence fatality rate",
                             min = 0.001,
                             max = 0.01,
                             value = 0.001,
                             step=0.001),
                 sliderInput("immunity",
                             "% of population that are naturally immune:",
                             min = 0,
                             max = 100,
                             value = 20)
      
                 
                 )) ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Deaths",  dygraphOutput("deaths")),
                    tabPanel("Cases",  dygraphOutput("cases")),
                    tabPanel("Gompertz fit", 
                             h5("This is simple curve fitting. Do not interpret the output as a predictive model!"),
                             h6("Uses non linear least squares to fit Gompertz to cumulative deaths per 100 thousand of the total population"),
                             h6("Note that fitting may throw errors if the model does not converge."),
                             plotOutput("gplot"),
                             verbatimTextOutput  ("results")),
                    
                    tabPanel("SIR fit to cumulative deaths", 
                             
                             plotOutput("sircumplot") ),
                    tabPanel("SIR fit to daily deaths", 
                             
                             plotOutput("sirplot") ),
                    tabPanel("SIR output", 
                             
                          
                             dygraphOutput("sird") )
                    
        )
        
      )
   )
)


server <- function(input, output) {
   
   output$deaths <- renderDygraph({
     countries<-input$country
     ndays<-input$ndays
     lscale<-input$lg
     sdate<-input$sdate
     d %>% filter(Country %in% countries) %>% arrange(Date) %>% filter(Date > sdate ) ->dd
     d_xts<-xts(x = dd$Daily_deaths, order.by = dd$Date)
     d_xts %>% dygraph(group = "Country")  %>%  dyRoller(rollPeriod = ndays) %>% dyRangeSelector() %>% dyAxis(name="y",logscale=lscale)
   })
   
   output$cases <- renderDygraph({
     countries<-input$country
     ndays<-input$ndays
     lscale<-input$lg
     sdate<-input$sdate
     d %>% filter(Country %in% countries) %>% arrange(Date) %>% filter(Date > sdate ) ->dd
     d_xts<-xts(x = dd$New_cases, order.by = dd$Date)
     d_xts %>% dygraph(group = "Country")  %>%  dyRoller(rollPeriod = ndays) %>% dyRangeSelector() %>% dyAxis(name="y",logscale=lscale)
   })
   
   output$cases10k <- renderDygraph({
     countries<-input$country
     ndays<-input$ndays
     lscale<-input$lg
     sdate<-input$sdate
     d %>% filter(Country %in% countries) %>% arrange(Date) %>% filter(Date > sdate ) ->dd
     d_xts<-xts(x = (dd$New_cases/(dd$pop/10000))*7, order.by = dd$Date)
     d_xts %>% dygraph(group = "Country",ylab = "Weekly cases per 10k of the population" )  %>%  dyRoller(rollPeriod = ndays) %>% dyRangeSelector() %>% dyAxis(name="y",logscale=lscale)
   })
   
   output$cumcases10k <- renderDygraph({
     countries<-input$country
     ndays<-input$ndays
     lscale<-input$lg
     sdate<-input$sdate
     d %>% filter(Country %in% countries) %>% arrange(Date) %>% filter(Date > sdate ) ->dd
     d_xts<-xts(x = (dd$NCases/(dd$pop/10000))*7, order.by = dd$Date)
     d_xts %>% dygraph(group = "Country",ylab = "Weekly cases per 10k of the population" )  %>%  dyRoller(rollPeriod = ndays) %>% dyRangeSelector() %>% dyAxis(name="y",logscale=lscale)
   })
   

   
   tryObserve({
     countries<-input$country
     sdate<-input$sdate
    
     d %>% filter(Country %in% countries) %>% arrange(Date) %>% filter(Date > sdate ) ->dd
     days<-length(dd$NDeaths)
     gdata<-data.frame(days=1:days, deaths= dd$NDeaths /(dd$pop/100000))

     model2 = nlsfit(gdata, model = 10, start = c(a = 200, b = 17, c = 0.06))
     output$results <- renderPrint(model2)

     output$gplot<- renderPlot({
     nlsplot(gdata, model = 10, start = c(a = 200, b = 17, c = 0.06),
             xlab = "Days" , ylab = "Cumulative number of deaths per 100k of total population", position = 1)
     })
   })
   
   tryObserve({
     countries<-input$country
     sdate<-input$sdate
     immunity<-input$immunity/100
     beta<-input$beta
     gamma<-1/input$gdays
     ifr<-input$ifr

    # countries <-"US"
    # sdate<-as.Date("2020-03-01")
    # immunity<-0.2
    # beta<-0.4
    # gamma<-0.2
    # ifr<-0.001
     d %>% filter(Country %in% countries) %>% arrange(Date) %>% filter(Date > sdate ) ->dd2
     days<-length(dd2$NDeaths)
   gdata2<-data.frame(days=1:days, deaths= dd2$NDeaths)
   gdata2 %>% mutate(Daily_deaths = deaths - lag(deaths, default = first(deaths))) %>% arrange(deaths) -> gdata2
   
   N<-dd$pop[1]
   
   
   init_deaths<-gdata2$deaths[1] +1 # Seed with one extra detah for situations where there are no cases to start
   init_cases<-init_deaths/ifr
   
   I <- init_cases/N
   S <- 1-I-immunity
   R <- immunity
   D <- init_deaths/N
  
   
   sd<-run_sir(beta=beta, gamma= gamma, S=S, I=I, days=days+15, D=0, R=R, v=ifr)
  
   output$sird <- renderDygraph(dygraph(sd))
   ds<-pivot_longer(sd,cols=2:5)
   ds$value<-ds$value*N
   
   #g1<-ggplot(data=ds,aes(x=time,y=value,color=name))
   #g1 +geom_line() +xlab("Time") +ylab("Number of people")
   output$sirplot<- renderPlot({
     plot_title<-sprintf("R zero approximately %s",round(beta/gamma,2))
     ds %>% filter(name=="D") %>% mutate(Daily_deaths = value - lag(value, default = first(value))) %>% arrange(value) -> ds2
     ds2 %>% filter(time>15) %>%
     ggplot(aes(x=time,y=Daily_deaths)) + 
     geom_line() + geom_line( aes(y=gdata2$Daily_deaths), col="red") + ggtitle(plot_title)
   })
   
   output$sircumplot<- renderPlot({
     plot_title<-sprintf("R zero approximately %s",round(beta/gamma,2))
     ds %>% filter(name=="D") %>% mutate(Daily_deaths = value - lag(value, default = first(value))) %>% arrange(value) -> ds2
     ds2 %>% filter(time>15) %>%
       ggplot(aes(x=time,y=value)) +
       geom_line() + geom_line( aes(y=gdata2$deaths), col="red") + ggtitle(plot_title)
   })

   
   })
   
   
}









# Run the application 
shinyApp(ui = ui, server = server)


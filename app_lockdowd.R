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
    dH <- gamma * I * v
    dD <- gamma2 * H
    
    return(list(c(dS, dI, dR,dH,dD)))
  })
}

run_sir<-function(beta=0.5, gamma= 0.1, days=100, S=0.9,I=0.1, v=0.1, R=0, D=0, H=0, gamma2=1/15)
{
  parameters <- c(beta = beta,gamma=gamma, v=v, gamma2=gamma2)
  init<- c(S = S, I = I,R = R, D=D, H=H)
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
wd<-wd[c(1,4)]
names(wd)[2] <-c("pop")

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
  
  Confirmed %>% group_by(Country, Date) %>% summarise(NCases=sum(NCases)) -> confirmed_country
  Deaths %>% group_by(Country, Date) %>% summarise(NDeaths=sum(NCases)) -> deaths_country
 confirmed_country %>% left_join(deaths_country,by = c("Country", "Date")) -> by_country
  
  by_country
}


df<-jh_data()

source("states.R")
df2<-get_states()

df %>%arrange(Date) %>% mutate(New_cases = NCases - lag(NCases, default = first(NCases))) -> df
df %>% mutate(Daily_deaths = NDeaths - lag(NDeaths, default = first(NDeaths))) %>% arrange(NDeaths) -> df
df %>% mutate(Daily_deaths = NDeaths - lag(NDeaths, default = first(NDeaths))) %>% arrange(Daily_deaths) -> df

df2 %>%arrange(Date) %>% mutate(New_cases = NCases - lag(NCases, default = first(NCases))) -> df2
df2 %>% mutate(Daily_deaths = NDeaths - lag(NDeaths, default = first(NDeaths))) %>% arrange(NDeaths) -> df2
df2 %>% mutate(Daily_deaths = NDeaths - lag(NDeaths, default = first(NDeaths))) %>% arrange(Daily_deaths) -> df2


## The jh_iso file is just a list of countries and their corresonding codes, used for merging
## Some still need correction 
## Some corrections of the jh_isos 
# jh_iso$iso2c[jh_iso$Country=="Serbia"] <-"CS"
# jh_iso$iso2c[jh_iso$Country=="Holy See"] <-NA
# jh_iso$iso2c[jh_iso$Country=="France"]<-"FR"
# jh_iso$iso2c[jh_iso$Country=="Canada"]<-"CA"
# save(jh_iso, file="jh_iso.rda")

load("jh_iso.rda")

df<-merge(df,jh_iso)
df<-merge(wd,df)
df<-df[,c(3:8,2)]
df2<- df2[,c(1,2,4,3,6,7,5)]

names(df2)[1]<-"Country"

names(df)
names(df2)

df<-bind_rows(df,df2)
d<-df
d$Daily_deaths[d$Daily_deaths < 0]<-NA

df %>% group_by(Country) %>% summarise( max=max(NDeaths)) %>% arrange(-max) %>% mutate(Country = factor(Country, Country)) -> tmp
c_options<-levels(tmp$Country)

#nm<- sprintf("workspace%s.rda", Sys.Date())
#save(list=ls(),file=nm)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Heuristic matching of the SIR model"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Choose data options",
        h4("Select a country or US State from the list"),
        h5("If the country or State does not appear in the drop down menu press backspace and type a few letters to find it."),
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
      ),tabPanel("SIR model parameters",
        h6("The interface allows combinations of SIR model parameters to be quickly evaluated against data."),         
        h6("There are many differnt combinations of parameters that can produce a close match to the data"),
        h6("The SIR model assmes panmixia so is likely to be particularly inappropriate for countries such as the US or Brazil: The data set includes US states as entities." ),
                 sliderInput("beta",
                             "Beta",
                             min = 0,
                             max = 1,
                             value = 0.53,
                             step=0.01),
                 
                 sliderInput("gdays",
                             "Days infectious (1/gamma)",
                             min = 1,
                             max = 20,
                             value = 3),
                 sliderInput("ifr",
                             "Incidence fatality rate per thousand",
                             min = 0.1,
                             max = 100,
                             value = 10,
                             step=0.1),
                 sliderInput("immunity",
                             "% of population that are naturally immune:",
                             min = 0,
                             max = 100,
                             value = 0),
                 
                 sliderInput("burn_in",
                             "Number of days prior to date chosen to start SIR model run:",
                             min = 7,
                             max = 30,
                             value = 15),
                sliderInput("Lockdown",
                    "Number of days to lockdown:",
                    min = 7,
                    max = 30,
                    value = 15)
                 
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
     ifr<-input$ifr/1000
     burn_in <- input$burn_in  ## This is the number of days prior to the observed death number at which to start the run
     gamma2<-  1/burn_in
     lockdown <- input$Lockdown
     
    #### For testing code
     # countries <-"US"
     # sdate<-as.Date("2020-03-01")
     # immunity<-0.2
     # beta<-0.4
     # gamma<-0.2
     # ifr<-0.001
     # lockdown <- 15
     # burn_in <- 15
     # gamma2<-  1/burn_in
     
     
     d %>% filter(Country %in% countries) %>% arrange(Date) %>% filter(Date > sdate ) ->dd2
     days<-length(dd2$NDeaths)
     run_length <- days+burn_in
  ### Arrange a data frame for work with just two columns, one being total deaths and the other days,
     
   gdata2<-data.frame(days=1:days, deaths= dd2$NDeaths)
   ## Find the differences (i.e. the daily deaths)
   gdata2 %>% mutate(Daily_deaths = deaths - lag(deaths, default = first(deaths))) %>% arrange(deaths) -> gdata2
   
   gdata2$Daily_deaths[gdata2$Daily_deaths<0]<-NA
   
   N<-dd2$pop[1]  # Population at risk
   init_deaths<-gdata2$deaths[1] +1 # Seed with one extra death just in case there are some situations where there are no deaths to start
   
   init_cases<-init_deaths/ifr ## Dividing by the incidence fatality rate gives the number of cases at the start
   
   
   I <- init_cases/N
   S <- 1-I-immunity
   R <- immunity
   D <- init_deaths/N
   H <- D/gamma2
 
   run_1 <- burn_in + lockdown ### First part of the run up to lockdown
   sd1<-run_sir(beta=beta, gamma= gamma, S=S, I=I,D=D,H=H, days=run_1 , R=R, v=ifr, gamma2)
   
   
   ## Reset post lockdown
  
   nn<-length(sd1$I)
   I <- sd1$I[ nn]
   S <- sd1$S[ nn ]
   R <- sd1$R[ nn]
   D <- sd1$D[ nn ]
   H <- sd1$H[ nn ]
   run_2 <- run_length-run_1
   
   sd2<-run_sir(beta=gamma*0.8, gamma= gamma, S=S, I=I,D=D,H=H, days=run_2, R=R, v=ifr, gamma2)[-1,]
   sd<-rbind(sd1,sd2)
   sd$time<-0:run_length
  
   output$sird <- renderDygraph(dygraph(sd))
   ds<-pivot_longer(sd,cols=2:5)
   ds$value<-ds$value*N
   
   #g1<-ggplot(data=ds,aes(x=time,y=value,color=name))
   #g1 +geom_line() +xlab("Time") +ylab("Number of people")
   output$sirplot<- renderPlot({
     plot_title<-sprintf("R zero approximately %s",round(beta/gamma,2))
     ds %>% filter(name=="D") %>% mutate(Daily_deaths = value - lag(value, default = first(value))) %>% arrange(value) -> ds2
     ds2 %>% filter(time>burn_in) %>%
     ggplot(aes(x=time,y=Daily_deaths)) + 
     geom_line() + geom_line( aes(y=gdata2$Daily_deaths), col="red") + ggtitle(plot_title)
   })
   
   output$sircumplot<- renderPlot({
     plot_title<-sprintf("R zero approximately %s",round(beta/gamma,2))
     ds %>% filter(name=="D") %>% mutate(Daily_deaths = value - lag(value, default = first(value))) %>% arrange(value) -> ds2
     ds2 %>% filter(time>burn_in) %>%
       ggplot(aes(x=time,y=value)) +
       geom_line() + geom_line( aes(y=gdata2$deaths), col="red") + ggtitle(plot_title)
   })

   
   })
   
   
}









# Run the application 
shinyApp(ui = ui, server = server)


# ui.R

library(shiny)
library(base64enc)
library(rCharts)
library(googleVis)
options(RCHART_LIB = 'morris')

inter.spain<-read.csv("data/inter.spain.csv")
start.date<-as.Date(inter.spain[1,1])
end.date<-as.Date(inter.spain[nrow(inter.spain),1])

shinyUI(fluidPage(
  titlePanel("An overview on tourism data in Spain"),
  p("Hi :)
This app was created for educational purposes for tourism degree studens.
It gets data from the", a("Tourism Studies Institute", href="http://www.iet.tourspain.es/en-EN/Paginas/default.aspx"), 
                          "in Spain and displays it in interactive, 
time series charts.The app also features some important figures from a time
interval selected by the user (from 2001 to 2014). 
"),
  p("One of the goals of this app is to show the importance of seasonality 
in touristic markets. The first chart displays the number of visitors in Spain, where
the user can change the levels of aggregation and choose from one
of the two types of visitors according to the definition by the",  a("Word Tourism Organization", href="http://www2.unwto.org/"),
": Tourist and Excursionists
"),
  p("The second chart shows the interannual growth rate in the number of
tourist for the main turistic regions in Spain, as well as this grow rate for Spain itself
"),
  p("Finally, a third chart displays the mean daily expenditures and the average days spent in Spain by tourists from 
    the most important tourists provider countries. This chart is animated, presenting the data thorught time (from 2004 to 2014)"),
  
  sidebarPanel(
    helpText("Select a time interval to perform the analysis"),
    dateRangeInput("dat", label=NULL, start="2008-01-01", "2012-01-01",
                   , min=inter.spain[1,1], max=inter.spain[nrow(inter.spain),1]),
    h4("Spain"),
    h5("Tourists"),
    textOutput("totTurSpain"),
    textOutput("maxTurSpain"),
    textOutput("minTurSpain"),
    h5("Excursionists"),
    textOutput("totExcSpain"),
    textOutput("maxExcSpain"),
    textOutput("minExcSpain"),
    h4(textOutput("regsel")),
    h5("Tourists"),
    textOutput("totReg"),
    textOutput("maxReg"),
    textOutput("minReg"),
    textOutput("maxIntReg"),
    textOutput("minIntReg"),
    width=5
  ),
  
  mainPanel(
    tableOutput("table"),
    h4("Number of visitors in Spain"),
    selectInput("agreg", h6("Aggregation"), choices=c("Monthly", "Trimestral", "Annual"),
                selected="Monthly"),
    radioButtons("visitor", label=NULL, choices=c("Turists", "Excursionists"),
                 selected="Turists", inline=TRUE),  
    showOutput("series", lib="morris"),
    h4("Tourists interannual growth rate"),
    selectInput("region", label=NULL, choices=c("Andalusia", "Balearic Islands",
                                                  "Canary Islands", "Catalonia",
                                                  "Valencia", "Madrid", "Rest of regions"),
                selected="Balearic Islands"),
    checkboxInput("interspain", "Show Spain interannual growth rate"),
    showOutput("rate", lib="morris"),
    h4("Daily expenditures and total days spent in Spain by international tourists"),
    htmlOutput("expend") 
  )
)
)
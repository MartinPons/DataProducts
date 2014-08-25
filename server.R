# server.R

## Loading libraries
library(shiny)
library(base64enc)
library(rCharts)
library(googleVis)
library(zoo)
options(RCHART_LIB = 'morris')

## Loading data
annual.data<-read.csv("data/annual.data.csv")
data.spain<-read.csv("data/data.spain.csv")
data.regions<-read.csv("data/data.regions.csv")
daysexp.a<-read.csv("data/daysexp.a.csv")
daysexp.m<-read.csv("data/daysexp.m.csv")
expend.a<-read.csv("data/expend.a.csv")
expend.m<-read.csv("data/expend.m.csv")
inter.regions<-read.csv("data/inter.regions.csv")
inter.spain<-read.csv("data/inter.spain.csv")
trim.data<-read.csv("data/trim.data.csv")
year.ed<-read.csv("data/year.ed.csv")

## Correcting colNames for interregions, previously deformated when saving it as a csv table
regions<-c("Andalusia", "Balearic Islands", "Canary Islands", 
           "Catalonia", "Valencia", "Madrid", "Rest of regions")
names(inter.regions)<-c("Date", regions, "Spain")
names(data.regions)<-c("Date", regions)

shinyServer(function(input, output) {
  
  ## Selecting data
  aggregatedData<-reactive({switch(input$agreg, 
                               "Monthly"=data.spain,
                               "Trimestral"=trim.data,
                               "Annual"=annual.data)
    })
  
  
  finalData<-reactive({
    agg.data<-aggregatedData()
    agg.data<-agg.data[as.Date(agg.data$Date)>input$dat[1] & as.Date(agg.data$Date)<input$dat[2],]
    agg.data})
  
  finalMonthly<-reactive({
    data.spain[as.Date(data.spain$Date)>input$dat[1] & as.Date(data.spain$Date)<input$dat[2],]
  })
  
  
  final.data.inter<-reactive({
    inter.regions$Date<-as.Date(inter.regions$Date)
    inter.regions<-inter.regions[inter.regions$Date>input$dat[1] & inter.regions$Date<input$dat[2],]
    inter.regions})
  
  final.data.region<-reactive({
    data.regions[as.Date(data.regions$Date)>input$dat[1] & as.Date(data.regions$Date)<input$dat[2],]    
  })
  
  ## Spain figures
  output$totTurSpain<-renderText({paste("Total number:", 
                                        sum(as.numeric(as.character(finalMonthly()$Turists)))
                                        )})
  
  output$maxTurSpain<-renderText({
    month<-finalMonthly()
    maxTurIndex<-which.max(as.numeric(as.character(month$Turists)))
    maxtur<-month[maxTurIndex, "Turists"]
    maxTurDate<-as.yearmon(month[maxTurIndex, "Date"])
    paste("Maximum:",maxtur, "in", maxTurDate)
  })
  
  output$minTurSpain<-renderText({
    month<-finalMonthly()
    minTurIndex<-which.min(as.numeric(as.character(month$Turists)))
    mintur<-month[minTurIndex, "Turists"]
    minTurDate<-as.yearmon(month[minTurIndex, "Date"])
    paste("Minimum:",mintur, "in", minTurDate)
  })
  
  output$totExcSpain<-renderText({paste("Total number:", 
                                        sum(as.numeric(as.character(finalMonthly()$Excursionists))))})
  
  output$maxExcSpain<-renderText({
    month<-finalMonthly()
    maxExcIndex<-which.max(as.numeric(as.character(month$Excursionists)))
    maxexc<-month[maxExcIndex, "Excursionists"]
    maxExcDate<-as.yearmon(month[maxExcIndex, "Date"])
    paste("Maximum:",maxexc, "in", maxExcDate)
  })


  
  output$minExcSpain<-renderText({
    month<-finalMonthly()
    minExcIndex<-which.min(as.numeric(as.character(month$Excursionists)))
    minexc<-month[minExcIndex, "Excursionists"]
    minExcDate<-as.yearmon(month[minExcIndex, "Date"])
    paste("Minimum:",minexc, "in", minExcDate)
  })
  
  ## Region figures
  output$regsel<-renderText({input$region})
  
  output$totReg<-renderText({
    region.selected<-input$region
    paste("Total number:", 
          sum(as.numeric(as.character(final.data.region()[,region.selected]))))
    })
   
  output$maxReg<-renderText({
   region.selected<-input$region
    maxRegIndex<-which.max(as.numeric(as.character(final.data.region()[,region.selected])))
    maxreg<-final.data.region()[maxRegIndex, region.selected]
    maxRegDate<-as.yearmon(final.data.region()[maxRegIndex, "Date"])
    paste("Maximum:", maxreg, "in", maxRegDate)
  })
  
  output$minReg<-renderText({
    region.selected<-input$region
    minRegIndex<-which.min(as.numeric(as.character(final.data.region()[,region.selected])))
    minreg<-final.data.region()[minRegIndex, region.selected]
    minRegDate<-as.yearmon(final.data.region()[minRegIndex, "Date"])
    paste("Minimum:", minreg, "in", minRegDate)
  })

  output$maxIntReg<-renderText({
    region.selected<-input$region
    maxIntIndex<-which.max(as.numeric(as.character(final.data.inter()[,region.selected])))
    maxint<-final.data.inter()[maxIntIndex, region.selected]
    maxIntDate<-as.yearmon(final.data.inter()[maxIntIndex, "Date"])
    paste("Maximum growth:", round(maxint*100,1), "% in", maxIntDate)
  })
  
  output$minIntReg<-renderText({
    region.selected<-input$region
    minIntIndex<-which.min(as.numeric(as.character(final.data.inter()[,region.selected])))
    minint<-final.data.inter()[minIntIndex, region.selected]
    minIntDate<-as.yearmon(final.data.inter()[minIntIndex, "Date"])
    paste("Minimum growth:", round(minint*100,1), "% in", minIntDate)
  })

  
  ## Figures
  output$series<-renderChart({     
    m2<-mPlot(x=as.character("Date"), y=input$visitor, data=finalData(), type="Line")
    m2$set(pointSize=0, lineWidth=1, title="hola", dom='series', width=600, height=300) 
    return(m2)    
  })
  
  output$rate<-renderChart({
    interannual<-final.data.inter()
    interannual$Date<-as.character(interannual$Date)
    if (input$interspain){
      m1<-mPlot(x="Date", y=c(input$region, "Spain"), data=interannual, type="Line")}
    else {
      m1<-mPlot(x="Date", y=c(input$region), data=interannual, type="Line")}
    
    m1$set(pointSize=0, lineWidth=1, dom='rate', width=600, height=300)

  
     return(m1)
  })
  
 
  google.plot<-gvisMotionChart(year.ed, "Country", "Year", 
                               options=list(width=600, height=400)
                               ,colorvar="Country", sizevar="populations",
                               xvar="Days", yvar="Expenditure")
  output$expend<-renderGvis({google.plot
  })
})

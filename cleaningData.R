##################################################################################
## DOWNLOADING, CLEANING AND FORMATING DATA FROM iNSTITUTO NACIONAL DEL TURISMO ##
##################################################################################

## SETTING INITIAL PARAMETERS AND LOADING NECESSARY PACKAGES

setwd("C:/Users/martin/Desktop/Data Science/Developing data products/turismProject")
library(zoo)
library(reshape2)
Sys.setlocale("LC_ALL", "English")

source("helplers.R")

## READING FILES FROM INTITUTO NACIONAL DEL TURISMO

# Number of visitors (turists + excursionist) in Spain
file.spain<-"http://www.iet.tourspain.es/WebPartInformes/paginas/rsinforme.aspx?ruta=%2fFrontur%2fSerie%2fMensual%2fEntradas+de+visitantes+seg%u00fan+tipolog%u00eda.+-+Ref.219&per=0&anio_ini=2000&anio_fin=2014&pag=1&idioma=es&tipo=2&grp=2&formato=CSV"
data.spain<-read.csv(file.spain, header=T)

# Number of turist to main regional destinations in Spain
file.regions<-"http://www.iet.tourspain.es/WebPartInformes/paginas/rsinforme.aspx?ruta=%2fFrontur%2fSerie%2fMensual%2fEntradas+de+turistas+seg%u00fan+Comunidad+Aut%u00f3noma+de+destino+principal.+-+Ref.235&per=0&anio_ini=2000&anio_fin=2014&pag=1&idioma=es&tipo=2&grp=2&formato=CSV"
data.regions<-read.csv(file.regions, header=T)


## CLEANING DATA AND FORMATTING DATA

## Subsetting columns to the ones that have actual data
data.spain<-data.spain[,c(6,8:9)]
data.regions<-data.regions[,c(11,13:19)]


## Getting rid of dots that separates thousands (using helpler funcion: getridofdots)
for (col in 2:3){
  data.spain[,col]<-getridofdots(data.spain[,col])
}
for (col in 2:8){
  data.regions[,col]<-getridofdots(data.regions[,col])
}

## Elinating "Total" rows
without.totals.spain<-grep("Total", data.spain[,1], invert=TRUE)
data.spain<-data.spain[without.totals.spain,]

without.totals.regions<-grep("Total", data.regions[,1], invert=TRUE)
data.regions<-data.regions[without.totals.regions,]

## Traslating months from Date column into english (with helpler function translate.date)
data.spain[,1]<-translate.date(data.spain[,1])
data.regions[,1]<-translate.date(data.regions[,1])

## Formatting dates with month and year into a zoo class
data.spain[,1]<-as.Date(as.yearmon(data.spain[,1]))
data.spain[,1]<-format(data.spain[,1], "%m-%d-%Y")

data.regions[,1]<-as.Date(as.yearmon(data.regions[,1]))
data.regions[,1]<-format(data.regions[,1], "%m-%d-%Y")

## transforming data in timeseries
data.spain<-ts(as.matrix(data.spain[,2:3]), start=c(2000,1), frequency=12)
data.regions<-ts(as.matrix(data.regions[,2:8]), start=c(2000, 1), frequency=12)


## interanual rates
inter.spain<-round(diff(data.spain, 12)/lag(data.spain, k=-12),3)
date.inter<-as.Date(inter.spain)
inter.spain<-as.data.frame(cbind(as.character(date.inter), inter.spain))

inter.regions<-round(diff(data.regions, 12)/lag(data.regions, k=-12),3)
inter.regions<-as.data.frame(cbind(as.character(date.inter), 
                                   inter.regions, 
                                   as.numeric(as.character(inter.spain[,ncol(inter.spain)]))))




regions<-c("Andalusia", "Balearic Islands", "Canary Islands", 
           "Catalonia", "Valencia", "Madrid", "Rest of regions")
visitors<-c("Turists", "Excursionists")

names(inter.spain)<-c("Date", visitors)
names(inter.regions)<-c("Date", regions, "Spain")

## Agggregations (Trimestral, Annual)

trim.data<-aggregate(data.spain, nfrequency=4)
date.trim<-as.Date(trim.data)
trim.data<-as.data.frame(cbind(as.character(date.trim), trim.data))
names(trim.data)<-c("Date", visitors)

annual.data<-aggregate(data.spain, nfrequency=1)
date.annual<-as.Date(annual.data)
annual.data<-as.data.frame(cbind(as.character(date.annual), annual.data))
names(annual.data)<-c("Date", visitors)

date<-as.Date(data.spain)
data.spain<-as.data.frame(cbind(as.character(date), data.spain))
names(data.spain)<-c("Date", visitors)

data.regions<-as.data.frame(cbind(as.character(date), data.regions))
names(data.regions)<-c("Date", regions)

write.table(data.spain, file="data/data.spain.csv", sep=",")
write.table(trim.data, file="data/trim.data.csv", sep=",")
write.table(annual.data, file="data/annual.data.csv", sep=",")
write.table(inter.regions, file="data/inter.regions.csv", sep=",")
write.table(inter.spain, file="data/inter.spain.csv", sep=",")
write.table(data.regions, file="data/data.regions.csv", sep=",")

###########################################################
# EXPENDITURES AND DAYS
################################################

## loading data
file.days<-"http://www.iet.tourspain.es/WebPartInformes/paginas/rsinforme.aspx?ruta=%2fEgatur%2fSerie%2fMensual%2fEstancia+media+de+los+turistas+seg%u00fan+pa%u00eds+de+residencia+-+Ref.2305&per=0&anio_ini=2004&anio_fin=2014&pag=1&idioma=es&tipo=2&grp=32768&formato=CSV"
days<-read.table(file.days, sep=",", dec=",", header=T)
days<-days[,3:9]

file.exp<-"http://www.iet.tourspain.es/WebPartInformes/paginas/rsinforme.aspx?ruta=%2fEgatur%2fSerie%2fMensual%2fGasto+medio+diario+de+los+turistas+seg%u00fan+pa%u00eds+de+residencia+-+Ref.2318&per=0&anio_ini=2004&anio_fin=2014&pag=1&idioma=es&tipo=2&grp=32768&formato=CSV"
expend<-read.table(file.exp, sep=",", dec=",", header=T)
expend<-expend[,c(5:10)]




## combining data
daysexpend<-cbind(days, expend)

## formating dates column
daysexpend[,1]<-translate.date(daysexpend[,1])

## naming data
countries<-c("Germany", "France", "Italy", "Netherlands",
             "UnitedKingdom", "Restoftheworld")

names(daysexpend)<-c("Date", 
                     paste("days", countries, sep=""),
                     paste("exp", countries, sep=""))



## getting rid of totals

without.totals<-grep("Total", daysexpend[,1], invert=T)
daysexp.m<-daysexpend[without.totals,]
only.totals<-grep("Total", daysexpend[,1], invert=F)
daysexp.a<-daysexpend[only.totals,]
daysexp.a[,1]<-2004:2014


daysexp.m[,1]<-translate.date(daysexp.m[,1])

## formating column 1 as date class
daysexp.m$Date<-as.Date(as.yearmon(daysexp.m$Date))


## Separating expenditures from days
expend.m<-daysexp.m[,c(1, 8:13)]
expend.a<-daysexp.a[,c(1,8:13)]

days.m<-daysexp.m[,1:7]
days.a<-daysexp.a[,1:7]

## cleaning names
names(expend.a)<-gsub("exp", "", names(expend.a))
names(days.a)<-gsub("days", "", names(days.a))

## Prepare data for plot: reshaping
expMelt<-melt(expend.a, id="Date", measure.vars=names(expend.a)[2:7])
daysMelt<-melt(days.a, id="Date", measure.vars=names(days.a)[2:ncol(days.a)])

## Prepare data for plot: merging data
year.ed<-merge(expMelt, daysMelt, by=c("Date", "variable"))
names(year.ed)<-c("Year", "Country", "Expenditure", "Days")

## Getridof Rest of the world
year.ed<-year.ed[grep("Restoftheworld", year.ed[,2], invert=T),]

## Prepare data for plot: population vector
populations<-c(66616416, 80716000, 60782668, 16856620, 64100000)
year.ed<-cbind(year.ed, populations)

write.table(daysexp.m, file="data/daysexp.m.csv", sep=",")
write.table(daysexp.a, file="data/daysexp.a.csv", sep=",")
write.table(expend.m, file="data/expend.m.csv", sep=",")
write.table(expend.a, file="data/expend.a.csv", sep=",")
write.table(year.ed, file="data/year.ed.csv", sep=",")
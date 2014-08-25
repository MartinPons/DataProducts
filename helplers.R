# helplers.R

## characcter -> numeric
## formats character figures in numeric getting rid off the dots that are use to separate
## thousands in spain.

getridofdots<-function(vect){
  vect<-as.character(vect)
  vect<-as.numeric(gsub("\\.", "", vect))
  vect
}

# character-> character
## returns a vector character with months in english language from characters with
## months in spanish
translate.date<-function(vect){
  meses<-c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio",
           "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
    for (mes in seq_along(meses)){
      vect<-gsub(meses[mes], month.abb[mes], vect)}
vect}
       
library(shiny)
library(shinyjs)
library(extrafont)
loadfonts(device = "win")
data1 <- read.csv("malnutrition-estimates.csv")
data2 <- read.csv("country-wise-average.csv")
data1[is.na(data1)] = 0

listCountry <-data2["Country"]
cols <- colnames(data2)
listTypeMal<-cols[3:7] 

getCountryTimeLine <-function(nameOfCountry,typeMalnut)
{
  countryTimeLine<-data1[data1$Country==nameOfCountry,]
  countryTimeLine<-countryTimeLine[,c('Year',typeMalnut)]
}


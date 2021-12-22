library(shiny)
library(shinyjs)
library(extrafont)
library(tidyverse)
library(tools)
loadfonts(device = "win")
#1 PLOT
data1 <- read.csv("malnutrition-estimates.csv")
data2 <- read.csv("country-wise-average.csv")
datos1 = data2
data1[is.na(data1)] = 0

listCountry <-data2["Country"]
cols <- colnames(data2)
listTypeMal<-cols[3:7] 

getCountryTimeLine <-function(nameOfCountry,typeMalnut)
{
  countryTimeLine<-data1[data1$Country==nameOfCountry,]
  countryTimeLine<-countryTimeLine[,c('Year',typeMalnut)]
}
#2 PLOT

datos1$sovereignt = tolower(datos1$Country)
datos1$sovereignt = unlist(lapply(datos1$sovereignt, FUN=toTitleCase))
datos1$sovereignt[datos1$sovereignt == "Bolivia (Plurinational State of)"] <- "Bolivia"
datos1$sovereignt[datos1$sovereignt == "Brunei Darussalam"] <- "Brunei"
datos1$sovereignt[datos1$sovereignt == "Cabo Verde"] <- "Cape Verde"
datos1$sovereignt[datos1$sovereignt == "Central African Republic (the)"] <- "Central African Republic"
datos1$sovereignt[datos1$sovereignt == "Comoros (the)"] <- "Comoros"
datos1$sovereignt[datos1$sovereignt == "Congo (the)"] <- "Republic of Congo"
datos1$sovereignt[datos1$sovereignt == "Cote D'ivoire"] <- "Ivory Coast"
datos1$sovereignt[datos1$sovereignt == "Czechia"] <- "Czech Republic"
datos1$sovereignt[datos1$sovereignt == "Democratic People's Rep. of Korea (the)"] <- "North Korea"
datos1$sovereignt[datos1$sovereignt == "Republic of Korea (the)"] <- "South Korea"
datos1$sovereignt[datos1$sovereignt == "Democratic Rep. of the Congo (the)"] <- "Democratic Republic of the Congo"
datos1$sovereignt[datos1$sovereignt == "Dominican Republic (the)"] <- "Dominican Republic"
datos1$sovereignt[datos1$sovereignt == "Eswatini"] <- "Swaziland"
datos1$sovereignt[datos1$sovereignt == "Gambia (the)"] <- "Gambia"
datos1$sovereignt[datos1$sovereignt == "Guinea-Bissau"] <- "Guinea Bissau"
datos1$sovereignt[datos1$sovereignt == "Iran (Islamic Republic of)"] <- "Iran"
datos1$sovereignt[datos1$sovereignt == "Lao People's Democratic Rep. (the)"] <- "Laos"
datos1$sovereignt[datos1$sovereignt == "Niger (the)"] <- "Niger"
datos1$sovereignt[datos1$sovereignt == "North Macedonia"] <- "Macedonia"
datos1$sovereignt[datos1$sovereignt == "Philippines (the)"] <- "Philippines"
datos1$sovereignt[datos1$sovereignt == "Republic of Moldova (the)"] <- "Moldova"
datos1$sovereignt[datos1$sovereignt == "Serbia"] <- "Republic of Serbia"
datos1$sovereignt[datos1$sovereignt == "State of Palestine"] <- "Israel"
datos1$sovereignt[datos1$sovereignt == "Sudan (the)"] <- "Sudan"
datos1$sovereignt[datos1$sovereignt == "Syrian Arab Republic (the)"] <- "Syria"
datos1$sovereignt[datos1$sovereignt == "Timor-Leste"] <- "East Timor"
datos1$sovereignt[datos1$sovereignt == "United Republic of Tanzania (the)"] <- "United Republic of Tanzania"
datos1$sovereignt[datos1$sovereignt == "United States of America (the)"] <- "United States of America"
datos1$sovereignt[datos1$sovereignt == "Venezuela (Bolivarian Republic of)"] <- "Venezuela"
datos1$sovereignt[datos1$sovereignt == "Viet Nam"] <- "Vietnam"

datos1 <- datos1[!(datos1$sovereignt =="Tuvalu"),]



fillVoids <-function(malf, mapdataGlobal)
{
  if(malf == "Severe.Wasting"){
    mapdataGlobal$Severe.Wasting[is.na(mapdataGlobal$Severe.Wasting)] <- 0
  }else if(malf == "Wasting"){
    mapdataGlobal$Wasting[is.na(mapdataGlobal$Wasting)] <- 0
  }else if(malf == "Overweight"){
    mapdataGlobal$Overweight[is.na(mapdataGlobal$Overweight)] <- 0
  }else if(malf == "Stunting"){
    mapdataGlobal$Stunting[is.na(mapdataGlobal$Stunting)] <- 0
  }else if (malf == "Underweight"){
    mapdataGlobal$Underweight[is.na(mapdataGlobal$Underweight)] <- 0
  }else{
    mapdataGlobal$U5.Population...000s.[is.na(mapdataGlobal$U5.Population...000s.)] <- 0
  }
  return(mapdataGlobal) 
}



#grafo 3  deads by income
#data of an income

getDataIncome<- function(income)
{
  dataIncome<-data2[,"Income.Clasification"==income]
  dataIncome<-dataIncome[,c("Country","U5 Population ('000s)")]
}


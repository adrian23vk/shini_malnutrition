library(shiny)
library(shinyjs)
library(extrafont)
library(tidyverse)
library(tools)
loadfonts(device = "win")
#1 PLOT
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
#2 PLOT

datos1  = read.csv("country-wise-average.csv", sep = ",")
datos1$region = tolower(datos1$Country)
datos1$region = unlist(lapply(datos1$region, FUN=toTitleCase))
datos1$region[datos1$region == "Bolivia (Plurinational State of)"] <- "Bolivia"
datos1$region[datos1$region == "Cote D'ivoire"] <- "Côte d'Ivoire"
datos1$region[datos1$region == "Central African Republic (the)"] <- "Central African Republic"
datos1$region[datos1$region == "Republic of Korea (the)"] <- "Republic of Korea"
datos1$region[datos1$region == "Syrian Arab Republic (the)"] <- "Syria"
datos1$region[datos1$region == "Philippines (the)"] <- "Philippines"
datos1$region[datos1$region == "United Republic of Tanzania (the)"] <- "Tanzania"
datos1$region[datos1$region == "United States of America (the)"] <- "United States"
datos1$region[datos1$region == "Venezuela (Bolivarian Republic of)"] <- "Venezuela"
datos1$region[datos1$region == "Niger (the)"] <- "Niger"
datos1$region[datos1$region == "Congo (the)"] <- "Republic of the Congo"
datos1$region[datos1$region == "State of Palestine"] <- "Palestine"
datos1$region[datos1$region == "Viet Nam"] <- "Vietnam"
datos1$region[datos1$region == "Czechia"] <- "Czech Republic"
datos1$region[datos1$region == "Democratic Rep. of the Congo (the)"] <- "Democratic Republic of the Congo"
datos1$region[datos1$region == "Sudan (the)"] <- "Sudan"
datos1$region[datos1$region == "Dominican Republic (the)"] <- "Dominican Republic"
datos1$region[datos1$region == "Gambia (the)"] <- "The Gambia"
datos1$region[datos1$region == "Lao People's Democratic Rep. (the)"] <- "Lao PDR"
datos1$region[datos1$region == "Democratic People's Rep. of Korea (the)"] <- "Dem. Rep. Korea"
datos1$region[datos1$region == "Iran (Islamic Republic of)"] <- "Iran"
datos1$region[datos1$region == "Republic of Moldova (the)"] <- "Moldova"
datos1$region[datos1$region == "North Macedonia"] <- "Macedonia"

datos1 <- datos1[!(datos1$region =="Bahrain"),]
datos1 <- datos1[!(datos1$region =="Barbados"),]
datos1 <- datos1[!(datos1$region =="Cabo Verde"),]
datos1 <- datos1[!(datos1$region =="Comoros (the)"),]
datos1 <- datos1[!(datos1$region =="Kiribati"),]
datos1 <- datos1[!(datos1$region =="Maldives"),]
datos1 <- datos1[!(datos1$region =="Marshall Islands"),]
datos1 <- datos1[!(datos1$region =="Mauritius"),]
datos1 <- datos1[!(datos1$region =="Nauru"),]
datos1 <- datos1[!(datos1$region =="Saint Lucia"),]
datos1 <- datos1[!(datos1$region =="Samoa"),]
datos1 <- datos1[!(datos1$region =="Seychelles"),]
datos1 <- datos1[!(datos1$region =="Singapore"),]
datos1 <- datos1[!(datos1$region =="Tonga"),]


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

library(shiny)
library(shinyjs)
library(extrafont)
library(tidyverse)
library(tools)
library(shinycssloaders)


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

datos1$admin = tolower(datos1$Country)
datos1$admin = unlist(lapply(datos1$admin, FUN=toTitleCase))
datos1$admin[datos1$admin == "Bolivia (Plurinational State of)"] <- "Bolivia"
datos1$admin[datos1$admin == "Brunei Darussalam"] <- "Brunei"
datos1$admin[datos1$admin == "Cabo Verde"] <- "Cape Verde"
datos1$admin[datos1$admin == "Central African Republic (the)"] <- "Central African Republic"
datos1$admin[datos1$admin == "Comoros (the)"] <- "Comoros"
datos1$admin[datos1$admin == "Congo (the)"] <- "Republic of Congo"
datos1$admin[datos1$admin == "Cote D'ivoire"] <- "Ivory Coast"
datos1$admin[datos1$admin == "Czechia"] <- "Czech Republic"
datos1$admin[datos1$admin == "Democratic People's Rep. of Korea (the)"] <- "North Korea"
datos1$admin[datos1$admin == "Republic of Korea (the)"] <- "South Korea"
datos1$admin[datos1$admin == "Democratic Rep. of the Congo (the)"] <- "Democratic Republic of the Congo"
datos1$admin[datos1$admin == "Dominican Republic (the)"] <- "Dominican Republic"
datos1$admin[datos1$admin == "Eswatini"] <- "Swaziland"
datos1$admin[datos1$admin == "Gambia (the)"] <- "Gambia"
datos1$admin[datos1$admin == "Guinea-Bissau"] <- "Guinea Bissau"
datos1$admin[datos1$admin == "Iran (Islamic Republic of)"] <- "Iran"
datos1$admin[datos1$admin == "Lao People's Democratic Rep. (the)"] <- "Laos"
datos1$admin[datos1$admin == "Niger (the)"] <- "Niger"
datos1$admin[datos1$admin == "North Macedonia"] <- "Macedonia"
datos1$admin[datos1$admin == "Philippines (the)"] <- "Philippines"
datos1$admin[datos1$admin == "Republic of Moldova (the)"] <- "Moldova"
datos1$admin[datos1$admin == "Serbia"] <- "Republic of Serbia"
datos1$admin[datos1$admin == "State of Palestine"] <- "Israel"
datos1$admin[datos1$admin == "Sudan (the)"] <- "Sudan"
datos1$admin[datos1$admin == "Syrian Arab Republic (the)"] <- "Syria"
datos1$admin[datos1$admin == "Timor-Leste"] <- "East Timor"
datos1$admin[datos1$admin == "United Republic of Tanzania (the)"] <- "United Republic of Tanzania"
datos1$admin[datos1$admin == "United States of America (the)"] <- "United States of America"
datos1$admin[datos1$admin == "Venezuela (Bolivarian Republic of)"] <- "Venezuela"
datos1$admin[datos1$admin == "Viet Nam"] <- "Vietnam"
datos1 <- datos1[!(datos1$admin =="Tuvalu"),]





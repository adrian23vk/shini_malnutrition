library(shiny)
library(shinyjs)
library(extrafont)
library(tidyverse)
library(tools)
library(shinycssloaders)
library(factoextra)
library("VennDiagram") 
library(DT) 

library(devtools)
library(chorddiag)


#1 PLOT
data1 <- read.csv("malnutrition-estimates.csv")
data2 <- read.csv("country-wise-average.csv")

datos1 = data1
library(dplyr)
datos1 = datos1 %>% 
  group_by(Country) %>% 
  filter(Year==max(Year))

datos3 = data2
# parte 1
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


#PLOT 3

selectedCols = data2
selectedCols <- selectedCols[,c(2,3,4,5,6,7,8)] 
reqCols <- colnames(selectedCols)

#REGRESSION PLOT 

encode_ordinal <- function(x, order = unique(x)) {
  x <- as.numeric(factor(x, levels = order, exclude = NULL))
  x
}
allData <- read.csv(file="malnutrition-estimates.csv")
allData[["Country_encoded"]] <- encode_ordinal(allData[["Country"]])
allData[["Year_encoded"]] <- encode_ordinal(allData[["Year"]])
allData[is.na(allData)] <- 0
trainingData <- allData[, c(6,7,8,9,10,20,21,22)]
trainingData[] <- lapply(trainingData, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})
sapply(trainingData, class)
trainingData$Survey.Sample..N. <- as.numeric(gsub(",","",trainingData$Survey.Sample..N.))
trainingData[is.na(trainingData)] <- 0
allCols <- colnames(allData)
yCols <- allCols[11:15]

#3º
incomes= c('Low income','Lower middle income', 'Upper middle income', 'High income')
health= c('Bad health','Lower middle health', 'Upper middle health', 'Good health')


f=datos3[,c(3,4,5,6,7)] 
f$Severe.Wasting[is.na(f$Severe.Wasting)]<-mean(f$Severe.Wasting,na.rm = TRUE)
f$Wasting[is.na(f$Wasting)]<-mean(f$Wasting, na.rm = TRUE)
f$Overweight[is.na(f$Overweight)]<-mean(f$Overweight, na.rm = TRUE)
f$Stunting[is.na(f$Stunting)]<-mean(f$Stunting, na.rm = TRUE)
f$Underweight[is.na(f$Underweight)]<-mean(f$Underweight, na.rm = TRUE)

fviz_nbclust(f, FUNcluster = kmeans  ) 

km = kmeans(x = f, centers = 4) 

fviz_cluster(km, data = f, 
             
             geom = "point", 
             ellipse.type = "convex",  
             ggtheme = theme_bw() 
) 

columnKmeans=as.data.frame(km$cluster) 
colnames(columnKmeans) <- c('ori')
dataKmeans= as.data.frame(km$centers) 
dataKmeans$index = c(1,2,3,4)
orderedCenters= dataKmeans[order(dataKmeans$Severe.Wasting),] 
orderedCenters$order = rev(health)
columnKmeans$ord = columnKmeans
columnKmeans$ord[columnKmeans$ord==1] <- orderedCenters$order[orderedCenters$index==1] 
columnKmeans$ord[columnKmeans$ord==2] <- orderedCenters$order[orderedCenters$index==2] 
columnKmeans$ord[columnKmeans$ord==3] <- orderedCenters$order[orderedCenters$index==3] 
columnKmeans$ord[columnKmeans$ord==4] <- orderedCenters$order[orderedCenters$index==4] 

datos3$heal = columnKmeans$ord


library("ggvenn")
getVenn <- function(incomeX, health)
{
  
  income=traductorIncome(incomeX)
  
  lista = list(Income = datos3$Country[datos1$Income.Classification==income],Health = datos3$Country[datos3$heal==health], 
               Merged=datos3$Country[datos3$heal==health & datos3$Income.Classification==income])
  # ggvenn(data = lista,
  #        columns = c('Income','Health') , fill_alpha = 0.5, fill_color = c('deepskyblue', 'yellow'))
  
  counter= lengths(lista)
  grid.newpage()
  draw.pairwise.venn(area1 = counter[1],                        # Create pairwise venn diagram
                     area2 = counter[2],
                     cross.area = counter[3]
                     ,fill=c('deepskyblue', 'red')
                     ,lty = "blank"
                     ,category = c("Income", "Health")
                     ,cat.cex = 2
                     ,cex = 2
                     ,cat.pos = 0
                     ,print.mode = c("raw","percent"))
}
getTabla <-function(incomeX, health)
{
  income=traductorIncome(incomeX)
  
  lista = list(Income = datos3$Country[datos1$Income.Classification==income],Health = datos3$Country[datos3$heal==health], 
               Merged=datos3$Country[datos3$heal==health & datos3$Income.Classification==income])
  max_length <- max(unlist(lapply (lista, FUN = length)))
  mat <- sapply (lista, function (x) {length (x) <- max_length; return (x)})
  mat =as.matrix(mat)

  return(as.data.frame( mat))
}
traductorIncome <-function(income)
{
  switch (income,
          'Low income' = 0,
          'Lower middle income'=1,
          'Upper middle income'=2,
          'High income'=3
  )
}


#plot corr

g<-data2[,c(3,4,5,6,7)]

g$Severe.Wasting[is.na(g$Severe.Wasting)]<-mean(g$Severe.Wasting,na.rm = TRUE)
g$Wasting[is.na(g$Wasting)]<-mean(g$Wasting, na.rm = TRUE)
g$Overweight[is.na(g$Overweight)]<-mean(g$Overweight, na.rm = TRUE)
g$Stunting[is.na(g$Stunting)]<-mean(g$Stunting, na.rm = TRUE)
g$Underweight[is.na(g$Underweight)]<-mean(g$Underweight, na.rm = TRUE)


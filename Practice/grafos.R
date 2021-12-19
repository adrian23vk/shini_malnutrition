datos1  = read.csv("csv/country-wise-average.csv", sep = ",")
datos2  = read.csv("csv/malnutrition-estimates.csv", sep = ",")


# library(sf)
# library(raster)
# #> Warning: multiple methods tables found for 'approxNA'
# library(dplyr)
# library(spData)
# #install.packages("spDataLarge", repos = "https://nowosad.r-universe.dev")
# library(spDataLarge)
# 
# library(tmap)    # for static and interactive maps
# library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package
library(tidyverse)
 
# map = tm_shape(world) + tm_raster(col = datos1$Severe.Wasting)
# class(map)
# map
# a =world$name_long
# 
# x <- c(datos1$Country ,datos1$Severe.Wasting)
# x
# x[is.na(x$Severe.Wasting)] <- 0
# normalized = (x-min(x))/(max(x)-min(x))
# normalized
# map = tm_shape(world) + tm_fill(col = normali)
# class(map)
# map



library(tools)
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



mapdata <-map_data("world")

mapdata <- left_join(mapdata,datos1, by = "region")

mapdata$Severe.Wasting[is.na(mapdata$Severe.Wasting)] <- 0
mapdata$Wasting[is.na(mapdata$Wasting)] <- 0
mapdata$Overweight[is.na(mapdata$Overweight)] <- 0
mapdata$Stunting[is.na(mapdata$Stunting)] <- 0
mapdata$Underweight[is.na(mapdata$Underweight)] <- 0
mapdata$U5.Population...000s.[is.na(mapdata$U5.Population...000s.)] <- 0

map1 <- ggplot(mapdata, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Severe.Wasting), color = "black")


map1 <- map1 + scale_fill_gradient(name = "Porcentaje de malnutricion", low = "grey50", high = "red") + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))



map1




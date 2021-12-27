dato1  = read.csv("country-wise-average.csv", sep = ",")
datos2  = read.csv("malnutrition-estimates.csv", sep = ",")

library(XML)
library(scales)
library(plyr)
library(maps)
library("rnaturalearth")
library("rnaturalearthdata")
library(sf)
library(tmap)    # for static and interactive maps
library(ggplot2) # tidyverse data visualization package
library(tidyverse)

library(tidyquery)




library(dplyr)
datos1 = datos2 %>% 
  group_by(Country) %>% 
  filter(Year==max(Year))

library(tools)
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


datosred = datos1[,c("sovereignt", "Severe.Wasting", "Country")]


library(maps)
library(leaflet)



world1 <- ne_countries(scale = "medium", returnclass = "sf")
world1 <- left_join(world1,datosred, by = "sovereignt")


selected = world1[c("Severe.Wasting")]
st_geometry(selected) <- NULL


pal <- colorBin("YlOrRd", domain = as.numeric(unlist(selected)) , na.color = "gainsboro")

labels <- sprintf(
  "<strong>%s</strong><br/>%g&#37",
  world1$sovereignt, as.numeric(unlist(selected))
) %>% lapply(htmltools::HTML)


leaflet(world1) %>%
  addTiles() %>%  addPolygons(
    layerId = world1$Country,
    fillColor = ~pal(as.numeric(unlist(selected)) ),
    weight = 1,
    opacity = 1,
    color = "grey",
    dashArray = "3",
    fillOpacity = 0.7,
    popup = paste(actionButton(inputId = "idButton", label = paste("View temporal flow"), 
                               onclick = 'Shiny.setInputValue(\"button_click\", this.id, {priority: \"event\"})')),
    highlightOptions = highlightOptions(
      weight = 1,
      color = "black",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  addLegend(pal = pal, values = as.numeric(unlist(selected)) , opacity = 0.7, title = paste("Percentage of "),
            position = "bottomright")




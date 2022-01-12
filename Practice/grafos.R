datos1  = read.csv("Practice/country-wise-average.csv", sep = ",")
datos2  = read.csv("Practice/malnutrition-estimates.csv", sep = ",")


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
library(factoextra)
f=datos1[,c(3,4,5,6,7)]
f <- f[!is.na(f[1]),]
f <- f[!is.na(f[2]),]
f <- f[!is.na(f[3]),]
f <- f[!is.na(f[4]),]
f <- f[!is.na(f[5]),]

fviz_nbclust(f, FUNcluster = kmeans  )

km = kmeans(x = f, centers = 4)

fviz_cluster(km, data = f,
              
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)
km$centers
km$size
columnKmeans=km$cluster
dataKmeans= as.data.frame(km$centers)
orderedData= dataKmeans[order(dataKmeans$Severe.Wasting),]
rownames= row.names(orderedData)
dataKmeans$kmean=columnKmeans

for (i in rownames)
{
  for (j in f)
  {
    
  }
}

km$totss
f$kmean=columnKmeans



library(tools)
datos1$region = tolower(datos1$Country)
datos1$region = unlist(lapply(datos1$region, FUN=toTitleCase))
datos1$region[datos1$region == "Bolivia (Plurinational State of)"] <- "Bolivia"
datos1$region[datos1$region == "Cote D'ivoire"] <- "C?te d'Ivoire"
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



getDataIncome<- function(income)
{
  dataIncome<-datos1[,"Income.Clasification"==income]
  dataIncome<-dataIncome[,c("Country","U5 Population.1000")]
}
dato0= getDataIncome(0)


dataIncome<-datos1[datos1$Income.Classification==0,]
dataIncome<-dataIncome[,c("Country","U5.Population.1000")]
dataIncome <- tibble::rowid_to_column(dataIncome, "id")


# ----- This section prepare a dataframe for labels ---- #
# Get the name and the y position of each label
label_data <- dataIncome

# calculate the ANGLE of the labels
number_of_bar <- nrow(label_data)
angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
label_data$hjust<-ifelse( angle < -90, 1, 0)

# flip angle BY to make them readable
label_data$angle<-ifelse(angle < -90, angle+180, angle)
# ----- ------------------------------------------- ---- #


# Start the plot
p <- ggplot(dataIncome, aes(x=as.factor(id), y=U5.Population.1000)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  # This add the bars with a blue color
  geom_bar(stat="identity", fill=alpha("skyblue", 0.7)) +
  
  # Limits of the plot = very important. The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
  ylim(-100,120) +
  
  # Custom the theme: no axis title and no cartesian grid
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm")      # Adjust the margin to make in sort labels are not truncated!
  ) +
  
  # This makes the coordinate polar instead of cartesian.
  coord_polar(start = 0) +
  
  # Add the labels, using the label_data dataframe that we have created before
  geom_text(data=label_data, aes(x=id, y=U5.Population.1000+10, label=Country, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 









getCountryTimeLine <-function(nameOfCountry,typeMalnut)
{
  countryTimeLine<-data1[data1$Country==nameOfCountry,]
  countryTimeLine<-countryTimeLine[,c('Year',typeMalnut)]
}


dataCountryMalnut=getCountryTimeLine('AFGHANISTAN' ,'Severe.Wasting')
dataCountryMalnut2=getCountryTimeLine( 'ALGERIA','Severe.Wasting')

xx    <- dataCountryMalnut[,1]
yy <- dataCountryMalnut[,2]

  xx2    <- dataCountryMalnut2[,1]
  yy2 <- dataCountryMalnut2[,2]



  dataCountryMalnut$Group = 'AFGHANISTAN'
  dataCountryMalnut2$Group = 'ALGERIA'
  dataPlot = merge(dataCountryMalnut,dataCountryMalnut2, no.dups = FALSE,all = TRUE )
  country1Plot <-ggplot(dataPlot, aes(x=Year, y=Severe.Wasting)) +
    geom_line(aes(colour=Group)) +
    xlab('Year')+
    ylab('Severe.Wasting') +
    geom_point(aes( color=Group),size =6)+
    theme_ipsum(axis_title_size=15) 
  country1Plot
  

  

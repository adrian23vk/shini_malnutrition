data1 <- read.csv("Practice/malnutrition-estimates.csv")
data2 <- read.csv("Practice/country-wise-average.csv")
library(factoextra) 
datos1 = data2
f=datos1[,c(3,4,5,6,7)] 
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
km$centers 
km$size 
columnKmeans=as.data.frame(km$cluster) 
colnames(columnKmeans) <- c('ori')
dataKmeans= as.data.frame(km$centers) 
dataKmeans$index = c(1,2,3,4)
orderedCenters= dataKmeans[order(dataKmeans$Severe.Wasting),] 
orderedCenters$order = c('a','b','c','d')
columnKmeans$ord = columnKmeans
columnKmeans$ord[columnKmeans$ord==1] <- orderedCenters$order[orderedCenters$index==1] 
columnKmeans$ord[columnKmeans$ord==2] <- orderedCenters$order[orderedCenters$index==2] 
columnKmeans$ord[columnKmeans$ord==3] <- orderedCenters$order[orderedCenters$index==3] 
columnKmeans$ord[columnKmeans$ord==4] <- orderedCenters$order[orderedCenters$index==4] 

datos1$heal = columnKmeans$ord


library("ggvenn")

subs = as.data.frame(datos1$Country[datos1$Income.Classification==1])
subs$heal =  as.data.frame(datos1$Country[datos1$heal=='d'])
colnames(subs) = c('inc','heal')

lista = list(Income = datos1$Country[datos1$Income.Classification==1],Health = datos1$Country[datos1$heal=='d'], 
             Merged=datos1$Country[datos1$heal=='d' & datos1$Income.Classification==1])
#ggvenn(data = lista,
#            columns = c('A','B') , fill_alpha = 0.5, fill_color = c('deepskyblue', 'yellow'))

counter= lengths(lista)
grid.newpage()
draw.pairwise.venn(area1 = counter[1],                        # Create pairwise venn diagram
                   area2 = counter[2],
                   cross.area = counter[3]
                   ,fill=c('deepskyblue', 'yellow')
                   ,lty = "blank"
                  ,category = c("Income", "Health")
      
)

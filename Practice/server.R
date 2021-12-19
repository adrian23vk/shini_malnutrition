library(shiny)
library(ggplot2)
library(hrbrthemes)
library(gridExtra)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  shinyjs::hide(id = "Country2")
  country <- reactive({
    input$Country
  })
  country2 <- reactive({
    input$Country2
  })
  malnut <- reactive({
    input$MalnutritionType
  })
  
  malnut2 <- reactive({
    input$MalnutritionType2
  })

  compare<-reactive({
    input$Compare
  })
  
  observeEvent(input$Compare, {
    
    if(input$Compare %% 2 == 1){
      shinyjs::show(id = "Country2")
    }else{
      shinyjs::hide(id = "Country2")
    }
    

  })
  
  observe({
    
  output$plot <- renderPlot({
    dataCountryMalnut=getCountryTimeLine( country(),malnut())
    xx    <- dataCountryMalnut[,1]
    yy <- dataCountryMalnut[,2]
    if (compare()){
      print('nuevos datos')
      dataCountryMalnut2=getCountryTimeLine( country2(),malnut())
      xx2    <- dataCountryMalnut2[,1]
      yy2 <- dataCountryMalnut2[,2]
    }

    
    # draw the histogram with the specified number of bins
    #plotly(data= dataCountryMalnut , x = ~Year, y = ~malnut() ,mode = 'lines')

    #plot(x=xx, y=yy, xlab='Year', ylab=malnut(),type='l')
    
    
      if (compare())
      {

        country1Plot <-ggplot(dataCountryMalnut, aes(x=xx, y=yy)) +
          geom_line(color="blue") +
          xlab('Year')+
          ylab(malnut()) +
          geom_point(shape=21, color="black", fill="#69b3a2", size=6)+
          theme_ipsum(axis_title_size=15) 
        country2Plot <-ggplot(dataCountryMalnut2, aes(x=xx2, y=yy2)) +
          geom_line(color="red") +
          xlab('Year')+
          ylab(malnut()) +
          geom_point(shape=21, color="black", fill="#69b3a2", size=6)+
          theme_ipsum(axis_title_size=15) 
        grid.arrange(country1Plot,country2Plot,nrow=2)
        
    }
    else{

      ggplot(dataCountryMalnut, aes(x=xx, y=yy)) +
        geom_line(color="blue") +
        xlab('Year')+
        ylab(malnut()) +
        geom_point(shape=21, color="black", fill="#69b3a2", size=6)+
        theme_ipsum(axis_title_size=15) 
    }
    })
  })
  
  observe({
  output$mapplot <- renderPlot({

    misdatos = datos1
    MalnutritionData = misdatos[,c("region", malnut2())]
      
    mapdata <-map_data("world")
    mapdata <- left_join(mapdata, MalnutritionData, by = "region")
    
    mapdata2 <- fillVoids(malnut2(), mapdata)

    if(malnut2() == "Severe.Wasting"){
      
      map1 <- ggplot(mapdata2, aes(x = long, y = lat, group = group )) +
        geom_polygon(aes(fill =Severe.Wasting ), color = "black")
      color = "red"
    }else if(malnut2() == "Wasting"){
      
      map1 <- ggplot(mapdata2, aes(x = long, y = lat, group = group )) +
        geom_polygon(aes(fill =Wasting ), color = "black")
      color = "blue"
    }else if(malnut2() == "Overweight"){
      
      map1 <- ggplot(mapdata2, aes(x = long, y = lat, group = group )) +
        geom_polygon(aes(fill =Overweight ), color = "black")
      color = "green"
    }else if(malnut2() == "Stunting"){
      
      map1 <- ggplot(mapdata2, aes(x = long, y = lat, group = group )) +
        geom_polygon(aes(fill =Stunting ), color = "black")
      color = "pink"
    }else if (malnut2() == "Underweight"){
      map1 <- ggplot(mapdata2, aes(x = long, y = lat, group = group )) +
        geom_polygon(aes(fill =Underweight ), color = "black")
      color = "purple"
    }

    
    map1 <- map1 + scale_fill_gradient(name = paste("Percentaje of ", malnut2()), low = "grey50", high = color) + 
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
    
      })
  

  })
}

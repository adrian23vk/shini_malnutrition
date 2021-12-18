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
}

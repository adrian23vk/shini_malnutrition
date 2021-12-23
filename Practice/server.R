library(shiny)
library(ggplot2)
library(hrbrthemes)
library(gridExtra)
library(scales)
library("rnaturalearth")
library("rnaturalearthdata")
library(sf)
library(tmap)  
library(XML)

library(maps)
library(leaflet)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  tam = 0
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
      dataCountryMalnut2=getCountryTimeLine( country2(),malnut())
      xx    <- dataCountryMalnut2[,1]
      yy <- dataCountryMalnut2[,2]
    }

    
    # draw the histogram with the specified number of bins
    #plotly(data= dataCountryMalnut , x = ~Year, y = ~malnut() ,mode = 'lines')

    #plot(x=xx, y=yy, xlab='Year', ylab=malnut(),type='l')
    
    
      if (compare())
      {
        dataCountryMalnut$Country = country()
        dataCountryMalnut2$Country = country2()
        dataPlot = merge(dataCountryMalnut,dataCountryMalnut2, no.dups = FALSE,all = TRUE )
        yyy =dataPlot[,2]
        country1Plot <-ggplot(dataPlot, aes(x=Year, y=yyy)) +
          geom_line(aes(colour=Country)) +
          xlab('Year')+
          ylab(malnut()) +
          geom_point(aes( color=Country),size =6)+
          #theme_minimal(base_size = 20) +
          theme(legend.position = "bottom" , legend.key = element_blank() )
        country1Plot
        
    }
    else{
      dataCountryMalnut$Country = country()
      
      ggplot(dataCountryMalnut, aes(x=xx, y=yy)) +
        geom_line(aes(colour=Country)) +
        xlab('Year')+
        ylab(malnut()) +
        geom_point(aes( color=Country),size=6)+
        # theme_minimal(base_size = 20) +
        theme(legend.position = "bottom", legend.key = element_blank()  )
    }
    })
  })
  
  observe({
  output$mapplot <- renderLeaflet({

    
    datosred = datos1[,c("sovereignt", malnut2(), "Country")]
    
    
    world1 <- ne_countries(scale = "medium", returnclass = "sf")
    world1 <- left_join(world1,datosred, by = "sovereignt")

    selected = world1[c(malnut2())]
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
      addLegend(pal = pal, values = as.numeric(unlist(selected)) , opacity = 0.7, title = paste("Percentage of ", malnut2()),
                position = "bottomright")
    
    
    
      })
 
  observeEvent(input$button_click, {
    
    
    id <- input$mapplot_shape_click$id
    updateSelectInput(session, "Country", selected = id)
    
    updateSelectInput(session, "MalnutritionType", selected = malnut2())
    updateCheckboxInput(session,"Compare", value = FALSE)
    updateTabsetPanel(session, "panels",selected = "Malnutrition around the world")
  })

  })
}

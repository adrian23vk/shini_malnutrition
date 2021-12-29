
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

library(GGally)
library(corrgram)

library(mlbench)
library(caret)
library(dotwhisker)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
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
  
  model <- reactive({
    y = input$Y
    lab <- allData[,c(y)]
    fit <- lm(lab ~ ., data=trainingData)
  })
  
  
  health <-reactive({
    
    input$Health
  })
  income <-reactive({

    input$Income
  })
  minCorr <-reactive({
    
    input$levelCorr
  })
  
  var1 <- reactive({
    input$variable1
  })
  
  var2 <- reactive({
    input$variable2
  })
  
  var3 <- reactive({
    input$variable3
  })
  
  
  observeEvent(input$Compare, {
    
    if(input$Compare %% 2 == 1){
      shinyjs::show(id = "Country2")
    }else{
      shinyjs::hide(id = "Country2")
    }
    

  })
  
  
  observeEvent(input$tabCorr, {
    
    if(input$tabCorr == "Global Correlations"){
      shinyjs::show(id = "levelCorr")
      shinyjs::hide(id = "variable1")
      shinyjs::hide(id = "variable2")
      shinyjs::hide(id = "variable3")

      
    }else{
      shinyjs::hide(id = "levelCorr")
      shinyjs::show(id = "variable1")
      shinyjs::show(id = "variable2")
      shinyjs::show(id = "variable3")
    }
    
    
  })
  #1º
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
  #2º
  observe({
  output$mapplot <- renderLeaflet({

    
    datosred = datos1[,c("admin", malnut2(), "Country")]
    
    
    world1 <- ne_countries(scale = "medium", returnclass = "sf")
    world1 <- left_join(world1,datosred, by = "admin")

    selected = world1[c(malnut2())]
    st_geometry(selected) <- NULL
    
    pal <- colorBin("YlOrRd", domain = as.numeric(unlist(selected)) , na.color = "gainsboro")
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g&#37",
      world1$admin, as.numeric(unlist(selected)) 
    ) %>% lapply(htmltools::HTML)
    
    
    leaflet(world1 , options = leafletOptions(minZoom = 2)) %>% 
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
    datosred = datos1[,c("admin", malnut2(), "Country")]
    
    world1 <- ne_countries(scale = "medium", returnclass = "sf")
    world1 <- left_join(world1,datosred, by = "admin")
    selected2 = as.data.frame(world1[c(malnut2(), 'Country')])
    check = as.data.frame(selected2[,1:2])
    check2= as.list(check[,2]==id);
    valores = check[check2==TRUE,]
    valores <- valores[!is.na(valores[1]),]
    valores <- valores[!is.na(valores[2]),]
    if(dim(valores)[1] != 0L){
      updateSelectInput(session, "Country", selected = id)
      updateSelectInput(session, "MalnutritionType", selected = malnut2())
      updateCheckboxInput(session,"Compare", value = FALSE)
      updateTabsetPanel(session, "panels",selected = "Malnutrition around the world")
    }

  })

  })
  
  #HamnaPlot
  observe({
    output$plotChord <- renderChorddiag({

       matriz<-cor(g)
       matrizAbs<-abs(matriz)
       matrizAbs[matrizAbs<as.numeric(minCorr())]=0
       dimnames(matrizAbs)<-list(cor1= c("Severe.Wasting", "Wasting", "Overweight", "Stunting", "Underweight"), cor2=c("Severe.Wasting", "Wasting", "Overweight", "Stunting", "Underweight"))
       matrizAbs
       chorddiag::chorddiag(data= matrizAbs,groupnameFontsize = 14)

    })

  })
  observe({
    output$corrplot <- renderPlot({
      df <- selectedCols[, c(var1(), var2(), var3(), 'U5.Population.1000')]
      df[is.na(df)] <- mean(df)
      
     plott = ggplot(df, aes(x=df[,1], y = df[,4], size = df[,2] , color = df[,3]))  +
        geom_point(alpha=0.7)
     plott + labs(colour = var3(), x = var1(), y = 'U5.Population.1000', size = var2())
     # ggpairs(df)
    })

    output$colorcorr <- renderPlot({
      df <- selectedCols[, c(var1(),var2(), var3(), 'U5.Population.1000')]
      ggcorr(df, low = "#3B9AB2", mid = "lightgrey", high = "#F21A00", nbreaks = 15)
    })

  })
  
  #Regression Plot
  observe({
    output$summary <- renderPrint({
      summary(model())
      
    })
    output$lrPlot <- renderPlot({
      plot(model(), which=1)
    })
    
    output$qqPlot <- renderPlot({
      plot(model(), which=2)
    })
    
    output$densityPlot <- renderPlot({
      res <- resid(model())
      plot(density(res))
    })
    
    output$coffPlot <- renderPlot({
      dwplot(model())
    })
    

  })

  
  #3º
  observe({
    output$venn <- renderPlot({
      getVenn(income(),health())
      
    })
    output$tabla<- DT::renderDataTable({
      table= getTabla(income(),health())
      DT::datatable(table) %>% formatStyle(
        'Income', backgroundColor = '#BBFAEF'
        ) %>% formatStyle(
        'Health', backgroundColor = '#FAF3B9'
      ) %>% formatStyle(
        'Intersection', backgroundColor = '#C4FAB8'
      )
    })

    
    
  })
  
  
  
  
  
  
  
}


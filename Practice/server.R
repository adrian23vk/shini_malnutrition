

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
  compare2<-reactive({
    input$Compare2
  })
  
  model <- reactive({
    
    y = input$Y
    traducido = traductorIncome(y)
    t=trainingData
    my_data = trainingData[trainingData$Income.Classification == traducido,]
    lab <- my_data[,c('U5.Population.1000')]
    my_data= my_data[, c('Severe.Wasting', 'Wasting', 'Stunting')]

    fit <- lm(lab ~ ., data=my_data)
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
  
  var1_2 <- reactive({
    input$variable1
  })
  
  var2_2 <- reactive({
    input$variable2
  })

  
  var1_3<- reactive({
    input$variable1_2
  })
  var2_3<- reactive({
    input$variable2_2
  })
  var3_3<- reactive({
    input$variable3_2
  })
  

  observeEvent(input$Compare, {
    
    if(input$Compare %% 2 == 1){
      shinyjs::show(id = "Country2")
    }else{
      shinyjs::hide(id = "Country2")
    }
    

  })
  
  
  observeEvent(input$Compare2, {
    
    if(input$Compare2 %% 2 == 1){
      shinyjs::show(id = "variable3_2")
    }else{
      shinyjs::hide(id = "variable3_2")
    }
    
    
  })
  
  
  observeEvent(input$tabCorr, {
    
    if(input$tabCorr == "Global Correlation"){
      shinyjs::show(id = "levelCorr")
      shinyjs::hide(id = "variable1")
      shinyjs::hide(id = "variable2")

    }else if(input$tabCorr == "Linear Correlation"){
      shinyjs::hide(id = "levelCorr")
      shinyjs::show(id = "variable1")
      shinyjs::show(id = "variable2")

    }
    
  })
  #1º
  observe({
    
  output$plot <- renderPlotly({
    dataCountryMalnut=getCountryTimeLine( country(),malnut())
    dataCountryMalnut=na.omit(dataCountryMalnut)
    row.names(dataCountryMalnut) <- NULL
    
    Year    <- dataCountryMalnut[,1]
    Value <- dataCountryMalnut[,2]
    if (compare()){
      dataCountryMalnut2=getCountryTimeLine( country2(),malnut())
      dataCountryMalnut2=na.omit(dataCountryMalnut2)
      row.names(dataCountryMalnut2) <- NULL
      Year    <- dataCountryMalnut2[,1]
      Value <- dataCountryMalnut2[,2]
    }

    
    # draw the histogram with the specified number of bins
    #plotly(data= dataCountryMalnut , x = ~Year, y = ~malnut() ,mode = 'lines')

    #plot(x=xx, y=yy, xlab='Year', ylab=malnut(),type='l')
    
    
      if (compare())
      {
        dataCountryMalnut$Country = country()
        dataCountryMalnut2$Country = country2()
        dataPlot = merge(dataCountryMalnut2,dataCountryMalnut, no.dups = FALSE,all = TRUE )
        Value =dataPlot[,2]
        country1Plot <-ggplot(dataPlot, aes(x=Year, y=Value)) +
          geom_line(aes(colour=Country)) +
          xlab('Year')+
          ylab(malnut()) +
          geom_point(aes( color=Country),size =6)+
          theme(legend.position = "bottom" , legend.key = element_blank() )
        plotCountries= ggplotly(country1Plot)
        plotCountries %>% layout(legend = list(orientation = 'h'))
        
    }
    else{
      dataCountryMalnut$Country = country()
      
      plotCrountry2=ggplot(dataCountryMalnut, aes(x=Year, y=Value)) +
        geom_line(aes(colour=Country)) +
        xlab('Year')+
        ylab(malnut()) +
        geom_point(aes( color=Country),size=6)+
        theme(legend.position = "bottom", legend.key = element_blank()  )
     plotCountries= ggplotly(plotCrountry2)
     plotCountries %>% layout(legend = list(orientation = 'h'))
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
      updateTabsetPanel(session, "panels",selected = "Temporal evolution of malnutrition")
    }

  })
  


})

 
  observe({
    output$plotHeat <- renderGirafe({
      dataCorr=trainingData[,c(1,2,3,4,5)]
      matrizAbs<-cor(dataCorr)
      rangeOfCorr=minCorr()
      val1=rangeOfCorr[1]
      val2=rangeOfCorr[2] 
      matrizAbs[matrizAbs<as.numeric(val1)]=0
      matrizAbs[ matrizAbs>as.numeric(val2)]=0
       matrizAbs= melt(matrizAbs)
      colnames(matrizAbs)<-c('Var1','Var2','value')
       matrizAbs$Cols=paste0(matrizAbs$Var1,'#',matrizAbs$Var2 )
       idCol=paste0(matrizAbs$Var1,'#',matrizAbs$Var2 )
       codeGGplot= ggplot(data = matrizAbs, aes(x=Var1, y=Var2, fill=value)) + 
         ylab("")+xlab("")+geom_tile_interactive(data_id=idCol,aes( tooltip=value )) +
         labs(fill = "Correlation") + scale_fill_gradient2(low = 'red',mid = "gray88", high = "royalblue",guide = 'colourbar',midpoint = 0,limits=c(-1,1))
       girafe(ggobj=codeGGplot, 
              options = list(opts_selection(type = "single", only_shiny = FALSE)))

    })
    
    observeEvent(input$plotHeat_selected, {
      
      id= input$plotHeat_selected
      types=as.data.frame(str_split(id,'#'))

      updateSelectInput(session, "variable1", selected =types[1,] )
      updateSelectInput(session, "variable2", selected =types[2,])
      updateTabsetPanel(session, "tabCorr",selected = "Linear Correlation")

      
    })


  })
  
  
  
  observe({
    output$corrplot <-renderGirafe({
      target1 = DataCopied2[,'U5.Population.1000']
      countrydf1 = DataCopied2[,'Country']
      df1 <- selectedCols1[, c(var1_3(), var2_3(), var3_3())]
      df1$U5.Population.1000 <- target1
      df1$Country <- countrydf1
      df1$Country <- str_remove_all(df1$Country,"'")
      if(compare2()){
        gg <- ggplot(df1, aes(x=df1[,4], y = df1[,1], size = df1[,2] , color = df1[,3]))  +
          geom_point_interactive(alpha=0.7, aes( tooltip = Country, data_id = Country))+ labs(colour = var3_3(), x = 'Deaths of children under 5 years of age per 1.000' , y =var1_3(), size = var2_2() ) +
          scale_size(range = c(1, 10)) +
          scale_color_gradient(low = 'turquoise', high = 'turquoise4')
      }else{
        gg <- ggplot(df1, aes(x=df1[,4], y = df1[,1] , color = df1[,2])) +
          geom_point_interactive(alpha=0.7, size = 5,  aes( tooltip = Country, data_id = Country))+ labs(colour = var2_3(),  x = 'Deaths of children under 5 years of age per 1.000' , y =var1_3()) +
          scale_color_gradient(low = 'turquoise', high = 'turquoise4')
      }
       
        
       
      giraf = girafe(ggobj  = gg,  width_svg = 12, height_svg = 6)
      giraf <- girafe_options(giraf,
                              opts_zoom(max = 20) )
      giraf <- girafe_options(giraf,
                               opts_tooltip(opacity = .7,
                                            offx = 20, offy = -10,
                                            use_fill = TRUE, use_stroke = TRUE, 
                                            delay_mouseout = 1000) )

    })
    output$lmPlot <- renderPlotly({
      plot=modelHeatMap()
      plot
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
      )%>% formatStyle(
         'Income', fontSize = '10px'
       ) %>% formatStyle(
        'Health', fontSize = '10px'
       )%>% formatStyle(
         'Intersection', fontSize = '10px'
       )
      
    })
    


    
    
  })

  modelHeatMap <- function(){
    
    x=var1()
    y=var2()
    yy=y
    
    if (x==y){
      y=paste0(y,'.1')
      
    }
    
    t=trainingData
    my_data = trainingData
    my_data= my_data[, c(x, yy)]
    dataX=my_data[, c(x)]
    dataY=my_data[, c(y)]
    names(dataX) <- c(var1())
    names(dataY) <- c(y)
    g=colnames(my_data)
    
    #model <- lm(dataX~dataY, data=my_data)
    #plotGGira=ggPredict(model,se=TRUE,interactive=TRUE, xlab='Hola')
    #giraf=girafe(print(plotGGira))
    #giraf
    
    gpl=  ggplot(my_data,aes(x=dataX,y=dataY)) +
      geom_point(alpha=0.5,color='lightseagreen') +
      labs(x= x, y=y)+
      geom_smooth(method = 'glm')
    ggplotly(gpl, tooltip = FALSE)
  }
  
  
  
  
  
  
  
  
}


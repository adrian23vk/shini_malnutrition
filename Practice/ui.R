# Define UI for application that draws a histogram
ui <- fluidPage(
  theme= bs_theme(version = 5,bootswatch = 'flatly'),
  
  useShinyjs(),
  # Application title
  titlePanel("MalNutriStats"),
  navbarPage("Menu", id = "panels",
            
             tabPanel("Geographical distribution of malnutrition",
         
                      tags$style(type = "text/css", "#mapplot {height: calc(100vh - 180px) !important;}"),
                      tags$style("
        #controls {
          opacity: 0.8;
          background-color: rgba(255,255,255,1);
        }
        #controls:hover{
          opacity: 1;
          background-color: rgba(255,255,255,1);
        }
               "),
                      leafletOutput("mapplot",height  = '100%')%>% withSpinner(),
                      
                      fixedPanel(id='controls',height = "auto",width = 300,bottom = "auto",right = 40,top = 152,
                                    class='panel panel-default',
                                  
                        
                          
                          selectInput("MalnutritionType2", "Malnutrition type",
                                      choices = listTypeMal)
                          
                        
                        ) 
                      
                      
                      
                      ,fluid = TRUE)
             ,
             tabPanel("Temporal evolution of malnutrition",
                      sidebarLayout(
                        sidebarPanel(
                          
                          selectInput("Country", "Country",
                                      choices = listCountry
                          ),
                     
                          selectInput("Country2", "Second country",
                                       choices = listCountry
                           ),
  
                          checkboxInput("Compare", "Compare", FALSE),
                          
                          selectInput("MalnutritionType", "Malnutrition type",
                                      choices = listTypeMal)
                          
                          
                        ),
                        mainPanel(
                          plotlyOutput("plot",height = 500)
                        ),
                      fluid = TRUE) 
                      
             ),
             tabPanel("Correlations among malnutrition types",
                      sidebarLayout(
                        sidebarPanel(
                          
                          selectInput("variable1", "Variable 1",
                                      choices = reqCols1
                          ),
                          
                          selectInput("variable2", "Variable 2",
                                      choices = reqCols1
                          ), 
                         
                          sliderInput("levelCorr", "Minimum correlation value", min = -1, max = 1, value = c(-1,1),step = 0.05)
                          
                        ),
                        mainPanel(


                          tabsetPanel(
                            tabPanel("Global Correlation",  ggiraph::girafeOutput('plotHeat')),
                            tabPanel("Linear Correlation", plotlyOutput("lmPlot")),

                            id ="tabCorr"),


                        ),
                        fluid = TRUE) 
                      
             ),
             tabPanel("Malnutrition vs. death of under-five-year-olds",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("variable1_2", "Variable 1",
                                    choices = reqCols1
                          ),
                          selectInput("variable2_2", "Variable 2",
                                    choices = reqCols1
                          ), 
                          checkboxInput("Compare2", "Compare 3 variables", FALSE
                          ), 
                          selectInput("variable3_2", "Variable 3",
                                    choices = reqCols1
                          )
                        
                      ),mainPanel(
                          ggiraph::girafeOutput('corrplot')
                      )
                    ), fluid = TRUE 
              ),
             
             tabPanel("Malnutrition level & Wealth",
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          selectInput("Income", "Income", choices = incomes),
                          
                          selectInput("Health", "Heath", choices = health), 
                          width = 2
                        ),
                        mainPanel(
                          fluidRow(
                              column(8,plotOutput("venn",height = 450, width = 500)),
                              column(4,DT::dataTableOutput('tabla', height = 250, width = 500))
                          )
                                  
                          ),
                        fluid = TRUE
                        
                      )
             )
   
             
    )
  
)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme= bs_theme(version = 5,bootswatch = 'flatly'),
  
  useShinyjs(),
  # Application title
  titlePanel("Malnutrition summary"),
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
             tabPanel("Malnutrition around the world",
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
                          plotOutput("plot",height = 500)
                        ),
                      fluid = TRUE) 
                      
             ),
             tabPanel("Correlograms",
                      sidebarLayout(
                        sidebarPanel(
                          
                          selectInput("variable1", "Variable 1",
                                      choices = reqCols
                          ),
                          
                          selectInput("variable2", "Variable 2",
                                      choices = reqCols
                          ), 
                          
                          selectInput("variable3", "Variable 3",
                                      choices = reqCols
                          ),
                          
                         sliderInput("levelCorr", "Minimum correlation value", min = 0, max = 1, value = 0, animate =
                                       animationOptions(interval = 100, loop = FALSE, playButton = NULL, pauseButton = NULL),)
                          
                        ),
                        mainPanel(


                          tabsetPanel(
                            tabPanel("Global Correlations",chorddiagOutput('plotChord', height = '500')),
                            tabPanel("U5 population vs others Plot", plotOutput("corrplot"),),
                            tabPanel("Correlation HeatMap", plotOutput("colorcorr")),

                            id ="tabCorr"),


                        ),
                        fluid = TRUE) 
                      
             ),
             tabPanel("Wealth related to the malnutrition level",
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          selectInput("Income", "Income", choices = incomes),
                          
                          selectInput("Health", "Heath", choices = health)
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Plot", plotOutput("venn")),
                            tabPanel("Extra",DT::dataTableOutput('tabla'))
                            
                            
                            
                          )
                          
                        ),
                        
                        
                      )
                      
             ) ,
             tabPanel("Linear Regression",
                      sidebarLayout(
                        sidebarPanel(

                          selectInput("Y", "Type of Malnutrition",
                                      choices = yCols
                          ),
                        ),

                        mainPanel(
                          tabsetPanel(
                            tabPanel("Summary", verbatimTextOutput("summary")),
                            tabPanel("Residual vs Fitted Plot", plotOutput("lrPlot")),
                            tabPanel("Q-Q Plot", plotOutput("qqPlot")),
                            tabPanel("Density Plot", plotOutput("densityPlot")),
                            tabPanel("Coefficient Plot", plotOutput("coffPlot"))
                          )
                          
                        ),
                        fluid = TRUE) 
                      
             )
             
             
    )
  
)

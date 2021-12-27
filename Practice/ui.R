library(leaflet)
# Define UI for application that draws a histogram
ui <- fluidPage(
  useShinyjs(),
  # Application title
  titlePanel("Malnutrition summary"),
  navbarPage("Menu", id = "panels",
            
             tabPanel("Geographical distribution of malnutrition",
         
                      tags$style(type = "text/css", "#mapplot {height: calc(100vh - 180px) !important;}"),
                      tags$style("
        #controls {
          opacity: 0.8;
        }
        #controls:hover{
          opacity: 1;
        }
               "),
                      leafletOutput("mapplot",height  = '100%')%>% withSpinner(),
                      
                      absolutePanel(id='controls',height = "auto",width = 300,bottom = "auto",right = 40, fixed = TRUE,top = 150,
                                    class = "panel panel-default",
                                  
                        
                          
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
                          plotOutput("plot",height = 400, width = 600)
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
                          
                          #                          selectInput("Variable3", "Variable 3",
                          #                                      choices = allCols)
                          
                          
                        ),
                        mainPanel(
                          plotOutput("corrplot"),
                          plotOutput("colorcorr")
                        ),
                        fluid = TRUE) 
                      
             ),
             tabPanel("Malnutrition vs wealth",
               
               sidebarLayout(
                 sidebarPanel(
                   
                   selectInput("Income", "Income", choices = incomes),
                 
                   selectInput("Health", "Heath", choices = health)
                 ),
                 mainPanel(
                   tabsetPanel(
                     tabPanel("Plot", plotOutput("venn")),
                     tabPanel("Extra", plotOutput("extra"))


                   )

                 ),
                 
                 
               )
             
             ) 
             
    )
  
)
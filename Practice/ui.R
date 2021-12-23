library(leaflet)
# Define UI for application that draws a histogram
ui <- fluidPage(
  useShinyjs(),
  # Application title
  titlePanel("Malnutrition summary"),
  navbarPage("Menu", id = "panels",
             tabPanel("Geographical distribution of malnutrition",
                      sidebarLayout(
                        sidebarPanel(
                          
                          selectInput("MalnutritionType2", "Malnutrition type",
                                      choices = listTypeMal)
                          
                        ),
                        mainPanel(
                          
                          leafletOutput("mapplot")%>% withSpinner(),
                          textOutput("prueba")
                          
                          
                          
                        ),
                        fluid = TRUE) 
                      
             )
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
                          plotOutput("plot")
                        ),
                      fluid = TRUE) 
                      
             )
             
             
  )
  
)
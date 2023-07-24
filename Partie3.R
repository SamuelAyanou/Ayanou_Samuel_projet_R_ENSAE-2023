#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(sp)
library(ggplot2)
library(dplyr)
library(shiny)
library(leaflet)
library(rnaturalearth)
library(rnaturalearthdata)

# Charger les données géographiques de l'Afrique de l'Ouest
ne_countries_data <- ne_countries(scale = "medium", continent = "Africa")
west_africa <- subset(ne_countries_data, subregion == "Western Africa")

# Charger les données de base en dehors de la fonction server
base <- read.csv("ACLED-Western_Africa.csv")

ui <- fluidPage(
  # titre de l'application
  titlePanel("shiny map"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "evenement",
        label = "Sélectionnez un evenement",
        choices = c(unique(base$type)),
        selected = "Protests",
        multiple = TRUE
      
      ),
      selectInput(
        inputId = "pays",
        label = "Sélectionnez un pays",
        choices = c(unique(base$pays)),
        selected = c(unique(base$pays))[sample(1:length(unique(base$pays)), 1)],
        multiple = TRUE
     
      ),
      selectInput(
        inputId = "annee",
        label = "Sélectionnez une annee",
        choices = c(unique(base$annee)),
        selected = "2023",
        multiple = TRUE
      ),
      
     
        
    ),
    # afficher un plot pour afficher la carte 
    mainPanel(
      leafletOutput(outputId = "map", width = "100%", height = "720px")
    )
  )
)

server <- function(input, output, session) {
  filtered_data <- reactive({
    subset(base, pays %in% input$pays & type %in% input$evenement & annee %in% input$annee)
  })
  
  output$map <- renderLeaflet({
    filtered_west_africa <- west_africa[west_africa$name %in% input$pays]
    
    leaflet() %>%
      addProviderTiles(providers$Stamen.Toner) %>%
      addPolygons(data = ne_countries(type = "countries", country = input$pays), fillColor = "#D1914D", color = "#D98", fillOpacity = 0.6) %>%
 
    
      addCircleMarkers(data = filtered_data(),
                       lat = ~latitude,
                       lng = ~longitude,
                       radius = 3,
                       opacity = 0.7)
  })
}

shinyApp(ui = ui, server = server)

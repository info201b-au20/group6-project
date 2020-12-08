# Load packages
library("shiny")
library("tidyverse")
library("leaflet")
library("RColorBrewer")
library("htmltools")

# Read data (set wdir to root)
data_sing <- read.csv("data/singapore_listings.csv")

# Define a server for the application

server <- function(input, output) {
  #---- Page one --------------------------------------------------------------
  
  palette_fn <- colorFactor(palette = "Dark2", domain = data_sing$room_type)
  
  output$my_map <- renderLeaflet({
    
    leaflet(data = data_sing) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = 103.851959, lat = 1.290270, zoom = 11) %>%
      addCircles(
        lat = ~latitude,
        lng = ~longitude,
        stroke = F,
        popup = ~listing_url,
        color = ~palette_fn(room_type),
        radius = 20,
        fillOpacity = 0.5
      ) %>% 
      addLegend(
        position = "bottomright",
        title = "Airbnb Listings in Singapore",
        pal = palette_fn,
        values = ~room_type,
        opacity = 1
      )
  })
  
  #---- Page two --------------------------------------------------------------
  
  
  
  #---- Page three ------------------------------------------------------------
  
}
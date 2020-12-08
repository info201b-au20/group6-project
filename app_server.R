# Load packages
library("shiny")
library("tidyverse")
library("leaflet")
library("RColorBrewer")

# Read data (set wdir to root)
data_sing <- read.csv("data/singapore_listings.csv")

# Define a server for the application

server <- function(input, output) {
  #---- Page one --------------------------------------------------------------
  
  output$my_map <- renderLeaflet({
    
    
    leaflet() %>%
      addProviderTiles("CartoDB.VoyagerLabelsUnder") %>%
      setView(lng = 103.851959, lat = 1.290270, zoom = 11) # center on sgp
  })
  
  #---- Page two --------------------------------------------------------------
  
  
  
  #---- Page three ------------------------------------------------------------
  
}
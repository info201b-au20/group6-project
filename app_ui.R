# Load packages
library("shiny")
library("tidyverse")
library("leaflet")
library("RColorBrewer")

# Read data (set wdir to root)
data_sing <- read.csv("data/singapore_listings.csv")


#---- Introduction ------------------------------------------------------------

intro <- tabPanel(
  title = tags$h4("Introduction")
)


#---- Interactive page one ----------------------------------------------------

page_one <- tabPanel(
  title = tags$h4("Interactive Map"),
  sidebarLayout(
    sidebarPanel(
    ),
    mainPanel(
      leafletOutput("my_map")
    )
  )
)

#---- Interactive page two ----------------------------------------------------



#---- Interactive page three --------------------------------------------------



#---- Summary -----------------------------------------------------------------



#------------------------------------------------------------------------------

# Define a ui for the application
ui <- navbarPage(
  title = tags$h1("Airbnb Data Exploration"),
  intro,
  page_one
)
# Load packages
library("shiny")
library("tidyverse")
library("leaflet")

# Read data (set wdir to root)
data_sing <- read.csv("data/singapore_listings.csv")


#### Introduction #############################################################

intro <- tabPanel(
  title = tags$h4("Introduction")
)


##### Interactive Page One ####################################################

# Convert price column to vector of numbers
data_sing$price <- as.numeric(gsub("[$,]", "", data_sing$price))


# Set default max slider range for better control (as opposed to the
# max price from the dataset due to heavy left skew distribution)
range_min <- min(data_sing$price)
range_max <- 500

slider_price <- sliderInput(
  inputId = "price_slider",
  label = "Listing Price ($SGD per night)",
  min = range_min,
  max = range_max,
  sep = ",",
  pre = "$",
  value = c(range_min, range_max),
  dragRange = TRUE
)

# Allow user to set new max price range
change_max_range <- textInput(
  inputId = "textbox",
  label = tags$h6("Set max slider range")
)

slider_accomodates <- sliderInput(
  inputId = "accom_slider",
  label = "Guest capacity (at least as high)",
  min = min(data_sing$accommodates),
  max = max(data_sing$accommodates),
  step = 1,
  value = 1
)

# Sort neigbhourhoods in order of num listings
sorted <- data_sing %>%
  group_by(neighbourhood_cleansed) %>%
  summarize(num_listings = n()) %>%
  arrange(desc(num_listings))

select_neigbourhood <- selectInput(
  inputId = "select",
  label = "Select  neigbourhood",
  choices = c("All", sorted$neighbourhood_cleansed)
)

checkbox_superhost <- checkboxInput(
  inputId = "checkbox",
  label = tags$strong("Superhost Listings Only")
)


# define a layout for first interactive page
page_one <- tabPanel(
  title = tags$h4("Interactive Map"),
  tags$h2("Singapore Airbnb Locations"),
  leafletOutput("my_map"),
  hr(),
  h3("Filter Options"),
  br(),
  fluidRow(
    column(4,
           slider_price,
           change_max_range,
           actionButton("button", "Change")
           ),
    column(4,
           slider_accomodates,
           ),
    column(4,
           select_neigbourhood,
           br(),
           checkbox_superhost)
  )
)



##### Interactive Page Two ####################################################



##### Interactive Page Three ##################################################



##### Summary #################################################################



#------------------------------------------------------------------------------

# Define a ui for the application
ui <- navbarPage(
  title = tags$strong("Airbnb Data Exploration"),
  intro,
  page_one
)
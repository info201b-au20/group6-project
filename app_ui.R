# Load packages
library("shiny")
library("tidyverse")
library("leaflet")

# Read data (set wdir to root)
data_sing <- read.csv("data/singapore_listings.csv")


#### Introduction #############################################################

intro <- tabPanel(
  title = tags$header("Introduction")
)


##### Interactive Page One ####################################################

# Convert price column to vector of numbers
data_sing$price <- as.numeric(gsub("[$,]", "", data_sing$price))


# Set default max slider range for better control (as opposed to the
# max price from the dataset due to heavy left skew distribution)
min_price <- min(data_sing$price)
max_price <- 500

slider_price <- sliderInput(
  inputId = "price_slider",
  label = "Listing Price ($SGD per night)",
  min = min_price,
  max = max_price,
  sep = ",",
  pre = "$",
  value = c(min_price, max_price),
  dragRange = TRUE
)

# Allow user to set new max price range
change_max_range <- textInput(
  inputId = "textbox",
  label = tags$h6("Set max price for slider")
)

slider_accomodates <- sliderInput(
  inputId = "accom_slider",
  label = "Guest capacity (equal to or higher)",
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


# Define a layout for interactive page
page_one <- tabPanel(
  title = tags$header("Interactive Map"),
  sidebarLayout(
    sidebarPanel(
      tags$h3("Filter listing options"),
      tags$hr(),
      slider_price,
      fluidRow(
        column(7,
               change_max_range
        ),
        column(5,
               br(),
               br(),
               actionButton("button", "change")
        ),
      ),
      tags$hr(),
      slider_accomodates,
      tags$hr(),
      select_neigbourhood,
      tags$hr(),
      checkbox_superhost
    ),
    mainPanel(
      tags$h2("Singapore Airbnb Locations"),
      leafletOutput("my_map")
    )
  )
)


##### Interactive Page Two ####################################################

# Define a layout for interactive page
page_two <- tabPanel(
  title = tags$header("Page 2")
)


##### Interactive Page Three ##################################################

# Define a layout for interactive page
page_three <- tabPanel(
  title = tags$header("Page 3")
)


##### Conclusion ##############################################################

conclusion <- tabPanel(
  title = tags$header("Conclusion")
)


#------------------------------------------------------------------------------

# Define a ui for the application
ui <- navbarPage(
  title = tags$strong("Airbnb Data Exploration"),
  intro,
  page_one,
  page_two,
  page_three,
  conclusion
)
# Load packages
library("shiny")
library("tidyverse")
library("leaflet")
library("plotly")

# Read data (set wdir to root)
data_sing <- read.csv("data/singapore_listings.csv")
data_chic <- read.csv("data/chicago_listings.csv")
data_bos <- read.csv("data/boston_listings.csv")

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
  label = "Filter by neigbourhood",
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
      HTML("<center><h3>Singapore Airbnb Listings (26 October, 2020)
           </h3></center>"),
      tags$style(type = "text/css",
                 "#m_sing {height: calc(100vh - 150px) !important;}"),
      leafletOutput("m_sing")
    )
  )
)


##### Interactive Page Two ####################################################

data_chic$price <- as.numeric(gsub("[$,]", "", data_chic$price))

# pulls a table of room types for radio button
room_type_list <- data_chic %>%
  group_by(room_type) %>%
  summarize(num_listings = n()) %>%
  arrange(desc(num_listings))

# pulls a table of neighbourhood in chicago for select box
neighbourhood_list <- data_chic %>%
  group_by(neighbourhood_cleansed) %>%
  summarize(num_listings = n()) %>%
  arrange(desc(num_listings))

# radio button input for room type
radio_room_type <- radioButtons(
  inputId = "room_type",
  label = "Room Type",
  choices = room_type_list$room_type
)

# select box input for neighbourhood
select_neighbourhood <- selectInput(
  inputId = "neighbourhood",
  label = "Neighbourhood",
  choices = neighbourhood_list$neighbourhood_cleansed,
  selected = 1
)

# slider range for number of guests accommodating
slider_range_accommod <- sliderInput(
  inputId = "accommodation_range",
  label = "Number of People Accommodating",
  min = 1,
  max = max(data_chic$accommodates),
  value = c(1,8),
  step = 1,
  width = "80%"
)

# slider range for price on chart
slider_range_price <- sliderInput(
  inputId = "price_range",
  label = "Price Range",
  min = min(data_chic$price),
  max = max(data_chic$price),
  value = c(0, 500),
  width = "80%"
)

# Define a layout for interactive page
page_two <- tabPanel(
  title = tags$header("Scatter Plot"),
  sidebarLayout(
    sidebarPanel(
      tags$h3("Filter listing options"),
      tags$hr(),
      select_neighbourhood,
      tags$hr(),
      radio_room_type
    ),
    mainPanel(
      plotlyOutput("chart_chic"),
      tags$h4("Adjust Chart Axis Range"),
      tags$hr(),
      slider_range_accommod,
      slider_range_price
    )
  )
)


##### Interactive Page Three ##################################################

data_bos$price <- as.numeric(gsub("[$,]", "", data_bos$price))
data_bos$host_is_superhost <- to_logical(data_bos$host_is_superhost)
data_bos$instant_bookable <- to_logical(data_bos$instant_bookable)


# slider for price range
price_slider <- sliderInput(
  inputId = "price",
  label = "Listing Price",
  min = 0,
  max = 500,
  sep = ",",
  pre = "$",
  value = c(0, 500),
  dragRange = TRUE
)

# slider for max accommodation
accommodation_size <- textInput(
  inputId = "accommodation_size",
  label = tags$h6("Accommodates"),
  value = 1
)

# slider for neighborhood filter
sort_neighbourhood <- data_bos %>% 
  group_by(neighbourhood_cleansed) %>% 
  summarise(num_listings = n()) %>% 
  arrange(desc(num_listings))
  
  
choose_neighbourhood <- selectInput(
  inputId = "neighbourhood",
  label = "Neigborhood",
  choices = c("All", sort_neighbourhood$neighbourhood_cleansed)
)

# slider for review ratings score
review_rating <- sliderInput(
  inputId = "review_rating",
  label = "Minimum Review Ratings Score",
  min = 50,
  max = 100,
  sep = ",",
  value = 50,
  dragRange = TRUE
)

# slider for superhost filter
superhost_checkbox <- checkboxInput(
  inputId = "superhost_checkbox",
  label = tags$strong("Superhost Listings Only")
)

#slider for instantly bookable filter
instantly_bookable <- checkboxInput(
  inputId = "instant_book_checkbox",
  label = tags$strong("Instantly Bookable")
)

# Define a layout for interactive page
page_three <- tabPanel(
  title = tags$header("Boston Listings"),
  sidebarLayout(
    sidebarPanel(
      tags$h3("Filter Listings"),
      tags$hr(),
      price_slider,
      tags$hr(),
      accommodation_size,
      tags$hr(),
      review_rating,
      tags$hr(),
      choose_neighbourhood,
      tags$hr(),
      superhost_checkbox,
      tags$hr(),
      instantly_bookable,
      tags$hr()
    ),
    mainPanel(
      tags$h2("Boston Airbnb Listings"),
      leafletOutput(outputId = "bos_map")
    )
  )
)

##### Conclusion ##############################################################

conclusion <- tabPanel(
  title = tags$header("Conclusion")
)


#------------------------------------------------------------------------------

# Define a ui for the application
ui <- navbarPage(
  windowTitle = ("Airbnb - Data Exploration"),
  title = tags$strong("Airbnb Data Exploration"),
  intro,
  page_one,
  page_two,
  page_three,
  conclusion
)
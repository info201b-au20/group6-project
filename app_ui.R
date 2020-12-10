# Load packages
library("shiny")
library("tidyverse")
library("leaflet")
library("plotly")

# Read data (setwd to root first)
data_sing <- read.csv("data/singapore_listings.csv")
data_chic <- read.csv("data/chicago_listings.csv")
data_bos <- read.csv("data/boston_listings.csv")

#### Introduction #############################################################

intro <- tabPanel(
  title = tags$header("Introduction"),
  mainPanel(
    h1("Statistical Data on Airbnb Listings in Different Cities"),
    img(src = "https://digital.hbs.edu/platform-digit/wp-content/uploads/sites/2/2020/04/unnamed-1-512x200.png"),
    h2("Overview"),
    p("In this report, we will analyze Airbnb listing data in different cities
      to answer multiple questions. The specific cities that we will focus on
      are Boston, Chicago, and Singapore. The data on these cities are 
      retrieved from ", a("insideairbnb.com", 
                         href = "http://insideairbnb.com/get-the-data.html" ),
      "which contains a collection of Airbnb datasets for various 
      cities/locations. This data is publicly available and is sourced from 
      Airbnb itself. Each dataset represents each city with detailed 
      information such as host acceptance rates, neighborhood, 
      longitude/latitude, host listings, amenities, etc. Our problem domain is 
      to look at how Airbnb can be useful for both hosts and consumers by 
      understanding price rates and availabilities in different cities. 
      Comparing Airbnb listing can help consumers find the best rates, while it
      can also help the hosts so that they can understand the average pricing
      rates."),
    h3("Questions To Be Answered for Boston, Singapore, and Chicago"),
    p("1. What are the different prices for a room based on the type of room
      such as a whole house, private room, shared room, or hotel rooms?"),
    p("2. What types of rooms are available in Singapore?"),
    p("3. Which neighborhoods have more listings available?"),
    p("4. What are the minimum and maximum listing price per night?"),
    p("5. What is the maximum guest capacity"),
    p("6. What is the availability of each room type in different 
      neighborhoods?"),
    p("7. What listings are available based on review rating scores?")
  )
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

min_guests <- min(data_sing$accommodates)
max_guests <- max(data_sing$accommodates)

slider_accomodates <- sliderInput(
  inputId = "accom_slider",
  label = "Maximum guest capacity",
  min = min_guests,
  max = max_guests,
  step = 1,
  value = c(min_guests, max_guests),
  dragRange = TRUE
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
      tags$h3("Map filter options"),
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
  title = tags$header("Conclusion"),
  mainPanel(
    h1("Summary Takeaways"),
    # takeaways for Singapore's page
    h3("Singapore Takeaways"),
    p("The first homestay map based on 2020 Singapore's Airbnb data provided an
      interactive page for users to select their preferred accommodation options
      based on their needs. In the map we found that the majority of Airbnb
      listings are located in southern Singapore and most offer entire home or
      apartment and private room. Most hosts could accept a
      maximum customer capacity of less than 5 people, and with the continuous
      growth of customer capacity, there are fewer and fewer listings
      that can meet user needs."),
    # takeaways for Chicago's page
    h3("Chicago Takeaways"),
    p("Chicago's scatter plot shows the relationship between Airbnb occupancy
      and price. By selecting different community and room types, users can
      intuitively understand the price distribution of a room. Majority of Chicago
      listings are on West Town, Near North Side and Lake View neighbourhoods,
      and among them Lake View has the highest unit price ($9999). By comparing
      Airbnb housing price data, both consumers and hosts can learn that the
      overall average price range in Chicago is between $40 and $2000,
      which can help users compare and find their optimal prices and help hosts
      understand about market price trends."),
    # takeaways for Boston's page
    h3("Boston Takeaways"),
    p("In the Airbnb Listing distribution map in Boston, we can know that the
      the most common housing types in downtown area is entire house and apartment;
      whereas in other areas, hosts offer more private rooms. At the same time, we
      could know that the Boston housing price distribution is around $50-$450."),
    # conclusion part
    h3("More..."),
    p("This project provides an interactive page for users (both housing hosts
      and consumers) to analyze and compare housing data in a given area in order
      to understand the market price distribution and trend of housing sources,
      so as to make their reasonable choices. However, under current pandemic,
      we should consciously maintain a safe distance between others, abide by
      and actively cooperate with the regional epidemic prevention policy, and
      strive to return to normal life as soon as possible. During the vacation
      trip, don't forget to..."),
    img(src = "https://thumbs.dreamstime.com/b/coronavirus-disease-public
        -awareness-woman-man-wearing-protective-surgical-masks-holding-stay-safe
        -healthy-signboards-vector-177404219.jpg",
        height = "50%", width = "50%", align = "right"),
  )
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

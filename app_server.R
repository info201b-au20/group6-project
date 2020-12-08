# Load packages
library("shiny")
library("tidyverse")
library("leaflet")
library("RColorBrewer")
library("batman")

# Read data (set wdir to root)
data_sing <- read.csv("data/singapore_listings.csv")


# Define a server for the application
server <- function(input, output, session) {

  ##### Interactive Page One ##################################################

  # Change max price range when button clicked
  observeEvent(input$button, {
    max <- input$textbox
    updateSliderInput(session, "price_slider", max = max)
    updateTextInput(session, "textbox", value = "") # clear input after click
  })

  # Construct a color palette (scale) based on the `room-type` column
  palette_fn <- colorFactor(palette = "Dark2", domain = data_sing$room_type)

  # Replace price column with a vector of numbers
  data_sing$price <- as.numeric(gsub("[$,]", "", data_sing$price))

  # Replace superhost column with boolean values
  data_sing$host_is_superhost <- to_logical(data_sing$host_is_superhost)

  output$my_map <- renderLeaflet({

    # Dynamic user filtering
    plot_data <- data_sing %>%
      filter(price >= input$price_slider[1] &
        price <= input$price_slider[2]) %>%
      filter(accommodates >= input$accom_slider) %>%
      filter(if (input$checkbox == TRUE) host_is_superhost == TRUE
             else id == id) %>%
      filter(if (input$select == "All") id == id
             else neighbourhood_cleansed == input$select)

    # Return the count of filtered listings
    filter_count <- nrow(plot_data)

    # Create Leaflet map of user-filtered Singapore listings
    leaflet(data = plot_data) %>%
      addTiles(
        urlTemplate = paste0("https://tile.jawg.io/ba3f805c-04fb-4fa7-99ef-b9",
        "05aa38b3c8/{z}/{x}/{y}.png?access-token=eIlOZCXWfZIR2t5pqcGt6vcc25pb",
        "scLwwCKzFgtOjISymDP6p3nvlwwLl4mA0qeH"),
      ) %>%
      setView(lng = 103.851959, lat = 1.3521, zoom = 10) %>%
      addCircles(
        lat = ~latitude,
        lng = ~longitude,
        stroke = FALSE,
        popup = ~ paste(name, listing_url),
        color = ~ palette_fn(room_type),
        radius = 20,
        fillOpacity = 0.5
      ) %>%
      addLegend(
        position = "bottomright",
        title = paste0(
          "Airbnb Listings in Singapore (",
          paste0(filter_count, " results)")
        ),
        pal = palette_fn,
        values = ~room_type,
        opacity = 1
      )
  })


  ##### Interactive Page Two ##################################################



  ##### Interactive Page Three ################################################
  
  
  
}

# Load packages
library("shiny")
library("tidyverse")
library("leaflet")
library("RColorBrewer")
library("batman")
library("ggplot2")
library("plotly")

# Read data (set wdir to root)
data_sing <- read.csv("data/singapore_listings.csv")
data_chic <- read.csv("data/chicago_listings.csv")
data_bos <- read.csv("data/boston_listings.csv")

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

  output$m_sing <- renderLeaflet({

    # Dynamic user filtering
    plot_data <- data_sing %>%
      filter(price >= input$price_slider[1] &
               price <= input$price_slider[2]) %>%
      filter(accommodates >= input$accom_slider[1] &
               accommodates <= input$accom_slider[2]) %>%
      filter(if (input$checkbox == TRUE)
        host_is_superhost == TRUE
        else id == id) %>%
      filter(if (input$select == "All")
        id == id
        else neighbourhood_cleansed == input$select)

    # Get the count of filtered listings
    filter_count <- nrow(plot_data)

    # Get map pop-up content for listing rating
    popup_rating <- ifelse(plot_data$number_of_reviews > 0,
      paste0(
        "<b style='color:#FF5A5F;'>&#9733; ",
        plot_data$review_scores_rating * 0.05,
        "</b> (",
        plot_data$number_of_reviews, ")"
      ),
      "No Reviews"
    )

    # Get map pop-up content for host status
    popup_superhost <- ifelse(plot_data$host_is_superhost == T,
      paste0(" &#183; <b style='color:#FF5A5F;'>
                                     &#127894;</b> Superhost"),
      ""
    )

    # Get map pop-up content for guest capacity
    popup_guests <- ifelse(plot_data$accommodates > 1,
      paste0(plot_data$accommodates, " guests"),
      paste0(plot_data$accommodates, " guest")
    )

    # Compile all content for map pop-up
    popup_content <- paste0(
      sep = "<br/>",
      paste0(
        "<h5><span style='color:#767676;'>",
        popup_rating, popup_superhost,
        " &#183; <u>", plot_data$neighbourhood_cleansed,
        ", Singapore</u></span></h5><hr>"
      ),
      paste0(
        "<center><h4><b>$", plot_data$price,
        "</b> / night</h4></center>"
      ),
      paste0("<center><h6>", popup_guests, "</h6></center>"),
      paste0(
        "<center><h5><b><a href=", plot_data$listing_url,
        ">", plot_data$name, "</a></b></h5></center>"
      ),
      paste0(
        "<center><img src=", plot_data$picture_url,
        " width=300 height=180></center>"
      )
    )

    # Create Leaflet map of user-filtered Singapore listings
    leaflet(data = plot_data) %>%
      addTiles(
        urlTemplate = paste0(
          "https://tile.jawg.io/ba3f805c-04fb-4fa7-99ef-b9",
          "05aa38b3c8/{z}/{x}/{y}.png?access-token=eIlOZCXWfZIR2t5pqcGt6vcc25",
          "pbscLwwCKzFgtOjISymDP6p3nvlwwLl4mA0qeH"
        ),
      ) %>%
      setView(lng = 103.841959, lat = 1.3521, zoom = 11.5) %>%
      addCircles(
        lat = ~latitude,
        lng = ~longitude,
        stroke = FALSE,
        popup = ~popup_content,
        color = ~palette_fn(room_type),
        radius = 20,
        fillOpacity = 0.5
      ) %>%
      addLegend(
        position = "bottomright",
        title = paste0(
          "Airbnb Listings in Singapore (", filter_count, " results)"
        ),
        pal = palette_fn,
        values = ~room_type,
        opacity = 1
      )
  })


  ##### Interactive Page Two ##################################################

  # changes the column of price from characters to numbers
  data_chic$price <- as.numeric(gsub("[$,]", "", data_chic$price))

  output$chart_chic <- renderPlotly({
    title <- paste0(
      input$room_type, " listings in ",
      input$neighbourhood, ", Chicago"
    )

    # prepares data for the chart
    chart_data <- data_chic %>%
      filter(grepl(input$room_type, room_type)) %>%
      filter(grepl(input$neighbourhood, neighbourhood_cleansed))

    scatter_plot <- ggplot(chart_data) +
      geom_point(
        mapping = aes_string(
          x = "accommodates",
          y = "price",
          color = "bedrooms"
        )
      ) +
      scale_x_continuous(
        breaks = seq(
          input$accommodation_range[1],
          input$accommodation_range[2],
          1
        ),
        limits = c(
          input$accommodation_range[1],
          input$accommodation_range[2]
        )
      ) +
      scale_y_continuous(
        limits = c(input$price_range[1], input$price_range[2])
      ) +
      scale_color_gradient2(low = "blue", mid = "white", high = "red") +
      labs(
        x = "Number of People Accommodating",
        y = "Price ($USD per night)",
        title = title
      )

    ggplotly(scatter_plot)
  })


  ##### Interactive Page Three ################################################

  # cleanse data
  data_bos$price <- as.numeric(gsub("[$,]", "", data_bos$price))
  data_bos$host_is_superhost <- to_logical(data_bos$host_is_superhost)
  data_bos$instant_bookable <- to_logical(data_bos$instant_bookable)
  # arrange_neighborhoods in descending order for neighborhood dropbox menu
  sort_neighbourhood <- data_bos %>% 
    group_by(neighbourhood_cleansed) %>% 
    summarise(num_listings = n()) %>% 
    arrange(desc(num_listings))
  # color palette
  palette_fn <- colorFactor(palette = "Set1", domain = data_bos$room_type)
  
  # Boston map
  output$bos_map <- renderLeaflet({
    # filters
    map_filters <- data_bos %>% 
      filter(if(input$Neighbourhood == "All")
        id == id
        else neighbourhood_cleansed==input$Neighbourhood) %>% 
      filter(price >= input$price[1] & price <= input$price[2]) %>%
      filter(accommodates >= input$accommodation_size) %>% 
      filter(review_scores_rating >= input$review_rating) %>% 
      filter(if(input$superhost_checkbox == TRUE)
        host_is_superhost == TRUE
        else id == id) %>% 
      filter(if(input$instant_book_checkbox == TRUE)
        instant_bookable == TRUE
        else id == id)
    
    # map
    leaflet(data = map_filters) %>% 
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -71.067083, lat = 42.343145, zoom = 12) %>% 
      addCircles(
        lat = ~latitude,   
        lng = ~longitude, 
        label = ~paste("$", price, sep = ""),
        color = ~palette_fn(room_type),
        radius = 60,     
        stroke = FALSE,
        fillOpacity = .4,
        popup = ~paste0("<b><a href='", listing_url, "'>",name, "</a></b>", 
                        "<br/>", description)
      ) %>% 
      addLegend(
        position = "bottomright",
        title = "Listing Type",
        pal = palette_fn, 
        values = ~room_type, 
        opacity = 1
      )
  })
  
  # Neighborhood filtered chart
  output$top_10_chart <- renderPlot({
    # top 10 neighborhoods by # listings
    top_10 <- data_bos %>%
      group_by(neighbourhood_cleansed) %>%
      summarize(num_listings = n()) %>%
      top_n(n = 10, wt = num_listings) %>%
      ggplot(mapping = aes(x = fct_reorder(neighbourhood_cleansed, num_listings), 
                           y = num_listings, fill = neighbourhood_cleansed)) +
      geom_col() +
      coord_flip() +
      theme(legend.position = "bottom") +
      labs(title = "Top 10 neighborhoods by no. of listings",
           x = "Neighborhood", y = "No. of listings")
    top_10
  })
}

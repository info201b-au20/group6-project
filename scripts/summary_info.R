library(tidyverse)
# set working directory as group6-project
boston <- read.csv("data/boston_listings.csv")
chicago <- read.csv("data/chicago_listings.csv")
singapore <- read.csv("data/singapore_listings.csv")

boston$price <- as.numeric(gsub('[$,]', '', boston$price))
chicago$price <- as.numeric(gsub('[$,]', '', chicago$price))
london$price <- as.numeric(gsub('[$,]', '', london$price))


# pass each dataset into the function to return an summary list
info_summary <- function(dataset) {
  # number of listings
  # median price
  # mean bedrooms
  # superhost ratio
  # number of neighborhoods
  # listings in each neighborhood
  # price by accommodation
  # price by neighborhood
  # property type by neighborhood
  num_listings <- nrow(dataset)
  median_price <- median(dataset$price)
  mean_bedrooms <- round(mean(dataset$bedrooms, na.rm = 1), digits = 2)
  superhost_ratio <- dataset %>% 
    filter(host_is_superhost == "t") %>% 
    summarise(superhost_ratio = round(length(host_is_superhost)
                                      / dataset$num_listings, 
                                      digits = 2)) %>% 
    pull(superhost_ratio)
  num_neighborhoods <- n_distinct(dataset$neighbourhood_cleansed)
  num_in_neighborhood <- dataset %>% 
    group_by(neighbourhood_cleansed) %>% 
    rename(neighborhood = neighbourhood_cleansed) %>% 
    count(neighborhood) %>% 
    arrange(desc(n))
  price_by_num_accomodates <- dataset %>% 
    group_by(accommodates) %>% 
    summarise(avg_price = mean(price))
  price_by_neighborhood <- dataset %>% 
    group_by(neighbourhood_cleansed) %>% 
    summarise(min_price = min(price),
              avg_price = round(mean(price), digits = 2),
              max_price = max(price))
  property_type <- dataset %>% 
    group_by(neighbourhood_cleansed) %>% 
    count(property_type)
  summary_dataset <- list (num_listings = num_listings, 
                           median_price = median_price, 
                           mean_bedrooms = mean_bedrooms,
                           superhost_ratio = superhost_ratio, 
                           num_neighborhoods = num_neighborhoods, 
                           num_in_neighborhood = num_in_neighborhood, 
                           price_by_num_accomodates = price_by_num_accomodates, 
                           price_by_neighborhood = price_by_neighborhood, 
                           property_type = property_type)
  summary_dataset
}


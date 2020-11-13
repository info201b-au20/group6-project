library(tidyverse)
# set working directory as group6-project
boston <- read.csv("data/boston_listings.csv")
chicago <- read.csv("data/chicago_listings.csv")
singapore <- read.csv("data/singapore_listings.csv")

boston$price <- as.numeric(gsub('[$,]', '', boston$price))
chicago$price <- as.numeric(gsub('[$,]', '', chicago$price))
london$price <- as.numeric(gsub('[$,]', '', london$price))


boston_summary <- list()
# number of listings
# median price
# mean bedrooms
# superhost ratio
# number of neighborhoods
# listings in each neighborhood
# price by accommodation
# price by neighborhood
boston_summary$num_listings <- nrow(boston)
boston_summary$median_price <- median(boston$price)
boston_summary$mean_bedrooms <- round(mean(boston$bedrooms, na.rm = 1), digits = 2)
boston_summary$superhost_ratio <- boston %>% 
  filter(host_is_superhost == "t") %>% 
  summarise(superhost_ratio = round(length(host_is_superhost)
                                    / boston_summary$num_listings, 
                                    digits = 2)) %>% 
  pull(superhost_ratio)
boston_summary$num_neighborhoods <- n_distinct(boston$neighbourhood_cleansed)
boston_summary$num_in_neighborhood <- boston %>% 
  group_by(neighbourhood_cleansed) %>% 
  rename(neighborhood = neighbourhood_cleansed) %>% 
  count(neighborhood) %>% 
  arrange(desc(n))
boston_summary$price_by_num_accomodates <- boston %>% 
  group_by(accommodates) %>% 
  summarise(avg_price = mean(price))
boston_summary$price_by_neighborhood <- boston %>% 
  group_by(neighbourhood_cleansed) %>% 
  summarise(avg_price = round(mean(price), digits = 2),
            min_price = min(price),
            max_price = max(price))







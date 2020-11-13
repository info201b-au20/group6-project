#Create a horizontal bar chart of Singapore neighborhoods with most listings
library(tidyverse)
data <- read.csv("data/singapore_listings.csv")

#Get number of listings for each neighborhood
num_listings_per_neighborhood <- data %>% 
  rename("neighborhood" = neighbourhood_cleansed) %>% 
  group_by(neighborhood) %>% 
  summarize(num_listings = n()) 

#Filter down to top 10 neighborhood with most listings
top_10 <- num_listings_per_neighborhood %>% 
  slice_max(num_listings, n = 10) %>% 
  arrange(num_listings) %>% #sort the data by population
  mutate(neighborhood = factor(neighborhood, neighborhood)) #set the row order


#Render a horizontal bar chart of listings
listings_plot <- ggplot(top_10) +
  geom_col(mapping = aes(x = neighborhood, y = num_listings)) +
  coord_flip() +
  
labs(
  title = "Singapore Top 10 Neighborhoods by Listings",
  x = "Neighborhood",
  y = "Number of Listings"
)

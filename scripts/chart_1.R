library(ggplot2)
library(tidyverse)
# set working directory first
boston <- read.csv("data/boston_listings.csv")

# extracts the room_type column and counts the numbers of each type
type <- boston %>%
  group_by(room_type) %>%
  summarize(num_type = n())

# creates a pie chart of room types on Airbnb in Boston
plot_type <- ggplot(data = type) +
  geom_bar(mapping = aes(x = "", y = num_type, fill = room_type), stat="identity") +
  coord_polar("y") +
  theme_void() +
  labs(title = "Types of Rooms on Airbnb in Boston") +
  geom_text(mapping = aes(x = "", y = num_type, label = num_type))

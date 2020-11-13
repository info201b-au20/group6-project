#This chart shows...

library("ggplot2")
library("dplyr")

#set working directory first
avg_boston <- info_summary(boston)$price_by_num_accomodates
avg_chicago <- info_summary(chicago)$price_by_num_accomodates
avg_singapore <- info_summary(singapore)$price_by_num_accomodates

colors <- c("Boston" = "blue", "Chicago" = "red", "Singapore" = "green")

#create a line plot showing the average prices for each city based on accommodation
compare_all <- ggplot() +
  geom_line(data = avg_boston, aes(x = accommodates, y = avg_price, color = "Boston")) +
  geom_line(data = avg_chicago, aes(x = accommodates, y = avg_price, color = "Chicago")) +
  geom_line(data = avg_singapore, aes(x = accommodates, y = avg_price, color = "Singapore")) +
  scale_x_continuous(breaks = seq(0, 17, 1)) +
  geom_point() +
  #geom_point(mapping = aes(x = accommodates, y = avg_price)) +
  #how to make it show points
  #also how to lint???
  labs(title = "Average Price for Number of Accomodations in Different Cities",
       x = "Number of People",
       y = "Average Price",
       color = "Legend") +
  scale_color_manual(values = colors) 
  



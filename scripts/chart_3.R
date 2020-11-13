#This chart shows...

library("ggplot2")
library("dplyr")

avg_boston <- info_summary(boston)$price_by_num_accomodates
avg_chicago <- info_summary(chicago)$price_by_num_accomodates
avg_singapore <- info_summary(singapore)$price_by_num_accomodates


#compare_boston <- ggplot(data = avg_boston) +
 # geom_line(mapping = aes(x = accommodates, y = avg_price))

#compare_chicago <- ggplot(data = avg_chicago)+
 # geom_line(mapping = aes(x = accommodates, y = avg_price))

#compare_singapor <- ggplot(data = avg_singapore)+
 # geom_line(mapping = aes(x = accommodates, y = avg_price))


colors <- c("Boston" = "blue", "Chicago" = "red", "Singapore" = "green")

compare_all <- ggplot() +
  geom_line(data = avg_boston, aes(x = accommodates, y = avg_price, color = "Boston")) +
  geom_line(data = avg_chicago, aes(x = accommodates, y = avg_price, color = "Chicago")) +
  geom_line(data = avg_singapore, aes(x = accommodates, y = avg_price, color = "Singapore")) +
  #geom_point(mapping = aes(x = accommodates, y = avg_price)) +
  #how to make it show points and for the x-axis to be separated by 1?
  #also how to create a legend 
  labs(title = "Average Price for Number of Accomodations in Different Cities",
       x = "Number of People",
       y = "Average Price",
       color = "Legend") +
  scale_color_manual(values = colors)



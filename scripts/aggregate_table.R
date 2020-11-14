install.packages("stringr")
install.packages("tidyverse")
install.packages("stringi")
library(tidyverse)
library(stringr)
library(stringi)

# read data of listing in Boston 
boston <- read.csv("~/Desktop/info201/group6-project/data/boston_listings.csv")

# find the Airbnb room with largest accommodates number in each scraped day
# Users can know the maximum number of people that 
# the host can accommodate and plan their occupancy allocation.
accommodates_max <- boston %>%
  group_by(last_scraped) %>%
  summarise(accommodates = max(accommodates))

# show each host's response rate and corresponding time
# Users can find the easiest host to communicate with and help check-in 
# through the host response rate and response speed.
host_response <- boston %>%
  group_by(host_id,host_name) %>%
  summarise(rate = unique(host_response_rate),time = unique(host_response_time))

# find the time when each operator first started hosting Airbnb
# Owners who have been operating airbnb longer are usually able to better understand 
# the needs of different users.
host_start <- boston %>%
  group_by(host_id,host_name) %>%
  summarise(time = unique(host_since))

# find the highest price of each host 
price_high <- boston %>%
  group_by(host_id,host_name) %>%
  summarise(highest = max(price,na.rm = T)) 

# find the lowest price of each host 
# By understanding the highest and lowest prices of the corresponding room of 
# the host, users can estimate the price range of the host and make a reasonable choice.
price_low <- boston %>%
  group_by(host_id,host_name) %>%
  summarise(lowest = min(price,na.rm = T)) 

# arrange each host's room type by his/her name and id
# By knowing the property type of each room, users can choose their favorite type and avoid the trouble caused by the room types.
room_type_arrange <- boston %>%
  group_by(host_id,host_name,property_type) %>%
  summarise(room = unique(room_type))



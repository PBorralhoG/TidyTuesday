
library(tidytuesdayR)
library(dplyr)
library(stringr)
library(sf)
library(ggplot2)
library(mapdata)

###############################################################################################################################################################################################

tuesdata <- tidytuesdayR::tt_load(2023, week = 41)
haunted_places <- tuesdata$haunted_places

###############################################################################################################################################################################################

haunted_schools <- haunted_places %>%
  filter(str_detect(location %>% tolower, "school|college|university|education|elementary")) %>%
  mutate(longitude = ifelse(is.na(longitude), city_longitude, longitude),
         latitude = ifelse(is.na(latitude), city_latitude, latitude)) %>%
  filter(!is.na(longitude) & !is.na(latitude)) %>%
  filter(!state %in% c("Alaska", "Hawaii")) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = F)

plot(haunted_schools$geometry)

###############################################

haunted_schools_count <- haunted_schools %>% as.data.frame %>% group_by(state) %>% summarise(n = n()) %>%
  mutate(state = tolower(state),
         state = ifelse(state == "washington dc", "district of columbia", state))

us_states_map <- map_data("state") %>%
  mutate(region = tolower(region)) %>%
  left_join(haunted_schools_count, by = join_by(region == state))

# setdiff(haunted_schools_count$state, us_states_map$region)
# setdiff(us_states_map$region, haunted_schools_count$state)

###############################################

ggplot(data = us_states_map, aes(x = long, y = lat, fill = n, group = group)) + 
  geom_polygon(color = "white") +
  scale_fill_gradient(low = "orange", high = "purple") +
  # scale_fill_gradient2(midpoint = max(haunted_schools_count$n)/2, low = "orange", mid = "purple", high = "black") +
  # guides(fill = "none") +
  theme_void() +
  coord_map(clip = "off")

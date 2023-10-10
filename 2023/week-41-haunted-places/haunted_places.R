
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

haunted_places <- haunted_places %>%
  mutate(longitude = ifelse(is.na(longitude), city_longitude, longitude),
         latitude = ifelse(is.na(latitude), city_latitude, latitude)) %>%
  filter(!is.na(longitude) & !is.na(latitude)) %>%
  filter(!state %in% c("Alaska", "Hawaii")) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = F)

haunted_schools <- haunted_places %>%
  filter(str_detect(location %>% tolower, "school|college|university|education|elementary"))

plot(st_geometry(haunted_schools))

###############################################

us_states <- map_data("state") %>% mutate(region = tolower(region))

###############################################

haunted_schools_count <- haunted_schools %>% as.data.frame %>% group_by(state) %>% summarise(n = n()) %>%
  mutate(state = tolower(state),
         state = ifelse(state == "washington dc", "district of columbia", state))

us_states <- us_states %>%
  left_join(haunted_schools_count, by = c("region"="state"))

# setdiff(haunted_schools_count$state, us_states$region)
# setdiff(us_states$region, haunted_schools_count$state)

###############################################

ggplot() +
  geom_polygon(data = us_states, aes(x = long, y = lat, group = group, fill = n), color = "white") +
  scale_fill_gradient(low = "orange", high = "purple") +
  geom_point(data = haunted_schools, aes(x = longitude, y = latitude)) +
  guides(fill = "none") +
  theme_void() +
  coord_map(clip = "off")

###############################################################################################################################################################################################

n_bins <- 5
color_palette <- colorRampPalette(c("orange", "purple"))(n_bins)

ggplot(haunted_schools, aes(x = longitude, y = latitude)) +
  geom_polygon(data = us_states, aes(x = long, y = lat, group = group), fill = grey(0.3), color = "white") +
  stat_density_2d(geom = "polygon", aes(fill = as.factor(..level..)), bins = n_bins, alpha = 0.8) +
  geom_polygon(data = us_states, aes(x = long, y = lat, group = group), fill = NA, color = alpha("white", 0.2)) +
  # geom_point() +
  scale_fill_manual(values = color_palette, aesthetics = c("fill", "color")) +
  guides(fill = "none", color = "none") +
  theme_void() +
  theme(plot.background = element_rect(fill = grey(0.8))) #3E2469

  




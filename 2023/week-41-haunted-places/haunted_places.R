
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

us_states_sf <- us_states %>% st_as_sf(coords = c("long", "lat"), crs = 4326)

grid <- st_make_grid(us_states_sf, cellsize = 1, square = F, flat_topped = T)

grid_sf <- st_sf(grid)
grid_sf$n <- lengths(st_intersects(grid_sf, haunted_schools))
grid_sf_g0 <- filter(grid_sf, n > 0)
grid_sf_geq10 <- filter(grid_sf, n >= 10)

ggplot(grid) + geom_sf()
ggplot(grid_sf_g0) + geom_sf()

ggplot() +
  # geom_sf(data = grid, fill = "orange") +
  geom_polygon(data = us_states, aes(x = long, y = lat, group = group), fill = "grey", color = "white") +
  geom_sf(data = grid_sf_geq10, fill = "orange") +
  geom_point(data = haunted_schools, aes(x = longitude, y = latitude)) +
  guides(fill = "none", color = "none") +
  theme_void() #+
  # coord_map(clip = "off")





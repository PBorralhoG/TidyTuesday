
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

us_states <- map_data("state") %>%
  mutate(region = tolower(region))

us_counties <- map_data("county")

###############################################

haunted_schools_count <- haunted_schools %>% as.data.frame %>% group_by(state) %>% summarise(n = n()) %>%
  mutate(state = tolower(state),
         state = ifelse(state == "washington dc", "district of columbia", state))

us_states <- us_states %>%
  left_join(haunted_schools_count, by = c("region"="state"))

# setdiff(haunted_schools_count$state, us_states$region)
# setdiff(us_states$region, haunted_schools_count$state)

###############################################

ggplot(data = us_states, aes(x = long, y = lat, group = group, fill = n)) + 
  geom_polygon(color = "white") +
  scale_fill_gradient(low = "orange", high = "purple") +
  # scale_fill_gradient2(midpoint = max(haunted_schools_count$n)/2, low = "orange", mid = "purple", high = "black") +
  # guides(fill = "none") +
  theme_void() +
  coord_map(clip = "off")

# ggplot(data = us_counties, aes(x = long, y = lat, fill = subregion)) + 
#   geom_polygon(color = "white") +
#   # guides(fill = "none") +
#   # theme_void() +
#   coord_map(clip = "off")

ggplot() +
  geom_polygon(data = us_states, aes(x = long, y = lat, group = group, fill = n), color = "white") +
  scale_fill_gradient(low = "orange", high = "purple") +
  geom_point(data = haunted_schools, aes(x = longitude, y = latitude)) +
  guides(fill = "none") +
  theme_void() +
  coord_map(clip = "off")

###############################################################################################################################################################################################
# clustering

km <- kmeans(haunted_schools %>% as.data.frame %>% select(longitude, latitude), centers = 10)
haunted_schools$cluster <- km$cluster

table(haunted_schools$cluster) %>% sort(decreasing = T)

ggplot() +
  geom_polygon(data = us_states, aes(x = long, y = lat, group = group), fill = "grey", color = "white") +
  scale_fill_gradient(low = "orange", high = "purple") +
  geom_point(data = haunted_schools, aes(x = longitude, y = latitude, color = cluster)) +
  guides(fill = "none") +
  theme_void() +
  coord_map(clip = "off")

###############################################################################################################################################################################################

us_states_sf <- us_states %>% st_as_sf(coords = c("long", "lat"), crs = 4326) %>% st_transform(3857)

grid <- st_make_grid(us_states_sf, cellsize = 100*10^3, square = F, flat_topped = T)

grid_sf <- st_sf(grid)
grid_sf$n <- lengths(st_intersects(grid_sf, haunted_schools %>% st_transform(3857)))
grid_sf_g0 <- filter(grid_sf, n > 0)
grid_sf_geq10 <- filter(grid_sf, n >= 10)

ggplot(grid) + geom_sf()
ggplot(grid_sf_g0) + geom_sf()

ggplot() +
  # geom_polygon(data = us_states, aes(x = long, y = lat, group = group), fill = "grey", color = "white") +
  geom_sf(data = us_states_sf, fill = "grey") +
  geom_sf(data = grid_sf_geq10, fill = "orange") +
  # scale_fill_gradient(low = "orange", high = "purple") +
  # geom_point(data = haunted_schools, aes(x = longitude, y = latitude, color = cluster)) +
  guides(fill = "none", color = "none") +
  theme_void() #+
  # coord_map(clip = "off")





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

us_states <- map_data("state") %>% mutate(region = tolower(region))

##########################################

n_bins <- 5
color_palette <- colorRampPalette(c("orange", "purple"))(n_bins)

ggplot(haunted_schools, aes(x = longitude, y = latitude)) +
  geom_polygon(data = us_states, aes(x = long, y = lat, group = group), fill = grey(0.3), color = "white") +
  stat_density_2d(geom = "polygon", aes(fill = as.factor(..level..)), bins = n_bins, alpha = 0.8) +
  geom_polygon(data = us_states, aes(x = long, y = lat, group = group), fill = NA, color = alpha("white", 0.2)) +
  # geom_point() +
  scale_fill_manual(values = color_palette, aesthetics = c("fill", "color")) +
  guides(fill = "none", color = "none") +
  coord_map(clip = "off") +
  theme_void() +
  theme(plot.title = element_text(size = 18, hjust = 0.11, face = "bold"),
        plot.subtitle = element_text(size = 12, hjust = 0.135, color = "purple"),
        plot.caption = element_text(size = 10, hjust = 1, vjust = 8),
        plot.margin = unit(c(1, 0.5, 1, 0.5), "cm"),
        plot.background = element_rect(fill = grey(0.8))) +
  labs(title = "Haunted Schools in the United States",
       subtitle = "If you go to school in these areas, you might want to skip classes for the spooky season",
       caption = "#TidyTuesday week 41 2023 | Data: Tim Renner on data.world | Plot: Pedro Borralho @PBorralhoG")

  




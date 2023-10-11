
library(tidytuesdayR)
library(dplyr)
library(stringr)
library(sf)
library(mapdata)
library(ggplot2)
library(showtext)

# add font
font_add_google("Special Elite", family = "Special Elite"); font <- "Special Elite"
showtext_auto()
showtext_opts(dpi = 320)

################################################################################################
# load data

tuesdata <- tidytuesdayR::tt_load(2023, week = 41)

################################################################################################
# prepare data

haunted_schools <- tuesdata$haunted_places %>%
  mutate(longitude = ifelse(is.na(longitude), city_longitude, longitude),
         latitude = ifelse(is.na(latitude), city_latitude, latitude)) %>%
  filter(!is.na(longitude) & !is.na(latitude)) %>%
  filter(!state %in% c("Alaska", "Hawaii")) %>%
  filter(str_detect(location %>% tolower, "school|college|university|education|elementary")) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = F)

# load us states to plot
us_states <- map_data("state")

################################################################################################
# plot

n_bins <- 5
color_palette <- colorRampPalette(c("orange", "purple"))(n_bins)

ggplot(haunted_schools, aes(x = longitude, y = latitude)) +
  geom_polygon(data = us_states, aes(x = long, y = lat, group = group), fill = grey(0.3), color = "white") +
  stat_density_2d(geom = "polygon", aes(fill = as.factor(after_stat(level))), bins = n_bins, alpha = 0.8) +
  geom_polygon(data = us_states, aes(x = long, y = lat, group = group), fill = NA, color = alpha("white", 0.2)) +
  scale_fill_manual(values = color_palette, aesthetics = c("fill", "color")) +
  coord_map(clip = "off") +
  labs(title = "Haunted Schools in the United States",
       # subtitle = "If you go to school in these areas, you might want to skip classes for the spooky season",
       subtitle = "If you live in these areas, you might want to skip classes for the spooky season",
       caption = paste0("#TidyTuesday week 41 2023 | Data: Tim Renner on data.world | Plot: Pedro Borralho")) +
  theme_void() +
  theme(plot.title = element_text(family = font, size = 20, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(family = font, size = 8.5, hjust = 0.5, color = "purple", margin = margin(t = 10, b = 40)), # size = 10
        plot.caption = element_text(family = font, size = 8, hjust = 0.5, margin = margin(t = 40)), # size = 8
        legend.position = "none",
        plot.margin = unit(c(1, 0.5, 1, 0.5), "cm"),
        plot.background = element_rect(fill = grey(0.85), color = NA))

# save plot
ggsave(paste0("haunted_schools_", format(Sys.time(), "%Y%m%d"), ".png"), dpi = 320, width = 6, height = 6)
  





library(tidytuesdayR)
library(dplyr)
library(stringr)
library(ggplot2)

###############################################################################################################################################################################################

tuesdata <- tidytuesdayR::tt_load(2023, week = 41)
haunted_places <- tuesdata$haunted_places

###############################################################################################################################################################################################

haunted_places %>% group_by(state) %>% summarise(n = n()) %>% arrange(desc(n))

########################################################

locations_2check <- "school|college|university|education|courthouse|road|restaurant|blvd|beach|park|cemetery|lake|hill|hospital|hotel|tavern|pub|theater|bridge|motel|asylum|house|train|library|church"

haunted_places %>% filter(!str_detect(location %>% tolower, locations_2check)) %>% pull(location) %>% unique

haunted_places$location %>% tolower %>% str_detect(locations_2check) %>% table

########################################################

haunted_schools <- haunted_places %>% filter(str_detect(location %>% tolower, "school|college|university|education|elementary"))

###############################################################################################################################################################################################



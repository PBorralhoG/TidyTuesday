
# library(tidytuesdayR)
library(dplyr)

################################################################################################
# load data

# tuesdata <- tidytuesdayR::tt_load(2023, week = 42)
# 
# taylor_album_songs <- tuesdata$taylor_album_songs
# taylor_all_songs <- tuesdata$taylor_all_songs
# taylor_albums <- tuesdata$taylor_albums

# taylor_album_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-17/taylor_album_songs.csv')
# taylor_all_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-17/taylor_all_songs.csv')
# taylor_albums <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-17/taylor_albums.csv')

library(taylor)

################################################################################################

plot(taylor_album_songs$album_release, taylor_album_songs$tempo)

tempo_by_album <- taylor_album_songs %>%
  group_by(album_name) %>%
  summarise(tempo_median = median(tempo, na.rm = T),
            album_release = unique(album_release))

plot(tempo_by_album$album_release, tempo_by_album$tempo_median)

setdiff(taylor_albums$album_name, tempo_by_album$album_name)
setdiff(tempo_by_album$album_name, taylor_albums$album_name)

taylor_albums <- taylor_albums %>% left_join(tempo_by_album %>% select(album_name, tempo_median), by = "album_name")

plot(taylor_albums$tempo_median, taylor_albums$metacritic_score)
plot(taylor_albums$tempo_median, taylor_albums$user_score)
plot(taylor_albums$metacritic_score, taylor_albums$user_score)

################################################################################################

taylor_album_songs$track_name[1]
"blue eyes" %in% taylor_album_songs$lyrics[[1]]$lyric



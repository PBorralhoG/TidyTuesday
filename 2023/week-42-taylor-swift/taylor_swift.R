
library(taylor)
library(dplyr)
library(stringr)

################################################################################################
# load data
 
# taylor_album_songs
# taylor_all_songs
# taylor_albums

################################################################################################

taylor_album_songs$lyrics <- lapply(1:nrow(taylor_album_songs), function(i) taylor_album_songs$lyrics[[i]]$lyric)
taylor_album_songs$found_n <- sapply(1:nrow(taylor_album_songs),
                                     function(i) str_count(taylor_album_songs$lyrics[i] %>% unlist %>% tolower, taylor_album_songs$track_name[i] %>% tolower) %>% sum)

boxplot(taylor_album_songs$found_n ~ taylor_album_songs$album_name)

sum(taylor_album_songs$found_n > 0) / nrow(taylor_album_songs) # 56%

perc_by_album <- taylor_album_songs %>%
  group_by(album_name) %>%
  summarise(perc = sum(found_n > 0) / n(),
            album_release = unique(album_release)) %>%
  arrange(desc(perc))

plot(perc_by_album$album_release, perc_by_album$perc)






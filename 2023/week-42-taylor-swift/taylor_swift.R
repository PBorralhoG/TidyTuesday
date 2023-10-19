
library(taylor)
library(dplyr)
library(stringr)
library(ggplot2)

################################################################################################
# load data
 
# taylor_album_songs
# taylor_all_songs
# taylor_albums

################################################################################################

taylor_album_songs$lyrics <- lapply(1:nrow(taylor_album_songs), function(i) taylor_album_songs$lyrics[[i]]$lyric)

taylor_album_songs <- taylor_album_songs %>% mutate(track_name_clean = track_name %>% tolower %>%
                                                      str_replace(" \\(taylor's version\\)", "") %>% str_replace(" \\[taylor's version\\]", "") %>%
                                                      str_replace(" \\(from the vault\\)", "") %>% str_replace(" \\[from the vault\\]", "") %>%
                                                      str_replace(" \\(piano version\\)", "") %>%
                                                      str_replace(" \\(pop version\\)", "") %>%
                                                      str_replace(" \\(acoustic version\\)", "") %>%
                                                      str_replace(" \\(10 minute version\\)", "") %>%
                                                      str_replace(" \\(remix\\)", "") %>%
                                                      str_replace(" \\(strings remix\\)", "") %>%
                                                      str_replace(" \\(piano remix\\)", "") %>%
                                                      str_replace(" \\(more lana del rey\\)", "") %>%
                                                      str_replace("&", "and"),
                                                    track_name_clean_v2 = track_name_clean %>% str_replace_all("[[:punct:]]", " ") %>% str_squish)

taylor_album_songs$lyrics_clean <- sapply(1:nrow(taylor_album_songs), function(i) taylor_album_songs$lyrics[i] %>% unlist %>% tolower %>% paste0(collapse = " ") %>% str_squish)
taylor_album_songs <- taylor_album_songs %>% mutate(lyrics_clean_v2 = lyrics_clean %>% str_replace_all("[[:punct:]]", " ") %>% str_squish)

taylor_album_songs$found_n_v1 <- sapply(1:nrow(taylor_album_songs), function(i) str_count(taylor_album_songs$lyrics_clean[i], paste0("\\b", taylor_album_songs$track_name_clean[i], "\\b")) %>% sum)
taylor_album_songs$found_n_v2 <- sapply(1:nrow(taylor_album_songs), function(i) str_count(taylor_album_songs$lyrics_clean[i], paste0("\\b", taylor_album_songs$track_name_clean_v2[i], "\\b")) %>% sum)
taylor_album_songs$found_n_v3 <- sapply(1:nrow(taylor_album_songs), function(i) str_count(taylor_album_songs$lyrics_clean_v2[i], paste0("\\b", taylor_album_songs$track_name_clean_v2[i], "\\b")) %>% sum)
# taylor_album_songs <- taylor_album_songs %>%
#   mutate(diff_found_n_v1v2 = found_n_v2 - found_n_v1,
#          diff_found_n_v1v3 = found_n_v3 - found_n_v1,
#          diff_found_n_v2v3 = found_n_v3 - found_n_v2)
taylor_album_songs <- taylor_album_songs %>% mutate(found_n = pmax(found_n_v1, found_n_v2, found_n_v3))
# count is not perfect yet

# me: 26
# so it goes: 12
# ready for it: 5
# that's when: 22

################################################################################################

# boxplot(taylor_album_songs$found_n ~ taylor_album_songs$album_name)
# 
# sum(taylor_album_songs$found_n > 0) / nrow(taylor_album_songs) # 56%
# 
# perc_by_album <- taylor_album_songs %>%
#   group_by(album_name) %>%
#   summarise(found_n_g0 = sum(found_n > 0),
#             n_songs = n(),
#             perc = found_n_g0 / n_songs,
#             album_release = unique(album_release)) %>%
#   arrange(desc(perc))
# 
# plot(perc_by_album$album_release, perc_by_album$perc)
# 
# ggplot(perc_by_album %>% filter(perc == 1), aes(x = n_songs, y = album_name)) +
#   geom_tile(aes(fill = found_n_g0), color = "white", lwd = 1, linetype = 1)

################################################################################################

ggplot(taylor_album_songs, aes(x = track_number, y = album_name)) +
  geom_tile(aes(fill = found_n > 0), color = "black") +
  facet_grid(rows = vars(album_name), scales = "free") +
  scale_fill_manual(values = c("TRUE" = "white", "FALSE" = "black"))

taylor_album_songs <- taylor_album_songs %>% arrange(album_name, track_number) %>% mutate(x_pos = NA)
for(j in 1:(taylor_album_songs$album_name %>% unique %>% length)){
  album_j <- (taylor_album_songs$album_name %>% unique)[j]
  songs_j <- taylor_album_songs %>% filter(album_name == album_j)
  songs_j$x_pos[1] <- 1
  for(i in 2:nrow(songs_j)){
    if(songs_j$found_n[i] > 0 & songs_j$found_n[i-1] > 0){
      songs_j$x_pos[i] <- songs_j$x_pos[i-1] + 22
    } else{
      songs_j$x_pos[i] <- songs_j$x_pos[i-1] + 11.5
    }
  }
  taylor_album_songs$x_pos[taylor_album_songs$album_name == album_j] <- songs_j$x_pos
}

taylor_album_songs <- taylor_album_songs %>% mutate(width = ifelse(found_n > 0, 22, 11.5),
                                                    xmax = x_pos + width,
                                                    album_name = factor(album_name),
                                                    ymax = album_name %>% as.numeric + 138,
                                                    ymin = ifelse(found_n > 0,
                                                                  album_name %>% as.numeric,
                                                                  album_name %>% as.numeric + 53))

# ggplot(taylor_album_songs, aes(x = x_pos, y = album_name, width = width)) +
#   geom_tile(aes(fill = found_n > 0), color = "black") +
#   facet_grid(rows = vars(album_name), scales = "free") +
#   scale_fill_manual(values = c("TRUE" = "white", "FALSE" = "black"))

ggplot(taylor_album_songs) +
  geom_rect(aes(xmin = x_pos, xmax = xmax, ymin = ymin, ymax = ymax, fill = found_n > 0),
            color = "black") +
  facet_grid(rows = vars(album_name)) +
  scale_fill_manual(values = c("TRUE" = "white", "FALSE" = "black"))







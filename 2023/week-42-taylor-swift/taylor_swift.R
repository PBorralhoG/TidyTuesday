
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

taylor_all_songs <- taylor_all_songs %>% filter(!is.na(album_name))

taylor_all_songs$lyrics <- lapply(1:nrow(taylor_all_songs), function(i) taylor_all_songs$lyrics[[i]]$lyric)

taylor_all_songs <- taylor_all_songs %>% mutate(track_name_clean = track_name %>% tolower %>%
                                                  str_replace("\\(taylor's version\\)", "") %>% str_replace("\\[taylor's version\\]", "") %>%
                                                  str_replace("\\(from the vault\\)", "") %>% str_replace("\\[from the vault\\]", "") %>%
                                                  str_replace("\\(piano version\\)", "") %>%
                                                  str_replace("\\(pop version\\)", "") %>%
                                                  str_replace("\\(acoustic version\\)", "") %>%
                                                  str_replace("\\(10 minute version\\)", "") %>%
                                                  str_replace("\\(remix\\)", "") %>%
                                                  str_replace("\\(strings remix\\)", "") %>%
                                                  str_replace("\\(piano remix\\)", "") %>%
                                                  str_replace("\\(more lana del rey\\)", "") %>%
                                                  str_replace("\\(original demo recording\\)", "") %>%
                                                  str_replace("\\(radio edit\\)", "") %>%
                                                  str_replace("\\(alternate version\\)", "") %>%
                                                  str_replace("&", "and") %>%
                                                  str_squish(),
                                                track_name_clean_v2 = track_name_clean %>% str_replace_all("[[:punct:]]", " ") %>% str_squish)

taylor_all_songs$lyrics_clean <- sapply(1:nrow(taylor_all_songs), function(i) taylor_all_songs$lyrics[i] %>% unlist %>% tolower %>% paste0(collapse = " ") %>% str_squish)
taylor_all_songs <- taylor_all_songs %>% mutate(lyrics_clean_v2 = lyrics_clean %>% str_replace_all("[[:punct:]]", " ") %>% str_squish)

taylor_all_songs$found_n_v1 <- sapply(1:nrow(taylor_all_songs), function(i) str_count(taylor_all_songs$lyrics_clean[i], paste0("\\b", taylor_all_songs$track_name_clean[i], "\\b")) %>% sum)
taylor_all_songs$found_n_v2 <- sapply(1:nrow(taylor_all_songs), function(i) str_count(taylor_all_songs$lyrics_clean[i], paste0("\\b", taylor_all_songs$track_name_clean_v2[i], "\\b")) %>% sum)
taylor_all_songs$found_n_v3 <- sapply(1:nrow(taylor_all_songs), function(i) str_count(taylor_all_songs$lyrics_clean_v2[i], paste0("\\b", taylor_all_songs$track_name_clean_v2[i], "\\b")) %>% sum)
taylor_all_songs <- taylor_all_songs %>% mutate(found_n = pmax(found_n_v1, found_n_v2, found_n_v3))
# count is not perfect yet

################################################################################################

songs_found_n_g0 <- taylor_all_songs %>% filter(found_n > 0) %>%
  group_by(album_name) %>% arrange(track_number) %>% mutate(xmin = 1:n())

songs_found_n_eq0 <- taylor_all_songs %>% filter(found_n == 0) %>% mutate(xmin = NA)

taylor_all_songs2 <- rbind(songs_found_n_g0, songs_found_n_eq0) %>% arrange(album_name, track_number) %>%
  mutate(xmin = xmin * 22,
         xmin = ifelse(found_n > 0, xmin, ifelse(track_number == 1, 22-11.5/2, lag(xmin)+22-11.5/2)),
         xmin = ifelse(!is.na(xmin), xmin, lag(xmin)+22),
         xmax = ifelse(found_n > 0, xmin + 22, xmin + 11.5),
         album_name_num = factor(album_name) %>% as.numeric,
         ymax = album_name_num + 138,
         ymin = ifelse(found_n > 0, album_name_num, album_name_num + 53)#,
         # track_name_label = ifelse(found_n == 0, track_name, NA)
         )

aux_labels <- taylor_all_songs2 %>% filter(found_n > 0) %>% group_by(album_name) %>% filter(track_number == min(track_number))
album_levels <- taylor_all_songs %>% arrange(album_release) %>% pull(album_name) %>% unique

ggplot(mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = found_n > 0)) +
  geom_rect(data = taylor_all_songs2 %>% filter(found_n > 0), color = "black") +
  geom_rect(data = taylor_all_songs2 %>% filter(found_n == 0), color = "black") +
  scale_fill_manual(values = c("TRUE" = "white", "FALSE" = "black")) +
  # facet_grid(rows = vars(factor(album_name, levels = album_levels))) +
  facet_wrap( ~ factor(album_name, levels = album_levels), ncol = 2, dir = "v") +
  geom_text(data = aux_labels, aes(x = xmin, y = ymax + 15, label = album_name), check_overlap = T, hjust = 0) +
  # ggrepel::geom_text_repel(data = taylor_all_songs2, aes(x = xmax, y = ymax, label = track_name_label)) +
  theme_void() +
  theme(strip.text = element_blank()) +
  coord_fixed(ratio = 1)






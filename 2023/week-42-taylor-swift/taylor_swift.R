
library(taylor)
library(dplyr)
library(stringr)
library(ggplot2)
library(showtext)

# add font
font_add_google("Poppins"); font <- "Poppins"
showtext_auto(); showtext_opts(dpi = 320)

################################################################################################
# load data
 
# taylor_album_songs
# taylor_all_songs
# taylor_albums

################################################################################################

# taylor_all_songs <- taylor_all_songs %>% select(album_name, ep, album_release, track_number, track_name, lyrics)

taylor_all_songs <- taylor_all_songs %>%
  filter(!is.na(album_name)) %>%
  filter(!ep) %>%
  filter(!album_name %in% c("Fearless (Taylor's Version)", "Red (Taylor's Version)")) %>%
  mutate(track_name_clean = track_name %>% tolower %>%
           str_replace("\\(piano version\\)", "") %>%
           str_replace("\\(pop version\\)", "") %>%
           str_replace("\\(acoustic version\\)", "") %>%
           str_replace("\\(strings remix\\)", "") %>%
           str_replace("\\(piano remix\\)", "") %>%
           str_replace("\\(original demo recording\\)", "") %>%
           str_replace("&", "and") %>%
           str_replace("1", "one") %>%
           str_squish(),
         track_name_clean_v2 = track_name_clean %>% str_replace_all("[[:punct:]]", " ") %>% str_squish)

taylor_all_songs$lyrics <- lapply(1:nrow(taylor_all_songs), function(i) taylor_all_songs$lyrics[[i]]$lyric)
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
         ymin = ifelse(found_n > 0, album_name_num, album_name_num + 53))

aux_labels <- taylor_all_songs2 %>% filter(found_n > 0) %>% group_by(album_name) %>% filter(track_number == min(track_number))
album_levels <- taylor_all_songs %>% arrange(album_release) %>% pull(album_name) %>% unique

ann_text <- rbind(data.frame(x = 366, y = 150, album_name = "Taylor Swift",
                             label = "Mary's Song (Oh My My My)\n(there is no Mary in the lyrics)"),
            data.frame(x = 520, y = 150, album_name = "Red",
                       label = "We Are Never Ever\nGetting Back Together\n(in the lyrics it's ever ever \nor even ever ever ever)"))
ann_arrows <- rbind(data.frame(x = 222, y = 145, xend = 356, yend = 160, album_name = "Taylor Swift"),
                    data.frame(x = 178, y = 145, xend = 510, yend = 160, album_name = "Red"))

ggplot() +
  geom_rect(data = taylor_all_songs2 %>% filter(found_n > 0), color = grey(0.3),
            mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = found_n > 0)) +
  geom_rect(data = taylor_all_songs2 %>% filter(found_n == 0), color = "black",
            mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = found_n > 0)) +
  scale_fill_manual(breaks = c("TRUE", "FALSE"), values = c("TRUE" = "white", "FALSE" = "black"),
                    labels = c("TRUE" = "song title is in the lyrics", "FALSE" = "it's not")) +
  scale_x_continuous(limits = c(0, 855)) +
  scale_y_continuous(limits = c(-10, 200)) +
  facet_wrap( ~ factor(album_name, levels = album_levels), ncol = 2, dir = "v") +
  geom_text(data = aux_labels, aes(x = xmin, y = ymax + 30, label = album_name), check_overlap = T, hjust = 0, family = font, size = 9/.pt) +
  geom_text(data = ann_text, aes(x = x, y = y, label = label), check_overlap = T, hjust = 0, vjust = 1, family = font, size = 9/.pt) +
  geom_curve(data = ann_arrows, aes(x = x, y = y, xend = xend, yend = yend),
             arrow = arrow(length = unit(0.08, "inch")), linewidth = 0.5, curvature = -0.3) +
  coord_fixed(ratio = 1) +
  labs(title = "Are we singing the song title?",
       subtitle = "Yes, yes we are (at least Taylor Swift is: the exception are 2 out of 179 songs)",
       caption = paste0("#TidyTuesday week 42 2023 | Data: {taylor} R package | Plot: Pedro Borralho")) +
  theme_void() +
  theme(strip.text = element_blank(),
        legend.position = c(0.4, 1.05), legend.direction = 'horizontal',
        legend.title = element_blank(), legend.box.margin = margin(t = 20, b = 20),
        legend.text = element_text(family = font, size = 10),
        plot.title = element_text(family = font, size = 25, hjust = 0.32, color = "white"),
        plot.subtitle = element_text(family = font, size = 10, hjust = 0.32, margin = margin(t = 10, b = 50), color = "white"),
        plot.caption = element_text(family = font, size = 10, hjust = 0.32, margin = margin(t = 20, b = 40)),
        text = element_text(family = font),
        plot.margin = unit(c(1, -1.2, 1, 1.5), "cm"),
        plot.background = element_rect(fill = "#AA9EB6", color = NA))

# save plot
ggsave(paste0("taylor_swift_", format(Sys.time(), "%Y%m%d"), ".png"), dpi = 320, width = 8, height = 8)





library(tidytuesdayR)
library(stringr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

###############################################################################################################################################################################################

tuesdata <- tidytuesdayR::tt_load(2023, week = 40)
grants <- tuesdata$grants
grant_opportunity_details <- tuesdata$grant_opportunity_details

###############################################################################################################################################################################################

grants %>%
  filter(!is.na(estimated_funding) & estimated_funding > 0) %>%
  mutate(posted_year = year(posted_date)) %>%
  group_by(posted_year) %>%
  summarise(sum_funding = sum(estimated_funding)) %>%
  ggplot(aes(x = posted_year, y = sum_funding)) +
  geom_col()

###############################################################################################################################################################################################

grants_AI <- grants %>% left_join(grant_opportunity_details %>% select(opportunity_id, description), by = "opportunity_id") %>%
  # filter(str_detect(description %>% tolower, "artificial intelligence|machine learning|data science")) %>%
  filter(str_detect(description %>% tolower, "artificial intelligence")) %>%
  filter(!is.na(description)) %>%
  filter(!is.na(estimated_funding) & estimated_funding > 0) %>%
  mutate(posted_year = year(posted_date))

grants_AI %>%
  group_by(posted_year) %>%
  summarise(sum_funding = sum(estimated_funding)) %>%
  ggplot(aes(x = posted_year, y = sum_funding)) +
  geom_col()

################################

grants_climate <- grants %>% left_join(grant_opportunity_details %>% select(opportunity_id, description), by = "opportunity_id") %>%
  filter(str_detect(description %>% tolower, "climate") | str_detect(opportunity_title %>% tolower, "climate")) %>%
  filter(!is.na(estimated_funding) & estimated_funding > 0) %>%
  mutate(posted_year = year(posted_date))

grants_climate %>%
  group_by(posted_year) %>%
  summarise(sum_funding = sum(estimated_funding)) %>%
  ggplot(aes(x = posted_year, y = sum_funding)) +
  geom_col()

###############################################################################################################################################################################################

# AI_topics <- "artificial intelligence|machine learning|big data|data science|deep learning"
AI_topics <- "artificial intelligence"

df <- grants %>% left_join(grant_opportunity_details %>% select(opportunity_id, description), by = "opportunity_id") %>%
  filter(!is.na(estimated_funding) & estimated_funding > 0) %>%
  mutate(posted_year = year(posted_date),
         climate = (str_detect(description %>% tolower, "climate") & !is.na(description)) | (str_detect(opportunity_title %>% tolower, "climate") & !is.na(opportunity_title)),
         AI = (str_detect(description %>% tolower, AI_topics) & !is.na(description)) | (str_detect(opportunity_title %>% tolower, AI_topics) & !is.na(opportunity_title)))
table(df$AI, useNA = "always")

df %>%
  group_by(posted_year, climate) %>%
  summarise(sum_funding = sum(estimated_funding)) %>%
  ggplot(aes(x = posted_year, y = sum_funding, fill = climate)) +
  geom_col()

df %>%
  group_by(posted_year) %>%
  summarise(sum_funding = sum(estimated_funding),
            sum_funding_climate = sum(estimated_funding[climate])) %>%
  ungroup() %>%
  mutate(ratio_climate = sum_funding_climate/sum_funding)

df %>%
  filter(climate) %>%
  group_by(posted_year) %>%
  summarise(sum_funding = sum(estimated_funding)) %>%
  ggplot(aes(x = posted_year, y = sum_funding)) +
  geom_col()

df %>%
  filter(AI) %>%
  group_by(posted_year) %>%
  summarise(sum_funding = sum(estimated_funding)) %>%
  ggplot(aes(x = posted_year, y = sum_funding)) +
  geom_col()

df %>%
  filter(AI) %>%
  group_by(posted_year) %>%
  summarise(sum_funding = sum(estimated_funding)) %>%
  ggplot(aes(x = posted_year, y = sum_funding)) +
  geom_line()


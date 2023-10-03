
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
# # grants: relevant atts
# opportunity_id
# opportunity_number + opportunity_title
# agency_code + agency_name
# estimated_funding

# # grant_opportunity_details: relevant atts
# opportunity_id
# funding_opportunity_number + funding_opportunity_title
# estimated_total_program_funding
# agency_name
# description
# category_x + category_explanation

###############################################################################################################################################################################################

# categ_cols <- colnames(grant_opportunity_details)[colnames(grant_opportunity_details) %>% substr(1, 8) == "category"]
# cols <- c("opportunity_id", categ_cols %>% setdiff("category_explanation"))
# categs_by_grant <- grant_opportunity_details[, cols] %>% gather(category, bool, -1) %>% filter(bool) %>% select(-bool)
# # there are grants with more than one category
# table(categs_by_grant$category, useNA = "always") %>% sort(decreasing = T)
# 
# df <- merge(categs_by_grant,
#             grant_opportunity_details %>% select(opportunity_id, estimated_total_program_funding, posted_date),
#             by = "opportunity_id", all.x = T) %>%
#   filter(!is.na(estimated_total_program_funding) & estimated_total_program_funding > 0) %>%
#   mutate(posted_year = year(posted_date))

###############################################################################################################################################################################################

grant_opportunity_details %>%
  filter(!is.na(estimated_total_program_funding) & estimated_total_program_funding > 0) %>%
  mutate(posted_year = year(posted_date)) %>%
  group_by(posted_year) %>%
  summarise(sum_funding = sum(estimated_total_program_funding)) %>%
  ggplot(aes(x = posted_year, y = sum_funding)) +
  geom_col()

grants %>%
  filter(!is.na(estimated_funding) & estimated_funding > 0) %>%
  mutate(posted_year = year(posted_date)) %>%
  group_by(posted_year) %>%
  summarise(sum_funding = sum(estimated_funding)) %>%
  ggplot(aes(x = posted_year, y = sum_funding)) +
  geom_col()

grant_opportunity_details %>%
  filter(str_detect(description, "artificial intelligence|machine learning|data science|neural networks")) %>%
  filter(!is.na(estimated_total_program_funding) & estimated_total_program_funding > 0) %>%
  mutate(posted_year = year(posted_date)) %>%
  group_by(posted_year) %>%
  summarise(sum_funding = sum(estimated_total_program_funding)) %>%
  ggplot(aes(x = posted_year, y = sum_funding)) +
  geom_col()

###############################################################################################################################################################################################

grants_AI <- grants %>% left_join(grant_opportunity_details %>% select(opportunity_id, description), by = "opportunity_id") %>%
  filter(str_detect(description %>% tolower, "artificial intelligence|machine learning|data science")) %>%
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





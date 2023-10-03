
library(tidytuesdayR)
library(stringr)
library(dplyr)
library(tidyr)
library(lubridate)

###############################################################################################################################################################################################

tuesdata <- tidytuesdayR::tt_load(2023, week = 40)

grants <- tuesdata$grants
grant_opportunity_details <- tuesdata$grant_opportunity_details

rm(tuesdata)

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

categ_cols <- colnames(grant_opportunity_details)[colnames(grant_opportunity_details) %>% substr(1, 8) == "category"]
cols <- c("opportunity_id", categ_cols %>% setdiff("category_explanation"))
categs_by_grant <- grant_opportunity_details[, cols] %>% gather(category, bool, -1) %>% filter(bool) %>% select(-bool)
# there are grants with more than one category
table(categs_by_grant$category, useNA = "always") %>% sort(decreasing = T)

#####################################

check_funds <- merge(grants %>% select(opportunity_id, estimated_funding),
                     grant_opportunity_details %>% select(opportunity_id, estimated_total_program_funding),
                     by = "opportunity_id", all = T)

check_funds <- check_funds %>% mutate(diff = estimated_total_program_funding - estimated_funding)

table(is.na(check_funds$estimated_funding), is.na(check_funds$estimated_total_program_funding))

#####################################

# grants <- merge(grants, categs_by_grant, by = "opportunity_id", all = T)
# sum(duplicated(grants$opportunity_id))
# View(grants[duplicated(grants$opportunity_id) | duplicated(grants$opportunity_id, fromLast = T), ])

categs_by_grant <- merge(categs_by_grant, grants %>% select(opportunity_id, estimated_funding), by = "opportunity_id", all = T)

plot(categs_by_grant %>% select(-1) %>% filter(!is.na(estimated_funding) & !is.na(category)))

###############################################################################################################################################################################################

AI_grants <- grant_opportunity_details %>% filter(str_detect(description, "artificial intelligence"))
plot(AI_grants$posted_date)
plot(AI_grants$posted_date, AI_grants$estimated_total_program_funding)

summary(AI_grants$estimated_total_program_funding)

AI_grants2 <- AI_grants %>% mutate(posted_year = year(posted_date)) %>% group_by(posted_year) %>% summarise(estimated_total_program_funding = sum(estimated_total_program_funding, na.rm = T))
plot(AI_grants2)


AI_grants3 <- AI_grants %>% mutate(posted_year = year(posted_date)) %>% group_by(posted_year) %>% summarise(n_estimated_total_program_funding = n())
plot(AI_grants3)



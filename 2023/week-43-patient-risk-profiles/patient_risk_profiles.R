
library(tidytuesdayR)
library(dplyr)
library(stringr)
library(tidyr)
# library(ggplot2)
# library(showtext)

################################################################################################
# load data

tuesdata <- tidytuesdayR::tt_load(2023, week = 43)
patient_risk_profiles <- tuesdata$patient_risk_profiles

################################################################################################
# prepare data

age_cols <- colnames(patient_risk_profiles)[colnames(patient_risk_profiles) %>% str_detect("age group")]

patient_risk_profiles <- patient_risk_profiles %>%
  pivot_longer(age_cols, names_to = "age_group", values_to = "age_bool") %>%
  filter(age_bool == 1) %>% select(-age_bool)

patient_risk_profiles <- patient_risk_profiles %>%
  mutate(age_group = age_group %>% str_split(":") %>% sapply("[[", 2) %>% str_squish,
         age_group_min = age_group %>% str_split("-") %>% sapply("[[", 1) %>% as.numeric,
         age_group_max = age_group %>% str_split("-") %>% sapply("[[", 2) %>% as.numeric)
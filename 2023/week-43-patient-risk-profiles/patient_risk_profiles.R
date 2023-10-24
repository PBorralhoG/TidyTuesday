
library(tidytuesdayR)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
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
         age_group_max = age_group %>% str_split("-") %>% sapply("[[", 2) %>% as.numeric,
         age_group_mean = (age_group_min + age_group_max) / 2)

################################################################################################

plot(patient_risk_profiles$age_group_mean, patient_risk_profiles$`predicted risk of Dementia`)
plot(patient_risk_profiles$age_group_mean, patient_risk_profiles$`predicted risk of Migraine`)
plot(patient_risk_profiles$age_group_mean, patient_risk_profiles$`predicted risk of Parkinson's disease, inpatient or with 2nd diagnosis`)
plot(patient_risk_profiles$age_group_mean, patient_risk_profiles$`predicted risk of Multiple Sclerosis`)

patient_risk_profiles <- patient_risk_profiles %>% mutate(pred_risk = `predicted risk of Migraine`)

ggplot(patient_risk_profiles, aes(x = age_group_mean, y = pred_risk)) +
  geom_point()

risk_by_age_group <- patient_risk_profiles %>%
  group_by(age_group) %>%
  summarise(pred_risk_min = min(pred_risk),
            pred_risk_min2 = pred_risk %>% sort %>% nth(2),
            pred_risk_p5 = quantile(pred_risk, 0.05),
            pred_risk_p50 = quantile(pred_risk, 0.50),
            pred_risk_p95 = quantile(pred_risk, 0.95),
            pred_risk_max2 = pred_risk %>% sort(decreasing = T) %>% nth(2),
            pred_risk_max = max(pred_risk),
            age_group_min = unique(age_group_min),
            age_group_mean = unique(age_group_mean),
            age_group_max = unique(age_group_max))

ggplot() +
  geom_rect(data = risk_by_age_group, aes(xmin = age_group_min, xmax = age_group_max+1, ymin = pred_risk_min, ymax = pred_risk_max), color = "black") +
  geom_rect(data = risk_by_age_group, aes(xmin = age_group_min, xmax = age_group_max+1, ymin = pred_risk_p5, ymax = pred_risk_p95), fill = alpha("purple", 0.5), color = "black") +
  geom_rect(data = risk_by_age_group, aes(xmin = age_group_min, xmax = age_group_max+1, ymin = pred_risk_min2, ymax = pred_risk_max2), fill = alpha("red", 0.5), color = "black") +
  geom_point(data = patient_risk_profiles, aes(x = age_group_mean+0.5, y = pred_risk)) +
  geom_point(data = risk_by_age_group, aes(x = age_group_mean+0.5, y = pred_risk_p50), shape = 4, size = 3, stroke = 2)








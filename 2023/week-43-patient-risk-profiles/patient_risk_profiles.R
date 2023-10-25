
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

patient_risk_profiles <- patient_risk_profiles %>%
  pivot_longer(cols = starts_with("age group"), names_to = "age_group", values_to = "age_bool", names_prefix = "age group:") %>%
  filter(age_bool == 1) %>% select(-age_bool) %>% mutate(age_group = str_squish(age_group)) %>%
  pivot_longer(cols = starts_with("predicted risk of"), names_to = "pred_risk", values_to = "pred_risk_value", names_prefix = "predicted risk of") %>%
  mutate(pred_risk = str_squish(pred_risk)) %>%
  mutate(age_group_min = age_group %>% str_split("-") %>% sapply("[[", 1) %>% as.numeric,
         age_group_max = age_group %>% str_split("-") %>% sapply("[[", 2) %>% as.numeric,
         age_group_mean = (age_group_min + age_group_max) / 2)

################################################################################################

ggplot(patient_risk_profiles %>% filter(pred_risk == "Dementia"),
       aes(x = age_group_mean, y = pred_risk_value)) +
  geom_point()

risk_by_age_group <- patient_risk_profiles %>%
  group_by(age_group, pred_risk) %>%
  summarise(pred_risk_min = min(pred_risk_value),
            pred_risk_min2 = pred_risk_value %>% sort %>% nth(2),
            pred_risk_p5 = quantile(pred_risk_value, 0.05),
            pred_risk_med = quantile(pred_risk_value, 0.50),
            pred_risk_p95 = quantile(pred_risk_value, 0.95),
            pred_risk_max2 = pred_risk_value %>% sort(decreasing = T) %>% nth(2),
            pred_risk_max = max(pred_risk_value),
            age_group_min = unique(age_group_min),
            age_group_mean = unique(age_group_mean),
            age_group_max = unique(age_group_max))

# ggplot() +
#   geom_rect(data = risk_by_age_group, aes(xmin = age_group_min, xmax = age_group_max+1, ymin = pred_risk_min, ymax = pred_risk_max), color = "black") +
#   geom_rect(data = risk_by_age_group, aes(xmin = age_group_min, xmax = age_group_max+1, ymin = pred_risk_p5, ymax = pred_risk_p95), fill = alpha("purple", 0.5), color = "black") +
#   geom_rect(data = risk_by_age_group, aes(xmin = age_group_min, xmax = age_group_max+1, ymin = pred_risk_min2, ymax = pred_risk_max2), fill = alpha("red", 0.5), color = "black") +
#   geom_point(data = patient_risk_profiles, aes(x = age_group_mean+0.5, y = pred_risk_value)) +
#   geom_point(data = risk_by_age_group, aes(x = age_group_mean+0.5, y = pred_risk_p50), shape = 4, size = 3, stroke = 2)

ggplot(risk_by_age_group, aes(x = age_group_mean, y = pred_risk_med, group = pred_risk)) +
  geom_line()

ggplot(risk_by_age_group, aes(x = age_group_mean, y = pred_risk_med, group = pred_risk)) +
  geom_line() +
  facet_wrap(~pred_risk, scales = "free")






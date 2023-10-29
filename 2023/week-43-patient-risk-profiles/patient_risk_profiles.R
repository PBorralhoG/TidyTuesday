
library(tidytuesdayR)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(ggtext)
# library(showtext)

################################################################################################
# load data

tuesdata <- tidytuesdayR::tt_load(2023, week = 43)
patient_risk_profiles <- patient_risk_profiles0 <- tuesdata$patient_risk_profiles

################################################################################################
# prepare data

patient_risk_profiles <- patient_risk_profiles %>%
  pivot_longer(cols = starts_with("age group"), names_to = "age_group", values_to = "age_bool", names_prefix = "age group:") %>%
  filter(age_bool == 1) %>% select(-age_bool) %>% mutate(age_group = str_squish(age_group)) %>%
  pivot_longer(cols = starts_with("predicted risk of"), names_to = "disease", values_to = "pred_risk", names_prefix = "predicted risk of") %>%
  mutate(disease = str_squish(disease)) %>%
  mutate(age_group_min = age_group %>% str_split("-") %>% sapply("[[", 1) %>% as.numeric,
         age_group_max = age_group %>% str_split("-") %>% sapply("[[", 2) %>% as.numeric,
         age_group_mean = (age_group_min + age_group_max) / 2)

risk_by_age_group <- patient_risk_profiles %>%
  group_by(age_group, disease) %>%
  summarise(pred_risk_min = min(pred_risk),
            pred_risk_p5 = quantile(pred_risk, 0.05),
            pred_risk_p25 = quantile(pred_risk, 0.25),
            pred_risk_med = quantile(pred_risk, 0.50),
            pred_risk_p75 = quantile(pred_risk, 0.75),
            pred_risk_p95 = quantile(pred_risk, 0.95),
            pred_risk_max = max(pred_risk),
            age_group_min = unique(age_group_min),
            age_group_mean = unique(age_group_mean),
            age_group_max = unique(age_group_max))

################################################################################################
# plot

risk_by_age_group2 <- risk_by_age_group %>% filter(disease %in% c("Dementia", "Migraine"))
risk_by_age_group2 <- rbind(
  risk_by_age_group2 %>% select(disease, age_group = age_group_min, pred_risk_min, pred_risk_p5, pred_risk_p25, pred_risk_med, pred_risk_p75, pred_risk_p95, pred_risk_max),
  risk_by_age_group2 %>% select(disease, age_group = age_group_max, pred_risk_min, pred_risk_p5, pred_risk_p25, pred_risk_med, pred_risk_p75, pred_risk_p95, pred_risk_max))

axis_lines <- rbind(data.frame(x = 0, xend = 94, y = 0.07*1.2, yend = 0.07*1.2, disease = "Migraine"),
                    data.frame(x = 97, xend = 97, y = 0, yend = 0.4, disease = "Dementia"),
                    data.frame(x = -3, xend = -3, y = 0, yend = 0.07, disease = "Migraine"))

axis_text <- rbind(data.frame(x = 97, y = 0, label = "0%", disease = "Dementia"),
                   data.frame(x = 97, y = 0.4, label = "40%", disease = "Dementia"),
                   data.frame(x = -3, y = 0, label = "0%", disease = "Migraine"),
                   data.frame(x = -3, y = 0.07, label = "7%", disease = "Migraine"),
                   data.frame(x = 0, y = 0.07*1.2, label = "0", disease = "Migraine"),
                   data.frame(x = 94/2, y = 0.07*1.2, label = "Age", disease = "Migraine"),
                   data.frame(x = 94, y = 0.07*1.2, label = "94", disease = "Migraine"),
                   data.frame(x = 94*0.4, y = 0.4*0.6, label = "Dementia", disease = "Dementia"),
                   data.frame(x = 94*0.6, y = 0.07*0.6, label = "Migraine", disease = "Migraine"))

axis_text_rot <- rbind(data.frame(x = 97, y = 0.4/2, label = "Predicted Risk", angle = -90, disease = "Dementia"),
                       data.frame(x = -3, y = 0.07/2, label = "Predicted Risk", angle = 90, disease = "Migraine"))

ggplot(risk_by_age_group2) +
  geom_ribbon(aes(x = age_group, ymin = pred_risk_min, ymax = pred_risk_max), fill = grey(0.8)) +
  geom_ribbon(aes(x = age_group, ymin = pred_risk_p5, ymax = pred_risk_p95), fill = grey(0.6)) +
  geom_ribbon(aes(x = age_group, ymin = pred_risk_p25, ymax = pred_risk_p75), fill = grey(0.4)) +
  geom_line(aes(x = age_group, y = pred_risk_med), color = grey(0.2)) +
  facet_wrap(~disease, scales = "free", nrow = 2) +
  scale_x_continuous(limits = c(-3, 97)) +
  theme_void() +
  geom_segment(data = axis_lines, aes(x = x, y = y, xend = xend, yend = yend), linewidth = 0.5) +
  geom_richtext(data = axis_text, aes(x = x, y = y, label = label), label.colour = NA) +
  geom_richtext(data = axis_text_rot, aes(x = x, y = y, label = label, angle = angle), label.colour = NA) +
  theme(strip.text = element_blank())



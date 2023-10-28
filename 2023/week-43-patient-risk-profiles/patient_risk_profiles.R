
library(tidytuesdayR)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
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
            pred_risk_min2 = pred_risk %>% sort %>% nth(2),
            pred_risk_p5 = quantile(pred_risk, 0.05),
            pred_risk_med = quantile(pred_risk, 0.50),
            pred_risk_p95 = quantile(pred_risk, 0.95),
            pred_risk_max2 = pred_risk %>% sort(decreasing = T) %>% nth(2),
            pred_risk_max = max(pred_risk),
            age_group_min = unique(age_group_min),
            age_group_mean = unique(age_group_mean),
            age_group_max = unique(age_group_max))

risk_by_age_group <- risk_by_age_group %>%
  group_by(disease) %>%
  mutate(pred_risk_med_max = max(pred_risk_med),
         pred_risk_med_norm = pred_risk_med / pred_risk_med_max,
         pred_risk_med_med = median(pred_risk_med),
         pred_risk_med_norm2 = pred_risk_med / pred_risk_med_med)

################################################################################################

ggplot(patient_risk_profiles %>% filter(disease == "Dementia"), aes(x = age_group_mean, y = pred_risk)) +
  geom_point()

ggplot() +
  # geom_rect(data = risk_by_age_group, aes(xmin = age_group_min, xmax = age_group_max+1, ymin = pred_risk_min, ymax = pred_risk_max), color = "black") +
  geom_rect(data = risk_by_age_group, aes(xmin = age_group_min, xmax = age_group_max+1, ymin = pred_risk_p5, ymax = pred_risk_p95), fill = alpha("purple", 0.5), color = "black") +
  # geom_rect(data = risk_by_age_group, aes(xmin = age_group_min, xmax = age_group_max+1, ymin = pred_risk_min2, ymax = pred_risk_max2), fill = alpha("red", 0.5), color = "black") +
  geom_point(data = patient_risk_profiles, aes(x = age_group_mean+0.5, y = pred_risk)) +
  # geom_point(data = risk_by_age_group, aes(x = age_group_mean+0.5, y = pred_risk_med), shape = 4, size = 3, stroke = 2) +
  facet_wrap(~disease, scales = "free")

#############################

ggplot(risk_by_age_group, aes(x = age_group_mean, y = pred_risk_med, group = disease)) +
  # geom_line() +
  geom_smooth(se = FALSE, linewidth = 1)

ggplot(risk_by_age_group, aes(x = age_group_mean, y = pred_risk_med, colour = disease)) +
  # geom_line() +
  geom_smooth(se = FALSE, linewidth = 1) +
  gghighlight::gghighlight(use_direct_label = F, unhighlighted_params = list(colour = "grey")) +
  facet_wrap(~disease) +
  theme(legend.position = "none")

ggplot(risk_by_age_group, aes(x = age_group_mean, y = pred_risk_med_norm, colour = disease)) +
  # geom_line(lwd = 1) +
  geom_smooth(se = FALSE, linewidth = 1) +
  gghighlight::gghighlight(use_direct_label = F, unhighlighted_params = list(colour = grey(0.85))) +
  facet_wrap(~disease) +
  theme(legend.position = "none")

################################################################################################

table(FEMALE = patient_risk_profiles0$`Sex = FEMALE`, MALE = patient_risk_profiles0$`Sex = MALE`)

patient_risk_profiles %>% select(personId, age_group) %>% unique %>% pull(age_group) %>% table
xx <- patient_risk_profiles %>% select(personId, age_group_mean) %>% unique %>% pull(age_group_mean) %>% table
plot(xx)

################################################################################################

risk_by_age_group %>% filter(disease %in% c("Dementia", "Migraine")) %>%
  ggplot() +
  geom_ribbon(aes(x = age_group_mean, ymin = pred_risk_min, ymax = pred_risk_max), fill = grey(0.8)) +
  geom_ribbon(aes(x = age_group_mean, ymin = pred_risk_p5, ymax = pred_risk_p95), fill = grey(0.6)) +
  geom_line(aes(x = age_group_mean, y = pred_risk_med), color = grey(0.2)) +
  facet_wrap(~disease, scales = "free", nrow = 2)

risk_by_age_group2 <- risk_by_age_group %>% filter(disease %in% c("Dementia", "Migraine"))
risk_by_age_group2 <- rbind(
  risk_by_age_group2 %>% select(disease, age_group = age_group_min, pred_risk_min, pred_risk_p5, pred_risk_med, pred_risk_p95, pred_risk_max),
  risk_by_age_group2 %>% select(disease, age_group = age_group_max, pred_risk_min, pred_risk_p5, pred_risk_med, pred_risk_p95, pred_risk_max))

risk_by_age_group2 %>%
  ggplot() +
  geom_ribbon(aes(x = age_group, ymin = pred_risk_min, ymax = pred_risk_max), fill = grey(0.8)) +
  geom_ribbon(aes(x = age_group, ymin = pred_risk_p5, ymax = pred_risk_p95), fill = grey(0.6)) +
  geom_line(aes(x = age_group, y = pred_risk_med), color = grey(0.2)) +
  facet_wrap(~disease, scales = "free", nrow = 2)
  



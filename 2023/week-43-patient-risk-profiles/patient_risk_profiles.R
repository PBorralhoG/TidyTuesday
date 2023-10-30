
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
  risk_by_age_group2 %>% mutate(age_group_max = age_group_max+0.99) %>% select(disease, age_group = age_group_max, pred_risk_min, pred_risk_p5, pred_risk_p25, pred_risk_med, pred_risk_p75, pred_risk_p95, pred_risk_max))

axis_lines <- rbind(data.frame(x = 0, xend = 94.5, y = 0.07*1.1, yend = 0.07*1.1, disease = "Migraine"),
                    data.frame(x = -3, xend = -3, y = 0, yend = 0.4, disease = "Dementia"),
                    data.frame(x = -3, xend = -3, y = 0, yend = 0.07, disease = "Migraine"))

axis_text <- rbind(data.frame(x = -3, y = 0, label = "0%", disease = "Dementia", angle = 0),
                   data.frame(x = -3, y = 0.4/2, label = "Predicted Risk", disease = "Dementia", angle = 90),
                   data.frame(x = -3, y = 0.4, label = "40%", disease = "Dementia", angle = 0),
                   data.frame(x = -3, y = 0, label = "0%", disease = "Migraine", angle = 0),
                   data.frame(x = -3, y = 0.07/2, label = "Predicted Risk", disease = "Migraine", angle = 90),
                   data.frame(x = -3, y = 0.07, label = "7%", disease = "Migraine", angle = 0),
                   data.frame(x = 0, y = 0.07*1.1, label = "0", disease = "Migraine", angle = 0),
                   data.frame(x = 94/2, y = 0.07*1.1, label = "Age", disease = "Migraine", angle = 0),
                   data.frame(x = 94.5, y = 0.07*1.1, label = "94", disease = "Migraine", angle = 0))

facet_text <- rbind(data.frame(x = 94*0.5, y = 0.4*0.6, label = "**Dementia**", disease = "Dementia", angle = 0),
                    data.frame(x = 94*0.5, y = 0.07*0.6, label = "**Migraine**", disease = "Migraine", angle = 0))

aux <- risk_by_age_group2 %>% filter(age_group == max(age_group) & disease == "Dementia")

leg_lines <- rbind(data.frame(x = 99, xend = 99, y = aux$pred_risk_p25, yend = aux$pred_risk_p75, disease = "Dementia"),
                   data.frame(x = 98.5, xend = 99, y = aux$pred_risk_p25, yend = aux$pred_risk_p25, disease = "Dementia"),
                   data.frame(x = 98.5, xend = 99, y = aux$pred_risk_p75, yend = aux$pred_risk_p75, disease = "Dementia"),
                   data.frame(x = 96, xend = 96, y = aux$pred_risk_p5, yend = aux$pred_risk_p95, disease = "Dementia"),
                   data.frame(x = 95.5, xend = 96, y = aux$pred_risk_p5, yend = aux$pred_risk_p5, disease = "Dementia"),
                   data.frame(x = 95.5, xend = 96, y = aux$pred_risk_p95, yend = aux$pred_risk_p95, disease = "Dementia"))

leg_text <- rbind(data.frame(x = 97, y = aux$pred_risk_med, label = "90% of patients", angle = -90, disease = "Dementia"),
                  data.frame(x = 100, y = aux$pred_risk_med, label = "50% of patients", angle = -90, disease = "Dementia"))

axis_color <- "#6F4E37"

ggplot(risk_by_age_group2) +
  geom_ribbon(aes(x = age_group, ymin = pred_risk_min, ymax = pred_risk_max), fill = grey(0.9)) +
  geom_ribbon(aes(x = age_group, ymin = pred_risk_p5, ymax = pred_risk_p95), fill = grey(0.7)) +
  geom_ribbon(aes(x = age_group, ymin = pred_risk_p25, ymax = pred_risk_p75), fill = grey(0.4)) +
  geom_line(aes(x = age_group, y = pred_risk_med), color = grey(0.2)) +
  facet_wrap(~disease, scales = "free", nrow = 2) +
  scale_x_continuous(limits = c(-3, 100)) +
  geom_segment(data = axis_lines, aes(x = x, y = y, xend = xend, yend = yend), linewidth = 0.5, color = axis_color) +
  geom_richtext(data = axis_text, aes(x = x, y = y, label = label, angle = angle), fill = "white", label.colour = NA, size = 9/.pt, color = axis_color) +
  # geom_text(data = facet_text, aes(x = x, y = y, label = label, angle = angle), hjust = 0.5, size = 12/.pt) +
  geom_richtext(data = facet_text, aes(x = x, y = y, label = label, angle = angle), hjust = 0.5, size = 12/.pt, fill = NA, label.colour = NA, color = axis_color) +
  geom_segment(data = leg_lines, aes(x = x, y = y, xend = xend, yend = yend), linewidth = 0.5, color = axis_color) +
  geom_text(data = leg_text, aes(x = x, y = y, label = label, angle = angle), hjust = 0.4, size = 9/.pt, color = axis_color) +
  labs(title = "Patient Risk Profiles",
       subtitle = "Predicted risk of Dementia and Migraine by age of 100 simulated patients",
       caption = paste0("#TidyTuesday week 43 2023 | Data: R/Pharma | Plot: Pedro Borralho")) +
  theme_void() +
  theme(strip.text = element_blank(),
        plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 10, hjust = 0.5, margin = margin(t = 10, b = 40)),
        plot.caption = element_text(size = 9, hjust = 0.5, margin = margin(t = 40)),
        plot.margin = unit(c(1, 0.5, 1, 0.5), "cm"),
        plot.background = element_rect(fill = "white", color = NA))

# save plot
ggsave(paste0("2023/week-43-patient-risk-profiles/patient_risk_", format(Sys.time(), "%Y%m%d"), ".png"),
       dpi = 320, width = 8, height = 8)


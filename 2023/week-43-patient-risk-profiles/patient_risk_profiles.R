
library(tidytuesdayR)
# library(dplyr)
# library(stringr)
# library(ggplot2)
# library(showtext)

################################################################################################
# load data

tuesdata <- tidytuesdayR::tt_load(2023, week = 43)
patient_risk_profiles <- tuesdata$patient_risk_profiles

################################################################################################
# prepare data


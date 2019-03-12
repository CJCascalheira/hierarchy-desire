# Dependencies
library(tidyverse)

# Import raw data
survey <- read_csv("data/raw/initial-attraction-online.csv")

# Drop collector ID, email, first name, last name, custom data 1
survey1 <- survey[-1, -c(2, 5:8)]

# Select radio button variables
survey_radio <- survey1[, -c(9:35, 41:46, 67:106)]

# Select dummy choice variables
survey_dummy <- survey1[, c(1, 9:35, 41:46, 67:106)]

# Clean variable names for radio button variables
radio_clean <- c("id", "start", "end", "consent", "sex_orient", "gender", "single",
                 "out", "pic", "age", "body", "ethnicity", "sex_position", "si_popular",
                 "si_racialgrp", "si_react", "si_appearance", "si_masc", "si_reputation",
                 "si_attractive", "si_skincolor", "si_gestures", "si_socialbhvr", "fc_1",
                 "fc_2", "fc_3", "fc_4", "fc_5", "fc_6", "fc_7", "fc_8", "fc_9", "fc_10")

# Clean variable names for dummy choice variables
dummy_clean <- c("id", "pa_a4a", "pa_dh")
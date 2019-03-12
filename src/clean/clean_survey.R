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

# CLEAN NAMES -------------------------------------------------------------

# Clean variable names for radio button variables
radio_clean <- c("id", "start", "end", "consent", "sex_orient", "gender", "single",
                 "out", "pic", "age", "body", "ethnicity", "sex_position", "si_popular",
                 "si_racialgrp", "si_react", "si_appearance", "si_masc", "si_reputation",
                 "si_attractive", "si_skincolor", "si_gestures", "si_socialbhvr", "fc_1",
                 "fc_2", "fc_3", "fc_4", "fc_5", "fc_6", "fc_7", "fc_8", "fc_9", "fc_10")

# Clean variable names for dummy choice variables
dummy_clean <- c("id", "pa_a4a", "pa_dh", "pa_dsc", "pa_grdr", "pa_grzz", "pa_grwl", "pa_hrnt",
                 "pa_jckd", "pa_okc", "pa_scf", "pa_srg", "pa_tdr", "pa_other", "ca_a4a", "ca_dh",
                 "ca_dsc", "ca_grdr", "ca_grzz", "ca_grwl", "ca_hrnt", "ca_jckd", "ca_okc",
                 "ca_scf", "ca_srg", "ca_tdr", "ca_other", "ca_none", "hi_profpic", "hi_age",
                 "hi_body", "hi_ethnty", "hi_sexpost", "hi_none", "black-hm-03", "latino-em-03",
                 "black-hm-02", "asian-hm-04", "asian-em-04", "white-hm-04", "black-em-05", "asian-em-03",
                 "latino-hm-01", "black-em-01", "white-em-05", "white-hm-03", "black-hm-05", "black-em-02",
                 "white-hm-02", "black-hm-04", "white-em-01", "black-em-03", "white-em-03", "asian-hm-02",
                 "asian-hm-05", "latino-em-01", "latino-em-04", "asian-em-02", "black-hm-01", "asian-em-01",
                 "latino-hm-02", "white-em-02", "white-hm-01", "latino-hm-05", "latino-em-05", "white-hm-05",
                 "latino-hm-04", "latino-hm-03", "asian-em-05", "asian-hm-01", "black-em-04", "latino-em-02",
                 "asian-hm-03", "white-em-04")

# Assign clean names
names(survey_radio) <- radio_clean
names(survey_dummy) <- dummy_clean

# REMOVE NO CONSENT & NA --------------------------------------------------

# No consent
no_consent <- survey_radio %>%
  filter(consent != "I agree") %>%
  select(id) %>%
  pull()

# Incomplete surveys
which(is.na(su))
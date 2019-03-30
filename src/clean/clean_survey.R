# Dependencies
library(tidyverse)
library(lubridate)

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

# REMOVE NO CONSENT & INCOMPLETE ------------------------------------------

# No consent
no_consent <- survey_radio %>%
  filter(consent != "I agree") %>%
  select(id) %>%
  pull()

# Remove non-consenting participants from analysis
survey_radio2 <- survey_radio %>%
  filter(!c(id %in% no_consent))

survey_dummy2 <- survey_dummy %>%
  filter(!c(id %in% no_consent))

# Missing response for forced choice image selection 10
no_fc_10 <- which(is.na(survey_radio2$fc_10))

# Which participants did not answer fc_10?
incomplete <- survey_radio2[no_fc_10, ] %>%
  select(id) %>%
  pull()

# Remove incomplete observations
survey_radio3 <- survey_radio2 %>%
  filter(!(id %in% incomplete))

survey_dummy3 <- survey_dummy2 %>%
  filter(!(id %in% incomplete))

# Are any responses missing entirely from the final, free response section?
complete <- survey_dummy3[, c(1, 35:74)] %>%
  gather(key = photo, value = choice, -id) %>%
  filter(!(choice %in% c("Image 1", "Image 2", "Image 3", "Image 4", "Image 5",
                         "Image 6", "Image 7", "Image 8", "Image 9", "Image 10"))) %>%
  mutate(choice = replace(choice, is.na(choice), "none")) %>%
  spread(key = photo, value = choice) %>%
  gather(key = photo, value = choice, -id) %>%
  filter(is.na(choice)) %>%
  mutate(choice = replace(choice, is.na(choice), "none")) %>%
  spread(key = photo, value = choice) %>%
  select(id) %>%
  pull()

# Are all incomplete observations removed?
map(survey_radio3, is.na) %>%
  map(sum) %>%
  bind_rows() %>%
  rowSums()

survey_dummy3[, c(1, 35:74)] %>%
  filter(!(id %in% complete)) %>%
  View()

# Participants who did not complete free response section
incomplete2 <- survey_dummy3[, c(1, 35:74)] %>%
  filter(!(id %in% complete)) %>%
  select(id) %>%
  pull()

# Remove remaining incomplete observations
survey_radio3 <- survey_radio3 %>%
  filter(!(id %in% incomplete2))

survey_dummy3 <- survey_dummy3 %>%
  filter(!(id %in% incomplete2))

# Equal identification?
identical(survey_radio3$id, survey_dummy3$id)

# MISSING VALUES & RECODING VALUES ----------------------------------------

# Prepare new data frame
survey_radio4 <- survey_radio3

# Replace all missing values across all variables
survey_dummy4 <- survey_dummy3 %>%
  replace(., is.na(.), "none")

# Replace all Image X with photo names
for (i in 35:74) {
  survey_dummy4[i] <- str_replace(string = survey_dummy4[i] %>% pull(),
                                  pattern = "^([a-zA-Z]{5}[:blank:]*[:digit:]*)",
                                  replacement = names(survey_dummy4)[i])
}

# Subset id for datetimes
no_seconds <- survey_radio3[c(1:56, 261:348), ] %>%
  select(id) %>%
  pull()

yes_seconds <- survey_radio3[-c(1:56, 261:348), ] %>%
  select(id) %>%
  pull()

# Convert to datetimes
dttm_no <- survey_radio3 %>%
  filter(id %in% no_seconds) %>%
  mutate(start = mdy_hm(start),
         end = mdy_hm(end)) %>%
  select(1:3)

dttm_yes <- survey_radio3 %>%
  filter(id %in% yes_seconds) %>%
  mutate(start = mdy_hms(start),
         end = mdy_hms(end)) %>%
  select(1:3)

# Merge dttm data frames
dttm_both <- bind_rows(dttm_no, dttm_yes)

# Join datetimes to data frame
survey_radio4 <- survey_radio3 %>%
  select(-c(2:3)) %>%
  left_join(dttm_both, by = "id") %>%
  select("id", "start", "end", everything())

# Convert variables to factors
survey_radio4[c(5:9, 11:33)] <- map_df(survey_radio3[c(5:9, 11:33)], factor)

# Recode factors
survey_radio4 <- within(survey_radio4, {
  gender <- fct_recode(gender, Man = "Cisgender man")
  single <- fct_recode(single, Relationship = "In a relationship")
  si_popular <- recode(as.numeric(si_popular), `5` = 4, `4` = 3, `3` = 2, `2` = 1, `1` = 5)
  si_racialgrp <- recode(as.numeric(si_racialgrp), `5` = 4, `4` = 3, `3` = 2, `2` = 1, `1` = 5)
  si_react <- recode(as.numeric(si_react), `5` = 4, `4` = 3, `3` = 2, `2` = 1, `1` = 5)
  si_appearance <- recode(as.numeric(si_appearance), `5` = 4, `4` = 3, `3` = 2, `2` = 1, `1` = 5)
  si_masc <- recode(as.numeric(si_masc), `5` = 4, `4` = 3, `3` = 2, `2` = 1, `1` = 5)
  si_reputation <- recode(as.numeric(si_reputation), `5` = 4, `4` = 3, `3` = 2, `2` = 1, `1` = 5)
  si_attractive <- recode(as.numeric(si_attractive), `5` = 4, `4` = 3, `3` = 2, `2` = 1, `1` = 5)
  si_skincolor <- recode(as.numeric(si_skincolor), `5` = 4, `4` = 3, `3` = 2, `2` = 1, `1` = 5)
  si_gestures <- recode(as.numeric(si_gestures), `5` = 4, `4` = 3, `3` = 2, `2` = 1, `1` = 5)
  si_socialbhvr <- recode(as.numeric(si_socialbhvr), `5` = 4, `4` = 3, `3` = 2, `2` = 1, `1` = 5)
  fc_1 <- fct_recode(fc_1, "white-em-01" = "Image 1",
                     "black-em-03" = "Image 2",
                     "white-em-03" = "Image 3",
                     "asian-hm-02" = "Image 4")
  fc_2 <- fct_recode(fc_2, "black-hm-05" = "Image 1",
                     "black-em-02" = "Image 2",
                     "white-hm-02" = "Image 3",
                     "black-hm-04" = "Image 4")
  fc_3 <- fct_recode(fc_3, "latino-hm-01" = "Image 1",
                     "black-em-01" = "Image 2",
                     "white-em-05" = "Image 3",
                     "white-hm-03" = "Image 4")
  fc_4 <- fct_recode(fc_4, "asian-em-04" = "Image 1",
                     "white-hm-04" = "Image 2",
                     "black-em-05" = "Image 3",
                     "asian-em-03" = "Image 4")
  fc_5 <- fct_recode(fc_5, "black-hm-03" = "Image 1",
                     "latino-em-03" = "Image 2",
                     "black-hm-02" = "Image 3",
                     "asian-hm-04" = "Image 4")
  fc_6 <- fct_recode(fc_6, "black-em-04" = "Image 1",
                     "latino-em-02" = "Image 2",
                     "asian-hm-03" = "Image 3",
                     "white-em-04" = "Image 4")
  fc_7 <- fct_recode(fc_7, "latino-hm-04" = "Image 1",
                     "latino-hm-03" = "Image 2",
                     "asian-em-05" = "Image 3",
                     "asian-hm-01" = "Image 4")
  fc_8 <- fct_recode(fc_8, "white-hm-01" = "Image 1",
                     "latino-hm-05" = "Image 2",
                     "latino-em-05" = "Image 3",
                     "white-hm-05" = "Image 4")
  fc_9 <- fct_recode(fc_9, "black-hm-01" = "Image 1",
                     "asian-em-01" = "Image 2",
                     "latino-hm-02" = "Image 3",
                     "white-em-02" = "Image 4")
  fc_10 <- fct_recode(fc_10, "asian-hm-05" = "Image 1",
                      "latino-em-01" = "Image 2",
                      "latino-em-04" = "Image 3",
                      "asian-em-02" = "Image 4")
})

# Coerce age to integer
survey_radio4 <- survey_radio4 %>%
  mutate(age = as.integer(age))

# EXPORT DATA -------------------------------------------------------------

# Save forced response to file
write_csv(survey_radio4, path = "data/forced_response.csv")

# Save free response to file
write_csv(survey_dummy4, path = "data/free_response.csv")

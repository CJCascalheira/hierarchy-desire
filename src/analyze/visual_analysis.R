# Dependencies
library(tidyverse)

# Import
forced <- read_csv("data/forced_response.csv")
free <- read_csv("data/free_response.csv")
interrater <- read_csv("data/interrater.csv")

# Collapse Asian and South Asian, non-theoretically relevant ethnicities
forced <- within(forced, {
  ethnicity <- recode(ethnicity, "South Asian" = "Asian",
                      "Middle Eastern" = "Other", "Mixed" = "Other",
                      "Native American" = "Other")
})

# HYPOTHESIS 7 ------------------------------------------------------------

# Top four most preferred forced choice photos
(top_4_forced <- forced %>%
  select(starts_with("fc_")) %>%
  gather(key = fc, value = photo) %>%
  group_by(photo) %>%
  summarize(popularity = n()) %>%
  arrange(desc(popularity)) %>%
  head(n = 4))

# Top four most preferred free response photos
(top_4_free <- free[, -c(1:34)] %>%
  gather(key = option, value = selection) %>%
  filter(selection != "none") %>%
  group_by(selection) %>%
  summarize(popularity = n()) %>%
  arrange(desc(popularity)) %>%
  head(n = 4))

# Rank top six photos
bind_rows(top_4_forced, top_4_free[2:3, ]) %>%
  arrange(desc(popularity))

# TOP PROTOTYPES BY ETHNICITY ---------------------------------------------

# Select participant ID based on ethnicity
ethnic <- c("Asian", "Black", "Latino", "Other", "White")

for (i in 1:length(ethnic)) {
  # Pull IDs for each ethnicity
  ids <- forced %>%
    filter(ethnicity == ethnic[i]) %>%
    select(id) %>%
    pull()
  
  # Save IDs as a vector
  assign(ethnic[i], ids)
}

# Top photo among Asian
forced %>%
  select(id, starts_with("fc_")) %>%
  filter(id %in% Asian) %>%
  gather(key = fc, value = photo, -id) %>%
  count(photo) %>%
  arrange(desc(n)) %>%
  head(n = 1)

free[, -c(2:34)] %>%
  filter(id %in% Asian) %>%
  gather(key = option, value = selection) %>%
  filter(selection != "none") %>%
  count(selection) %>%
  arrange(desc(n)) %>%
  head(n = 1)

# Top photo among Black
forced %>%
  select(id, starts_with("fc_")) %>%
  filter(id %in% Black) %>%
  gather(key = fc, value = photo, -id) %>%
  count(photo) %>%
  arrange(desc(n)) %>%
  head(n = 1)

free[, -c(2:34)] %>%
  filter(id %in% Black) %>%
  gather(key = option, value = selection) %>%
  filter(selection != "none") %>%
  count(selection) %>%
  arrange(desc(n)) %>%
  head(n = 1)

# Top photo among Latino
forced %>%
  select(id, starts_with("fc_")) %>%
  filter(id %in% Latino) %>%
  gather(key = fc, value = photo, -id) %>%
  count(photo) %>%
  arrange(desc(n)) %>%
  head(n = 1)

free[, -c(2:34)] %>%
  filter(id %in% Latino) %>%
  gather(key = option, value = selection) %>%
  filter(selection != "none") %>%
  count(selection) %>%
  arrange(desc(n)) %>%
  head(n = 1)

# Top photo among Other
forced %>%
  select(id, starts_with("fc_")) %>%
  filter(id %in% Other) %>%
  gather(key = fc, value = photo, -id) %>%
  count(photo) %>%
  arrange(desc(n)) %>%
  head(n = 1)

free[, -c(2:34)] %>%
  filter(id %in% Other) %>%
  gather(key = option, value = selection) %>%
  filter(selection != "none") %>%
  count(selection) %>%
  arrange(desc(n)) %>%
  head(n = 1)

# Top photo among White
forced %>%
  select(id, starts_with("fc_")) %>%
  filter(id %in% White) %>%
  gather(key = fc, value = photo, -id) %>%
  count(photo) %>%
  arrange(desc(n)) %>%
  head(n = 1)

free[, -c(2:34)] %>%
  filter(id %in% White) %>%
  gather(key = option, value = selection) %>%
  filter(selection != "none") %>%
  count(selection) %>%
  arrange(desc(n)) %>%
  head(n = 1)

# INTERRATER RELIABILITY --------------------------------------------------

# Too much granularity, so condense
interrater <- within(interrater, {
  fem_masc <- recode(fem_masc, "1" = "1", "2" = "1", "3" = "2", "4" = "2",
                     "5" = "3", "6" = "3", "7" = "4", "8" = "4", "9" = "5",
                     "10" = "5")
  thin_musc <- recode(thin_musc, "1" = "1", "2" = "1", "3" = "2", "4" = "2",
                     "5" = "3", "6" = "3", "7" = "4", "8" = "4", "9" = "5",
                     "10" = "5")
  hairless_hairy <- recode(hairless_hairy, "1" = "1", "2" = "1", "3" = "2", "4" = "2",
                     "5" = "3", "6" = "3", "7" = "4", "8" = "4", "9" = "5",
                     "10" = "5")
  age_range <- recode(age_range, "18-20" = "18-23", "21-23" = "18-23",
                      "24-26" = "24-29", "27-29" = "24-29",
                      "30-32" = "30-35", "33-35" = "30-35",
                      "36-38" = "36-41", "39-41" = "36-41",
                      "42-44" = "42+", "45+" = "42+")
})

# Prototypes
proto <-unique(interrater$prototype)

# Empty data frame
reliability <- data.frame("success" = 0, "failure" = 0)

# Count success vs failure
for (i in 1:length(proto)) {
  reliability <- interrater %>%
    filter(prototype == proto[i]) %>%
    summarize(
      fem_masc = fem_masc[1] == fem_masc[2],
      thin_musc = thin_musc[1] == thin_musc[2],
      hairless_hairy = hairless_hairy[1] == hairless_hairy[2],
      sex_position = sex_position[1] == sex_position[2],
      ethnicity = ethnicity[1] == ethnicity[2],
      age_range = age_range[1] == age_range[2]
    ) %>%
    map_df(as.numeric) %>%
    summarize(
      success = rowSums(.),
      failure = ncol(.)
    ) %>%
    bind_rows(reliability)
}

# Overall inter-rater reliability
reliability[-7, ] %>%
  summarize(
    success = sum(success),
    failure = sum(failure),
    prop = success / failure
  )

# Inter-rater reliability per item
interrater %>%
  group_by(prototype) %>%
  summarize(
    fem_masc = fem_masc[1] == fem_masc[2],
    thin_musc = thin_musc[1] == thin_musc[2],
    hairless_hairy = hairless_hairy[1] == hairless_hairy[2],
    sex_position = sex_position[1] == sex_position[2],
    ethnicity = ethnicity[1] == ethnicity[2],
    age_range = age_range[1] == age_range[2]
  ) %>%
  ungroup() %>%
  map_df(as.numeric) %>%
  summarize(
    fem_masc = sum(fem_masc) / 6,
    thin_musc = sum(thin_musc) / 6,
    hairless_hairy = sum(hairless_hairy) / 6,
    sex_position = sum(sex_position) / 6,
    ethnicity = sum(ethnicity) / 6,
    age_range = sum(age_range) / 6
  )
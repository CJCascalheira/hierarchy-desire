# Dependencies
library(tidyverse)

# Import data
forced <- read_csv("data/forced_response.csv")

# METHODS: PARTICIPANTS ---------------------------------------------------

# Average age
forced %>%
  summarize(
    min_age = min(age),
    max_age = max(age),
    m_age = mean(age),
    sd_age = sd(age)
  )

# Ethnicity
forced %>%
  count(ethnicity) %>%
  mutate(percentage = round(n / nrow(forced), digits = 2)) %>%
  arrange(desc(percentage))

# Sexual orientation
forced %>%
  count(sex_orient) %>%
  mutate(percentage = round(n / nrow(forced), digits = 2)) %>%
  arrange(desc(percentage))

# Gender identity
forced %>%
  count(gender) %>%
  mutate(percentage = round(n / nrow(forced), digits = 2)) %>%
  arrange(desc(percentage))

# Level of outness
forced %>%
  count(out) %>%
  mutate(percentage = round(n / nrow(forced), digits = 2)) %>%
  arrange(desc(percentage))

# Relationship status
forced %>%
  count(single) %>%
  mutate(percentage = round(n / nrow(forced), digits = 2)) %>%
  arrange(desc(percentage))

# Sexual position
forced %>%
  count(sex_position) %>%
  mutate(percentage = round(n / nrow(forced), digits = 2)) %>%
  arrange(desc(percentage))

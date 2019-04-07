# Dependencies
library(tidyverse)

# Import
forced <- read_csv("data/forced_response.csv")

# Collapse Asian and South Asian, non-theoretically relevant ethnicities
forced <- within(forced, {
  ethnicity <- recode(ethnicity, "South Asian" = "Asian",
                      "Middle Eastern" = "Other", "Mixed" = "Other",
                      "Native American" = "Other")
})

# HYPOTHESIS 6 ------------------------------------------------------------

# Proportion of participants who preferred no sex as their sexual position
forced %>%
  select(sex_position, ethnicity) %>%
  filter(sex_position == "No sex") %>%
  count(sex_position) %>%
  mutate(prop = n / 348)

# Chi-square test of independence
forced %>%
  select(sex_position, ethnicity) %>%
  filter(sex_position != "No sex") %>%
  table() %>%
  chisq.test()

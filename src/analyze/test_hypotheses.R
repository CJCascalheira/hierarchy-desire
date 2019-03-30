# Dependencies
library(tidyverse)
library(ez)

# Import
forced <- read_csv("data/forced_response.csv")
forced_long <- read_csv("data/forced_long.csv")
free <- read_csv("data/free_response.csv")
free_long <- read_csv("data/free_long.csv")

# Collapse Asian and South Asian, non-theoretically relevant ethnicities
forced <- within(forced, {
  ethnicity <- recode(ethnicity, "South Asian" = "Asian",
                      "Middle Eastern" = "Other", "Mixed" = "Other",
                      "Native American" = "Other")
})

# HYPOTHESIS 1 ------------------------------------------------------------

  
# HYPOTHESIS 2 ------------------------------------------------------------


# HYPOTHESIS 3 ------------------------------------------------------------

# Differentiate between the two ethnicity variables
forced_long_h3 <- forced_long %>%
  mutate(photo_ethnicity = ethnicity) %>%
  select(-("ethnicity"))

free_long_h3 <- free_long %>%
  mutate(photo_ethnicity = ethnicity) %>%
  select("id", "photo_ethnicity")

# Construct data frame for hypothesis 3 (forced)
h3_df_forced <- forced2 %>%
  mutate(msm_ethnicity = ethnicity) %>%
  select("id", "msm_ethnicity") %>%
  left_join(forced_long_h3, by = "id") %>%
  select("id", ends_with("ethnicity"))

# Construct data frame for hypothesis 3 (free)
h3_df_free <- forced2 %>%
  mutate(msm_ethnicity = ethnicity) %>%
  select("id", "msm_ethnicity") %>%
  left_join(free_long_h3, by = "id") %>%
  select("id", ends_with("ethnicity"))

# Table of photo ethnicity versus self-reported ethnicity (forced)
table(h3_df_forced$photo_ethnicity, h3_df_forced$msm_ethnicity)

# Table of photo ethnicity versus self-reported ethnicity (free)
table(h3_df_free$photo_ethnicity, h3_df_free$msm_ethnicity)

# HYPOTHESIS 4 ------------------------------------------------------------

forced

# HYPOTHESIS 5 ------------------------------------------------------------


# HYPOTHESIS 7 ------------------------------------------------------------




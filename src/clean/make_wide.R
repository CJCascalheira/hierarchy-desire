# Dependencies
library(tidyverse)

# Import
forced_long <- read_csv("data/forced_long.csv")
free_long <- read_csv("data/free_long.csv")
forced <- read_csv("data/forced_response.csv")

# Collapse Asian and South Asian, non-theoretically relevant ethnicities
forced <- within(forced, {
  ethnicity <- recode(ethnicity, "South Asian" = "Asian",
                      "Middle Eastern" = "Other", "Mixed" = "Other",
                      "Native American" = "Other")
})

# Participant ethnicity
ethnic <- forced %>%
  select(id, participant_ethnic = ethnicity)

# FORCED WIDE -------------------------------------------------------------

# Ethnicity x Effeminancy
for_em <- forced_long[, -5] %>%
  spread(key = ethnicity, value = n) %>%
  filter(masculinity == "em") %>%
  select(id, asian_em = asian, black_em = black, latino_em = latino,
         white_em = white)

# Ethnicity x Heteronormativity
for_hm <- forced_long[, -5] %>%
  spread(key = ethnicity, value = n) %>%
  filter(masculinity == "hm") %>%
  select(id, asian_hm = asian, black_hm = black, latino_hm = latino,
         white_hm = white)

# Combine
forced_wide <- left_join(for_em, for_hm, by = "id") %>%
  left_join(ethnic, by = "id")

# Save as CSV
write_csv(forced_wide, path = "data/forced_wide.csv")

# FREE WIDE ---------------------------------------------------------------

# Ethnicity x Effeminancy
fre_em <- free_long[, -5] %>%
  spread(key = ethnicity, value = n) %>%
  filter(masculinity == "em") %>%
  select(id, asian_em = asian, black_em = black, latino_em = latino,
         white_em = white)

# Ethnicity x Heteronormativity
fre_hm <- free_long[, -5] %>%
  spread(key = ethnicity, value = n) %>%
  filter(masculinity == "hm") %>%
  select(id, asian_hm = asian, black_hm = black, latino_hm = latino,
         white_hm = white)

# Combine
free_wide <- left_join(fre_em, fre_hm, by = "id") %>%
  left_join(ethnic, by = "id")

# Save as CSV
write_csv(free_wide, path = "data/free_wide.csv")

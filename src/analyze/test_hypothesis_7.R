# Dependencies
library(tidyverse)

# Import
forced <- read_csv("data/forced_response.csv")
free <- read_csv("data/free_response.csv")

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

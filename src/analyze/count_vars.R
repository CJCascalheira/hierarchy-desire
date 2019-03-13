# After writing this code, I realized an easier method existed. However,
# I did not have the gall to delete this complicated nested for loop --
# my most ambitious one yet!

# Dependencies
library(tidyverse)

# Import data
forced <- read_csv("data/forced_response.csv")

# Select forced response image variables
fc <- forced %>%
  select("id", starts_with("fc_")) %>%
  mutate(heteronormative = rep(0, 354),
         effeminate = rep(0, 354),
         asian = rep(0, 354),
         black = rep(0, 354),
         latino = rep(0, 354),
         white = rep(0, 354))

# COUNT LEVELS: FORCED RESPONSE -------------------------------------------

# For each observation
for (j in 1:nrow(fc)) {
  
  # Set starting values to 0
  hetero <- 0
  effem <- 0
  asn <- 0
  blk <- 0
  ltn <- 0
  wht <- 0
  
  # For each forced choice image, which levels of ethnicity and masculinity 
  # are present? 
  for (i in 2:11) {
    
    # Ethnicity present?
    if (str_detect(fc[j, i], "^asian") == TRUE) {
      asn <- asn + 1
    } else if (str_detect(fc[j, i], "^black") == TRUE) {
      blk <- blk + 1
    } else if (str_detect(fc[j, i], "^latino") == TRUE) {
      ltn <- ltn + 1  
    } else {
      wht <- wht + 1
    } 
    
    # Masculinity present?
    if (str_detect(fc[j, i], "em") == TRUE) {
      effem <- effem + 1
    } else {
      hetero <- hetero + 1
    }
    
  }
  
  fc[j, ] <- fc[j, ] %>%
    mutate(
      heteronormative = hetero,
      effeminate = effem,
      asian = asn,
      black = blk,
      latino = ltn,
      white = wht
    )
}

# Did it work?
fc

# Raw sums of key levels
fc %>% 
  summarize(
    hetero = sum(heteronormative),
    effem = sum(effeminate),
    asn = sum(asian),
    blk = sum(black),
    ltn = sum(latino),
    wht = sum(white)
  )

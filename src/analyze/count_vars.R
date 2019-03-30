# After writing this code, I realized an easier method existed. However,
# I did not have the gall to delete this complicated nested for loop --
# my most ambitious one yet!

# Dependencies
library(tidyverse)

# Import data
forced <- read_csv("data/forced_response.csv")
free <- read_csv("data/free_response.csv")

# Select forced response image variables
fc <- forced %>%
  select("id", starts_with("fc_")) %>%
  mutate(heteronormative = rep(0, 348),
         effeminate = rep(0, 348),
         asian = rep(0, 348),
         black = rep(0, 348),
         latino = rep(0, 348),
         white = rep(0, 348))

# Select free response image variables
fr <- free[, c(1, 35:74)] %>%
  mutate(heteronormative = rep(0, 348),
         effeminate = rep(0, 348),
         asian = rep(0, 348),
         black = rep(0, 348),
         latino = rep(0, 348),
         white = rep(0, 348),
         none = rep(0, 348))

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

# COUNT LEVELS: FREE RESPONSE ---------------------------------------------

# For each observation
for (j in 1:nrow(fr)) {
  
  # Set starting values to 0
  hetero <- 0
  effem <- 0
  asn <- 0
  blk <- 0
  ltn <- 0
  wht <- 0
  nn <- 0
  
  # For each forced choice image, which levels of ethnicity and masculinity 
  # are present? 
  for (i in 2:41) {
    
    # Ethnicity present?
    if (str_detect(fr[j, i], "^asian") == TRUE) {
      asn <- asn + 1
    } else if (str_detect(fr[j, i], "^black") == TRUE) {
      blk <- blk + 1
    } else if (str_detect(fr[j, i], "^latino") == TRUE) {
      ltn <- ltn + 1  
    } else if (str_detect(fr[j, i], "^white") == TRUE) {
      wht <- wht + 1
    } else {
      nn <- nn +1
    }
    
    # Masculinity present?
    if (str_detect(fr[j, i], "em") == TRUE) {
      effem <- effem + 1
    } else if (str_detect(fr[j, i], "hm") == TRUE) {
      hetero <- hetero + 1
    } else {
      nn <- nn + 1
    }
    
  }
  
  fr[j, ] <- fr[j, ] %>%
    mutate(
      heteronormative = hetero,
      effeminate = effem,
      asian = asn,
      black = blk,
      latino = ltn,
      white = wht,
      none = nn
    )
}

# Did it work?
fr$heteronormative

# Raw sums of key levels
fr %>% 
  summarize(
    hetero = sum(heteronormative),
    effem = sum(effeminate),
    asn = sum(asian),
    blk = sum(black),
    ltn = sum(latino),
    wht = sum(white)
  )

# THIS --------------------------------------------------------------------

# Add type of response
fc1 <- fc[, c(1, 12:17)] %>%
  mutate(response = rep("forced", 348))

fr1 <- fr[, c(1, 42:47)] %>%
  mutate(response = rep("free", 348))

# Separate ethnicity and masculinity
fc_eth <- fc1 %>%
  select(id, response, asian, black, latino, white)

fc_masc <- fc1 %>%
  select(id, response, heteronormative, effeminate)

fr_eth <- fr1 %>%
  select(id, response, asian, black, latino, white)

fr_masc <- fr1 %>%
  select(id, response, heteronormative, effeminate)

# Merge data frames
ethnic <- bind_rows(fc_eth, fr_eth) %>%
  gather(key = ethnicity, value = n, -c(id, response)) %>%
  mutate(response = factor(response),
         ethnicity = factor(ethnicity))

masc <- bind_rows(fc_masc, fr_masc) %>%
  gather(key = masculinity, value = n, -c(id, response)) %>%
  mutate(response = factor(response),
         masculinity = factor(masculinity))
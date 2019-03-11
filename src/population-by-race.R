####### CENSUS DATA FOR RACE BY ZIP CODES #######

# Load dependencies
library(tidyverse)

# ETHNIC ENCLAVES IN NEW YORK ---------------------------------------------

# Import data
newyork <- read_csv("data/ACS_16_5YR_B02001_NY.csv")

# Explore data
glimpse(newyork)
head(newyork)
colnames(newyork)

# Make data tidy
newyork2 <- newyork %>% select(`GEO.display-label`, HD01_VD01, HD01_VD02, HD01_VD03, HD01_VD05)
newyork2 <- newyork2[-c(1), ]
clear_names <- c("zipcode", "total_pop", "white_pop", "black_pop", "asian_pop")
colnames(newyork2) <- clear_names

# Isolate zip codes
newyork2$zipcode <- str_replace(newyork2$zipcode, "ZCTA5 ", "")
newyork2

# Change to integers
sum(is.na(newyork2))
newyork2 <- newyork2 %>% filter(total_pop != 0)
newyork3 <- apply(newyork2, 2, as.integer)
newyork3 <- as.tbl(as.data.frame(newyork3))

# Identify ethnic enclaves in New York
newyork3 %>% arrange(desc(white_pop))
newyork3 %>% arrange(desc(black_pop))
newyork3 %>% arrange(desc(asian_pop))

# Select ethnic enclaves in New York
newyork3 %>% filter(zipcode == 11385) %>%
  summarize(total = total_pop,
            white = white_pop,
            percent = white_pop / total_pop)

newyork3 %>% filter(zipcode == 11236) %>%
  summarize(total = total_pop,
            black = black_pop,
            percent = black_pop / total_pop)

newyork3 %>% filter(zipcode == 11355) %>%
  summarize(total = total_pop,
            asian = asian_pop,
            percent = asian_pop / total_pop)

# MOST POPULOUS ETHNIC ENCLAVES -------------------------------------------

# Import data
usa <- read_csv("data/ACS_16_5YR_B02001_USA.csv")

# Explore data
glimpse(usa)
head(usa)
colnames(usa)

# Make data tidy
usa2 <- usa %>% select(`GEO.display-label`, HD01_VD01, HD01_VD02, HD01_VD03, HD01_VD05)
usa2 <- usa2[-c(1), ]

clear_names <- c("zipcode", "total_pop", "white_pop", "black_pop", "asian_pop")
colnames(usa2) <- clear_names
usa2

# Isolate zip codes
usa2$zipcode <- str_replace(usa2$zipcode, "ZCTA5 ", "")
usa2

# Change to integers
sum(is.na(usa2))
usa2 <- usa2 %>% filter(total_pop != 0)
usa3 <- apply(usa2, 2, as.integer)
usa3 <- as.tbl(as.data.frame(usa3))

# Identify ethnic enclaves outside of New York
usa3 %>% arrange(desc(white_pop))
usa3 %>% arrange(desc(black_pop))
usa3 %>% arrange(desc(asian_pop))

# Select ethnic enclaves outside of New York
usa3 %>% filter(zipcode == 79936) %>%
  summarize(total = total_pop,
            white = white_pop,
            percent = white_pop / total_pop)

usa3 %>% filter(zipcode == 60620) %>%
  summarize(total = total_pop,
            black = black_pop,
            percent = black_pop / total_pop)

usa3 %>% filter(zipcode == 95035) %>%
  summarize(total = total_pop,
            asian = asian_pop,
            percent = asian_pop / total_pop)

# RANDOM ZIP CODES --------------------------------------------------------

# Note: not completely reproducible
set.seed(1234)

# Sample ten random zip codes
sample(usa3$zipcode, size = 10)

####### CENSUS DATA FOR RACE BY ZIP CODES #######

# Load dependency
library(tidyverse)

# ETHNIC ENCLAVES IN USA --------------------------------------------------

# Import the data
zip_codes <- read_csv("data/ACS_17_5YR_B03002.csv")

# Variables to keep
zip_codes2 <- zip_codes %>%
  select(`GEO.display-label`, HD01_VD01, HD01_VD03, HD01_VD04, HD01_VD06, HD01_VD12)

# Drop first row
zip_codes3 <- zip_codes2[-1,]

# Clean variable names
nice_names <- c("zipcodes", "total_pop", "white_pop", "black_pop", "asian_pop", "latino_pop")

# Set variable names
names(zip_codes3) <- nice_names

# Coerce into integers
(zip_codes4 <- within(zip_codes3, {
  total_pop <- as.integer(total_pop)
  white_pop <- as.integer(white_pop)
  black_pop <- as.integer(black_pop)
  asian_pop <- as.integer(asian_pop)
  latino_pop <- as.integer(latino_pop)
}))

# Isolate ZIP codes
zip_codes4$zipcodes <- str_replace(zip_codes4$zipcodes, "ZCTA5 ", "")

# White ethnic enclaves
zip_codes4 %>%
  arrange(desc(white_pop)) %>%
  select(zipcodes, white_pop) %>%
  head(n = 5)

# Black ethnic enclaves
zip_codes4 %>% arrange(desc(black_pop)) %>%
  select(zipcodes, black_pop) %>%
  head(n = 5)

# Asian ethnic enclaves
zip_codes4 %>% arrange(desc(asian_pop)) %>%
  select(zipcodes, asian_pop) %>%
  head(n = 5)

# Latino ethnic enclaves
zip_codes4 %>% arrange(desc(latino_pop)) %>%
  select(zipcodes, latino_pop) %>%
  head(n = 5)
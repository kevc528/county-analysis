# load libraries
library(tidyverse)

# download raw, unclean county data from American Community Survey
url = "https://www.openintro.org/data/csv/county_2019.csv"
counties_raw = read_csv(url)

# write raw data to file
write_csv(x = counties_raw, file = "data/raw/counties_raw.csv")

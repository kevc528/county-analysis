# load libraries
library(tidyverse)

# read in the cleaned data
counties_clean = read_csv("data/clean/counties_clean.csv")

# split into train and test
set.seed(471)

set.seed(471)
train_samples = sample(1:nrow(counties_clean), 0.8*nrow(counties_clean))
counties_train = counties_clean %>% filter(row_number() %in% train_samples)
counties_test = counties_clean %>% filter(!(row_number() %in% train_samples))

# save the train and test data
write_csv(x = counties_train, file = "data/clean/counties_train.csv")
write_csv(x = counties_test, file = "data/clean/counties_test.csv")
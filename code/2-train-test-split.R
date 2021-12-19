# load libraries
library(tidyverse)

# read in the cleaned data
counties_clean = read_csv("data/clean/counties_clean.csv")

# split into train and test
set.seed(471)

# generate 80-20 split for train-test split
train_samples = sample(1:nrow(counties_clean), 0.8*nrow(counties_clean))

# see which column names contain the word "income"
colnames(counties_clean[grepl('income', colnames(counties_clean))])

# generate split and remove correlated income features to keep only `median_household_income`
counties_train = counties_clean %>%
  filter(row_number() %in% train_samples) %>%
  select(-mean_household_income, -median_individual_income, -median_individual_income_age_25plus, -per_capita_income)

counties_test = counties_clean %>% 
  filter(!(row_number() %in% train_samples)) %>%
  select(-mean_household_income, -median_individual_income, -median_individual_income_age_25plus, -per_capita_income)

# save the train and test data
write_csv(x = counties_train, file = "data/clean/counties_train.csv")
write_csv(x = counties_test, file = "data/clean/counties_test.csv")



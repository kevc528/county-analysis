# load libraries
library(tidyverse)

# load raw counties data
counties_raw = read_csv(file = "data/raw/counties_raw.csv")

# remove margin of error columns
counties_wo_moe = counties_raw %>%
  select(-ends_with("moe"))

# find other columns with the most null values
counties_wo_moe %>%
  summarise_all(~sum(is.na(.))) %>%
  pivot_longer(everything(), names_to = "col_name", values_to = "null_count") %>%
  arrange(desc(null_count))

# calculate state averages for columns with null values
avg_state_poverty = counties_wo_moe %>%
  select("state", "poverty") %>%
  na.omit() %>%
  group_by(state) %>%
  summarise(avg_poverty = mean(poverty))
  
avg_state_poverty_65_and_over = counties_wo_moe %>%
  select("state", "poverty_65_and_over") %>%
  na.omit() %>%
  group_by(state) %>%
  summarise(avg_poverty_65_and_over = mean(poverty_65_and_over))

avg_state_poverty_under_18 = counties_wo_moe %>%
  select("state", "poverty_under_18") %>%
  na.omit() %>%
  group_by(state) %>%
  summarise(avg_poverty_under_18 = mean(poverty_under_18))

avg_state_mean_work_travel = counties_wo_moe %>%
  select("state", "mean_work_travel") %>%
  na.omit() %>%
  group_by(state) %>%
  summarise(avg_mean_work_travel = mean(mean_work_travel))

avg_state_vals = left_join(avg_state_poverty, 
                           left_join(
                             avg_state_poverty_65_and_over,
                             left_join(
                               avg_state_poverty_under_18,
                               avg_state_mean_work_travel,
                               by = c("state")
                             ),
                             by = c("state")
                           ), 
                           by = c("state"))

# impute missing columns based on state averages
counties_clean = left_join(counties_wo_moe, avg_state_vals, by = c("state")) %>%
  mutate(poverty = coalesce(poverty, avg_poverty)) %>%
  mutate(poverty_65_and_over = coalesce(poverty_65_and_over, avg_poverty_65_and_over)) %>%
  mutate(poverty_under_18 = coalesce(poverty_under_18, avg_poverty_under_18)) %>%
  mutate(mean_work_travel = coalesce(mean_work_travel, avg_mean_work_travel)) %>%
  select(-avg_poverty, -avg_poverty_65_and_over, -avg_poverty_under_18, -avg_mean_work_travel)

# verify no columns with null values
counties_clean %>%
  summarise_all(~sum(is.na(.))) %>%
  pivot_longer(everything(), names_to = "col_name", values_to = "null_count") %>%
  arrange(desc(null_count))

# write cleaned data to file
write_csv(counties_clean, file = "data/clean/counties_clean.csv")

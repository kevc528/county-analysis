# load libraries
library(kableExtra)                     # for printing tables
library(maps)                           # for creating maps
library(tidyverse)

# install.packages("magick")
# install.packages("webshot")
# webshot::install_phantomjs()

# read in the cleaned data
counties_train = read_csv("data/clean/counties_clean.csv")

# create table with different percentiles of median household income
counties_train %>% 
  summarise(
    "Percentile" = c(0, 25, 50, 75, 100),
    `Median Household Income` = quantile(median_household_income, c(0, 0.25, 0.5, 0.75, 1))
  ) %>%
  kable(format = 'latex', row.names = NA,
        booktabs = TRUE, digits = 2,
        caption = 'Percentiles for Median Household Income') %>%
  kable_styling() %>%
  save_kable(file ="results/median-household-income-quartiles.png")

# create boxplot of the median household income
(counties_train %>%
    ggplot(aes(x = median_household_income / 1000)) + 
    geom_boxplot() +
    scale_x_continuous(breaks=seq(0,150,10)) +
    labs(x = "Median Household Income (Thousands of Dollars)", 
         y = "Number of Counties") +
    theme_bw()) %>%
    ggsave(filename = "results/median-household-income-boxplot.png", 
           device = "png", 
           width = 5, 
           height = 3)

# create histogram of the median household income
(counties_train %>%
  ggplot(aes(x = median_household_income / 1000)) + 
  geom_histogram(fill = 'blue', col = 'black') +
  scale_x_continuous(breaks=seq(0,150,10)) +
  labs(x = "Median Household Income (Thousands of Dollars)", 
       y = "Number of Counties") +
  theme_bw()) %>%
  ggsave(filename = "results/median-household-income-histogram.png", 
       device = "png", 
       width = 5, 
       height = 3)

# find top 10 counties by median household income
counties_train %>% 
  select(name, state, median_household_income) %>%
  arrange(desc(median_household_income)) %>%
  head(10) %>%
  kable(format = 'latex', row.names = NA,
        booktabs = TRUE, digits = 2,
        caption = 'Top 10 Counties for Median Household Income') %>%
  kable_styling() %>%
  save_kable(file ="results/top-10-counties.png")

# find bottom 10 counties by median household income
counties_train %>% 
  select(name, state, median_household_income) %>%
  arrange(median_household_income) %>%
  head(10) %>%
  kable(format = 'latex', row.names = NA,
        booktabs = TRUE, digits = 2,
        caption = 'Bottom 10 Counties for Median Household Income') %>%
  kable_styling() %>%
  save_kable(file ="results/bottom-10-counties.png")

# create a heatmap of case fatality rate across the U.S.
p = map_data("county") %>%
  as_tibble() %>% 
  left_join(case_data %>% 
              rename(region = state, 
                     subregion = county,
                     `Case Fatality Rate` = case_fatality_rate) %>% 
              mutate(region = str_to_lower(region), 
                     subregion = str_to_lower(subregion)), 
            by = c("region", "subregion")) %>%
  ggplot() + 
  geom_polygon(data=map_data("state"), 
               aes(x=long, y=lat, group=group),
               color="black", fill=NA,  size = 1, alpha = .3) + 
  geom_polygon(aes(x=long, y=lat, group=group, fill = `Case Fatality Rate`),
               color="darkblue", size = .1) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_void()

ggsave(filename = "results/response-map.png", 
       plot = p, 
       device = "png", 
       width = 7, 
       height = 4)
# load libraries
library(kableExtra)                     # for printing tables
library(maps)                           # for creating maps
library(tidyverse)

library(corrplot)
library(RColorBrewer)

# install.packages("magick")
# install.packages("webshot")
# webshot::install_phantomjs()

# read in the cleaned data
counties_train = read_csv("data/clean/counties_train.csv")

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
  save_kable(file ="results/eda/median-household-income-quartiles.png")

# create boxplot of the median household income
(counties_train %>%
    ggplot(aes(x = median_household_income / 1000)) + 
    geom_boxplot() +
    scale_x_continuous(breaks=seq(0,150,10)) +
    labs(x = "Median Household Income (Thousands of Dollars)") +
    theme_bw() +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())) %>%
    ggsave(filename = "results/eda/median-household-income-boxplot.png", 
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
  ggsave(filename = "results/eda/median-household-income-histogram.png", 
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
  save_kable(file ="results/eda/top-10-counties.png")

# find bottom 10 counties by median household income
counties_train %>% 
  select(name, state, median_household_income) %>%
  arrange(median_household_income) %>%
  head(10) %>%
  kable(format = 'latex', row.names = NA,
        booktabs = TRUE, digits = 2,
        caption = 'Bottom 10 Counties for Median Household Income') %>%
  kable_styling() %>%
  save_kable(file ="results/eda/bottom-10-counties.png")

# create a heatmap of median household income across the U.S.
(map_data("county") %>%
  as_tibble() %>% 
  left_join(counties_train %>% 
              rename(region = state, 
                     subregion = name,
                     `Median Household Income` = median_household_income) %>% 
              mutate(region = str_to_lower(region), 
                     subregion = gsub(" county| parish", "", str_to_lower(subregion))), 
            by = c("region", "subregion")) %>%
  ggplot() + 
  geom_polygon(data=map_data("state"), 
               aes(x=long, y=lat, group=group),
               color="black", fill=NA,  size = 1, alpha = .3) + 
  geom_polygon(aes(x=long, y=lat, group=group, fill = `Median Household Income`),
               color="darkblue", size = .1) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_void()) %>%
  ggsave(filename = "results/eda/median-household-income-map.png", 
         device = "png", 
         width = 7, 
         height = 4)

# find all the numerical features for correlation calculations
numeric_features = counties_train %>%
  select_if(~is.numeric(.)) %>%
  select(-fips) # remove fips since it is categorical

# create a correlation plot between the features
png(file="results/eda/correlation-plot.png", res=300, width=4500, height=4500)
numeric_features %>%
  cor() %>%
  corrplot(method="color")
dev.off()

# find most correlated features with median household income
numeric_features %>%
  cor() %>%
  as_tibble() %>%
  add_column(Feature = colnames(numeric_features)) %>%
  select(Feature, median_household_income) %>%
  rename(Correlation = median_household_income) %>%
  filter(Feature != "median_household_income") %>%
  arrange(desc(Correlation)) %>%
  head(10) %>%
  kable(format = 'latex', row.names = NA,
                  booktabs = TRUE, digits = 2,
                  caption = 'Features with Most Positive Correlation with Median Household Income') %>%
  kable_styling() %>%
  save_kable(file ="results/eda/top-10-positive-correlation.png")

numeric_features %>%
  cor() %>%
  as_tibble() %>%
  add_column(feature = colnames(numeric_features)) %>%
  select(feature, median_household_income) %>%
  rename(correlation = median_household_income) %>%
  arrange(correlation) %>%
  head(10) %>%
  kable(format = 'latex', row.names = NA,
        booktabs = TRUE, digits = 2,
        caption = 'Features with Most Negative Correlation with Median Household Income') %>%
  kable_styling() %>%
  save_kable(file ="results/eda/top-10-negative-correlation.png")

# plot scatter of a few positively correlated features
(numeric_features %>%
  rename(`Percent Bachelors` = bachelors, `Median Household Income` = median_household_income,
         `Percent Household with Computer` = household_has_computer) %>%
  ggplot() + 
  geom_point(aes(x = `Percent Bachelors`, y = `Median Household Income`,
                 color = `Percent Household with Computer`)) +
  theme_bw()) %>%
  ggsave(filename = "results/eda/positive-features-scatter.png", 
         device = "png", 
         width = 7, 
         height = 4)

# plot scatter of a few negative correlated features
(numeric_features %>%
    rename(`Poverty Rate` = poverty, `Median Household Income` = median_household_income,
           `Unemployment Rate` = unemployment_rate) %>%
    ggplot() + 
    geom_point(aes(x = `Poverty Rate`, y = `Median Household Income`,
                   color = `Unemployment Rate`)) +
    theme_bw()) %>%
  ggsave(filename = "results/eda/negative-features-scatter.png", 
         device = "png", 
         width = 7, 
         height = 4)

# Boxplot for Household Percentage with Computers
(counties_train %>%
    ggplot(aes(x = household_has_computer)) + 
    geom_boxplot() +
    scale_x_continuous(breaks=seq(0,150,10)) +
    labs(x = "Percentage Households with Computers") +
    theme_bw() +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())) %>%
  ggsave(filename = "results/eda/household-with-computer-boxplot.png", 
         device = "png", 
         width = 5, 
         height = 3)

# Boxplot for Poverty Rate
(counties_train %>%
    ggplot(aes(x = poverty)) + 
    geom_boxplot() +
    scale_x_continuous(breaks=seq(0,150,10)) +
    labs(x = "Percentage Population Below Poverty Level") +
    theme_bw() +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())) %>%
  ggsave(filename = "results/eda/poverty-boxplot.png", 
         device = "png", 
         width = 5, 
         height = 3)

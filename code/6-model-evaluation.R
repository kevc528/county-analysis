# load libraries
library(glmnetUtils)
library(tidyverse)

# load test data
counties_test = read_csv("data/clean/counties_test.csv") %>%
  select(-name, -state, -fips)

# load OLS object
load("results/model-results/lm_fit.Rda")

# load ridge fit object
load("results/ridge_fit.Rda")

# load lasso fit object
load("results/lasso_fit.Rda")

# evaluate OLS RMSE
lm_predictions = predict(lm_fit,
                         newdata = counties_test)
lm_RMSE = sqrt(mean((lm_predictions-counties_test$median_household_income)^2))
lm_RMSE

# evaluate ridge RMSE
ridge_predictions = predict(ridge_fit, 
                            newdata = covid_test, 
                            s = "lambda.1se") %>%
  as.numeric()
ridge_RMSE = sqrt(mean((ridge_predictions-covid_test$case_fatality_rate)^2))

# evaluate lasso RMSE
lasso_predictions = predict(lasso_fit, 
                            newdata = covid_test, 
                            s = "lambda.1se") %>%
  as.numeric()
lasso_RMSE = sqrt(mean((lasso_predictions-covid_test$case_fatality_rate)^2))

# print nice table
tibble(Method = c("Ridge", "Lasso"), `Test RMSE` = c(ridge_RMSE, lasso_RMSE)) %>%
  write_tsv("results/model-evaluation.tsv")
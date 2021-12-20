# load libraries
library(glmnetUtils)
library(tidyverse)
source("code/functions/plot_glmnet.R")
library(randomForest)
library(gbm)
library(kableExtra) 

# load test data
counties_test = read_csv("data/clean/counties_test.csv") %>%
  select(-name, -state, -fips)
counties_train = read_csv("data/clean/counties_train.csv") %>%
  select(-name, -state, -fips)

# load OLS object
load("results/model-results/lm_fit.Rda")

# load ridge fit object
load("results/model-results/ridge_fit.Rda")

# load lasso fit object
load("results/model-results/lasso_fit.Rda")

# load elastic net object
load("results/model-results/elnet_fit.Rda")

# load regression tree object
load("results/model-results/tree_fit.Rda")

# load tuned regression tree object
load("results/model-results/tree_optimal_fit.Rda")

# load random forest object
load("results/model-results/rf_fit.Rda")

# load tuned random forest object
load("results/model-results/rf_fit_tuned.Rda")

# load boosted model object
load("results/model-results/gbm_fit_tuned.Rda")



# evaluate OLS RMSE
lm_predictions = predict(lm_fit,
                         newdata = counties_test)
lm_RMSE = sqrt(mean((lm_predictions-counties_test$median_household_income)^2))
lm_RMSE

# evaluate ridge RMSE
ridge_predictions = predict(ridge_fit,
                         newdata = counties_test)
ridge_RMSE = sqrt(mean((ridge_predictions-counties_test$median_household_income)^2))
ridge_RMSE

# evaluate lasso RMSE
lasso_predictions = predict(lasso_fit, 
                            newdata = counties_test)
lasso_RMSE = sqrt(mean((lasso_predictions-counties_test$median_household_income)^2))
lasso_RMSE

# evaluate elastic net RMSE
elnet_predictions = predict(elnet_fit, 
                            alpha = extract_best_elnet(elnet_fit)$alpha,
                            newdata = counties_test)
elnet_RMSE = sqrt(mean((elnet_predictions-counties_test$median_household_income)^2))
elnet_RMSE

# evaluate untuned tree fit RMSE
tree_predictions = predict(tree_fit, 
                           newdata = counties_test)
tree_RMSE = sqrt(mean((tree_predictions-counties_test$median_household_income)^2))
tree_RMSE

# evaluate tuned tree fit RMSE
tree_tuned_predictions = predict(tree_optimal_fit, 
                           newdata = counties_test)
tree_tuned_RMSE = sqrt(mean((tree_tuned_predictions-counties_test$median_household_income)^2))
tree_tuned_RMSE

# evaluate untuned random forest RMSE
rf_predictions = predict(rf_fit, 
                           newdata = counties_test)
rf_RMSE = sqrt(mean((rf_predictions-counties_test$median_household_income)^2))
rf_RMSE

# evaluate tuned random forest RMSE
rf_tuned_predictions = predict(rf_fit_tuned, 
                         newdata = counties_test)
rf_tuned_RMSE = sqrt(mean((rf_tuned_predictions-counties_test$median_household_income)^2))
rf_tuned_RMSE

# evaluate boosted model RMSE
gbm_predictions = predict(gbm_fit_tuned, 
                          n.trees = gbm.perf(gbm_fit_3, plot.it = FALSE), 
                          newdata = counties_test)
gbm_RMSE = sqrt(mean((gbm_predictions-counties_test$median_household_income)^2))
gbm_RMSE

#evaluate intercept only RMSE
intercept_prediction = mean(counties_train$median_household_income)
intercept_RMSE = sqrt(mean((intercept_prediction-counties_test$median_household_income)^2))
intercept_RMSE

# print nice table
performance = tibble(`Model` = c("Intercept-Only", "Ordinary-Least-Squares", "Ridge", 
                   "Lasso", "Elastic Net", 
                   "Default Regression Tree", "Tuned Regression Tree",
                   "Default Random Forest", "Tuned Random Forest",
                   "Boosted Model"), 
       `Test RMSE` = c(intercept_RMSE, lm_RMSE, ridge_RMSE, lasso_RMSE,
                       elnet_RMSE, tree_RMSE, tree_tuned_RMSE, 
                       rf_RMSE, rf_tuned_RMSE, gbm_RMSE)) %>%
  arrange(desc(`Test RMSE`))

performance



# load libraries
library(tidyverse)
library(glmnetUtils)                    # to run ridge and lasso
library(kableExtra)                     # for printing tables

# install.packages("magick")
# install.packages("webshot")
# webshot::install_phantomjs()

source("code/functions/plot_glmnet.R")  # for lasso/ridge trace plots

# read in the training data
counties_train = read_csv("data/clean/counties_train.csv") %>% 
  select(-state, -name, -fips)
set.seed(471)

# =========================RUN OLS REGRESSION==========================
lm_fit = lm(formula = median_household_income ~ ., 
            data = counties_train)

# save the OLS fit object
save(lm_fit, file = "results/model-results/lm_fit.Rda")


# =========================RUN RIDGE REGRESSION=========================
set.seed(471)
ridge_fit = cv.glmnet(median_household_income ~ ., 
                      alpha = 0, 
                      nfolds = 10,
                      data = counties_train)

# save the ridge fit object
save(ridge_fit, file = "results/model-results/ridge_fit.Rda")

# create ridge CV plot
png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "results/model-results/ridge-cv-plot.png")
plot(ridge_fit)
dev.off()

# create ridge trace plot
ridge_plot = plot_glmnet(ridge_fit, counties_train, features_to_plot = 6)
ggsave(filename = "results/model-results/ridge-trace-plot.png", 
       plot = ridge_plot, 
       device = "png", 
       width = 6.5, 
       height = 4)


# =========================RUN LASSO REGRESSION==========================
set.seed(471)
lasso_fit = cv.glmnet(median_household_income ~ .,   
                      alpha = 1,                 
                      nfolds = 10,               
                      data = counties_train)

# save the lasso fit object
save(lasso_fit, file = "results/model-results/lasso_fit.Rda")

# create lasso CV plot
png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "results/model-results/lasso-cv-plot.png")
plot(lasso_fit)
dev.off()

# create lasso trace plot
lasso_plot = plot_glmnet(lasso_fit, counties_train, features_to_plot = 6)
ggsave(filename = "results/model-results/lasso-trace-plot.png", 
       plot = lasso_plot, 
       device = "png", 
       width = 6.5, 
       height = 4)

# extract features selected by lasso and their coefficients
beta_hat_std = extract_std_coefs(lasso_fit, counties_train)
beta_hat_std %>%
  filter(coefficient != 0) %>%
  arrange(desc(abs(coefficient))) %>% 
  rename(Feature = feature, Coefficient = coefficient) %>%
  head(10) %>%
  kable(format = 'latex', row.names = NA,
        booktabs = TRUE, digits = 2, linesep = "") %>%
  kable_styling() %>%
  save_kable(file ="results/model-results/lasso-coefficients.png")

beta_hat_std = extract_std_coefs(lasso_fit, counties_train)
beta_hat_std %>%
  filter(coefficient != 0) %>%
  arrange(desc(abs(coefficient))) %>% 
  rename(Feature = feature, Coefficient = coefficient) %>%
  head(10)


# =========================RUN ELASTIC NET REGRESSION=======================
set.seed(471)
elnet_fit = cva.glmnet(median_household_income ~ .,   
                      nfolds = 10,               
                      data = counties_train)

elnet_fit$alpha

# save the elastic net fit object
save(elnet_fit, file = "results/model-results/elnet_fit.Rda")

# create elastic net CV plot for alpha
png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "results/model-results/elnet-cv-alpha-plot.png")
plot_cva_glmnet(elnet_fit)
dev.off()

# extract optimal elastic net
elnet_fit_best = extract_best_elnet(elnet_fit)

# create elastic net CV plot with optimal alpha
png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "results/model-results/elnet-cv-plot.png")
plot(elnet_fit_best)
dev.off()

# create elastic net trace plot
elnet_plot = plot_glmnet(elnet_fit_best, counties_train, features_to_plot = 6)
ggsave(filename = "results/model-results/elnet-trace-plot.png", 
       plot = elnet_plot, 
       device = "png", 
       width = 6.5, 
       height = 4)



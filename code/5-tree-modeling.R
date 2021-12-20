# load libraries
library(tidyverse)
library(rpart)            
library(rpart.plot)
library(randomForest)
library(gbm)
library(cowplot)
library(kableExtra)                     # for printing tables

# install.packages("magick")
# install.packages("webshot")
# webshot::install_phantomjs()

# read in the training data
counties_train = read_csv("data/clean/counties_train.csv")
set.seed(471)


# =========================RUN REGRESSION TREE==========================
set.seed(471)
tree_fit = rpart(median_household_income ~ ., 
                 data = counties_train %>%
                   select(-state, -name, -fips))

# save the tree fit object
save(tree_fit, file = "results/model-results/tree_fit.Rda")

# create tree diagram
png(width = 6, 
    height = 3,
    res = 300,
    units = "in", 
    filename = "results/model-results/tree-plot.png")
rpart.plot(tree_fit)
dev.off()

# save tree fit variable importances
tree_variable_importance = tibble(
  feature = names(tree_fit$variable.importance),
  importance = tree_fit$variable.importance,
) %>%
  rename(Feature = feature, Importance = importance) %>%
  head(10) %>%
  kable(format = 'latex', row.names = NA,
        booktabs = TRUE, digits = 2, linesep = "") %>%
  kable_styling() %>%
  save_kable(file ="results/model-results/tree-importances.png")

# generate CV plot
cp_table = printcp(tree_fit) %>% as_tibble()
tree_cv_plot = cp_table %>% 
  ggplot(aes(x = nsplit+1, y = xerror, 
             ymin = xerror - xstd, ymax = xerror + xstd)) + 
  geom_point() + geom_line() +
  geom_errorbar(width = 0.2) +
  xlab("Number of terminal nodes") + ylab("CV error") + 
  geom_hline(aes(yintercept = min(xerror)), linetype = "dashed") + 
  theme_bw()
ggsave(filename = "results/model-results/tree_cv_plot.png", 
       plot = tree_cv_plot, 
       device = "png", 
       width = 6.5, 
       height = 4)

# find optimal tree
optimal_tree_info = cp_table %>% 
  filter(xerror - xstd < min(xerror)) %>% 
  arrange(nsplit) %>% 
  head(1)

# save the tree fit object
tree_optimal_fit = prune(tree_fit, cp = optimal_tree_info$CP)
save(tree_optimal_fit, file = "results/model-results/tree_optimal_fit.Rda")

# create optimal tree diagram
png(width = 7, 
    height = 3,
    res = 300,
    units = "in", 
    filename = "results/model-results/tree-optimal-plot.png")
rpart.plot(tree_optimal_fit)
dev.off()

# save optimal tree fit variable importances
tree_optimal_variable_importance = tibble(
  feature = names(tree_optimal_fit$variable.importance),
  importance = tree_optimal_fit$variable.importance,
) %>%
  rename(Feature = feature, Importance = importance) %>%
  head(10) %>%
  kable(format = 'latex', row.names = NA,
        booktabs = TRUE, digits = 2, linesep = "") %>%
  kable_styling() %>%
  save_kable(file ="results/model-results/tree-optimal-importances.png")


# =========================RUN RANDOM FOREST===================================
set.seed(471)
rf_fit = randomForest(median_household_income ~ ., 
                      data = counties_train %>%
                        select(-state, -name, -fips))

# save the random forest object
save(rf_fit, file = "results/model-results/rf_fit.Rda")

# create OOB vs. number of trees plot
png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "results/model-results/rf-oob-error-vs-B-plot.png")
plot(rf_fit)
dev.off()

# create OOB vs. m plot
set.seed(471)
mvalues = seq(1,41, by = 3)
oob_errors = numeric(length(mvalues))
ntree = 100
for(idx in 1:length(mvalues)){
  m = mvalues[idx]
  rf_fit = randomForest(median_household_income ~ ., 
                        mtry = m,
                        data = counties_train %>%
                          select(-state, -name, -fips))
  oob_errors[idx] = rf_fit$mse[ntree]
}
m_and_oob_errors = tibble(m = mvalues, oob_err = oob_errors)
rf_oob_error_vs_m_plot = m_and_oob_errors %>%
  ggplot(aes(x = m, y = oob_err)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = mvalues) +
  labs(x = "Value for m", y = "OOB error") +
  theme_bw()
ggsave(filename = "results/model-results/rf-oob-error-vs-m-plot.png", 
       plot = rf_oob_error_vs_m_plot, 
       device = "png", 
       width = 6.5, 
       height = 4)

# extract m corresponding to min value of OOB error
best_m = m_and_oob_errors %>% arrange(oob_errors) %>% head(1) %>% pull(m)

# create tuned rf
set.seed(471)
rf_fit_tuned = randomForest(median_household_income ~ ., 
                            mtry = best_m, 
                            ntree = 500,
                            importance = TRUE, 
                            data = counties_train %>%
                              select(-state, -name, -fips))

# save the tuned random forest object
save(rf_fit_tuned, file = "results/model-results/rf_fit_tuned.Rda")

# variable importances
png(width = 12, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "results/model-results/rf-tuned-variable-importances.png")
varImpPlot(rf_fit_tuned, n.var = 10)
dev.off()

# create OOB vs. number of trees plot for tuned random forest
png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "results/model-results/rf-tuned-oob-error-vs-B-plot.png")
plot(rf_fit_tuned)
dev.off()



# =========================RUN BOOSTING MODEL===================================
set.seed(471)
# fit boosted model with interaction depth 1
gbm_fit_1 = gbm(median_household_income ~ .,
                distribution = "gaussian", 
                n.trees = 1000, 
                interaction.depth = 1, 
                shrinkage = 0.1,
                cv.folds = 5,
                data = counties_train %>%
                  select(-state, -name, -fips))

set.seed(471) 
# fit boosted model with interaction depth 2
gbm_fit_2 = gbm(median_household_income ~ .,
                distribution = "gaussian", 
                n.trees = 1000, 
                interaction.depth = 2, 
                shrinkage = 0.1,
                cv.folds = 5,
                data = counties_train %>%
                  select(-state, -name, -fips))

set.seed(471) 
# fit boosted model with interaction depth 3
gbm_fit_3 = gbm(median_household_income ~ .,
                distribution = "gaussian", 
                n.trees = 1000, 
                interaction.depth = 3, 
                shrinkage = 0.1,
                cv.folds = 5,
                data = counties_train %>%
                  select(-state, -name, -fips))


# extract CV errors
ntrees = 1000
cv_errors = bind_rows(
  tibble(ntree = 1:ntrees, cv_err = gbm_fit_1$cv.error, depth = 1), 
  tibble(ntree = 1:ntrees, cv_err = gbm_fit_2$cv.error, depth = 2), 
  tibble(ntree = 1:ntrees, cv_err = gbm_fit_3$cv.error, depth = 3)
)

# plot CV errors
boosted_cv_plot = cv_errors %>%
  ggplot(aes(x = ntree, y = cv_err, colour = factor(depth))) + 
  # add horizontal dashed lines at the minima of the three curves
  geom_hline(yintercept = min(gbm_fit_1$cv.error), 
             linetype = "dashed", color = "red") +
  geom_hline(yintercept = min(gbm_fit_2$cv.error), 
             linetype = "dashed", color = "green") +
  geom_hline(yintercept = min(gbm_fit_3$cv.error), 
             linetype = "dashed", color = "blue") +
  geom_line() + 
  # set colors to match horizontal line minima
  scale_color_manual(labels = c("1", "2", "3"), 
                     values = c("red", "green", "blue")) +
  labs(x = "Number of trees", y = "CV error", colour = "Interaction depth") +
  theme_bw()
ggsave(filename = "results/model-results/gbm_cv_plot.png", 
       plot = boosted_cv_plot, 
       device = "png", 
       width = 6.5, 
       height = 4)

# extract optimal boosted model
gbm_fit_tuned = gbm_fit_3
optimal_num_trees = gbm.perf(gbm_fit_3, plot.it = FALSE)
optimal_num_trees

# save the tuned boosted model object
save(gbm_fit_tuned, file = "results/model-results/gbm_fit_tuned.Rda")

# save boosted model feature importances
as_tibble(summary(gbm_fit_tuned, n.trees = optimal_num_trees, plotit = FALSE)) %>%
  rename(Feature = var, "Relative Influence" = rel.inf) %>%
  head(10) %>%
  kable(format = 'latex', row.names = NA,
        booktabs = TRUE, digits = 2, linesep = "") %>%
  kable_styling() %>%
  save_kable(file ="results/model-results/gbm-importances.png")


# plot partial dependence plots
p1 = plot(gbm_fit_tuned,
          i.var = "household_has_computer",
          n.trees = optimal_num_trees)
p2 = plot(gbm_fit_tuned,
          i.var = "household_has_broadband",
          n.trees = optimal_num_trees)
p3 = plot(gbm_fit_tuned,
          i.var = "household_has_smartphone",
          n.trees = optimal_num_trees)
p4 = plot(gbm_fit_tuned,
          i.var = "poverty",
          n.trees = optimal_num_trees)
gbm_dependence_plot = plot_grid(p1, p2, p3, p4, align = "h")
ggsave(filename = "results/model-results/gbm-dependence-plot.png", 
       plot = gbm_dependence_plot, 
       device = "png", 
       width = 8, 
       height = 6)

# plot partial dependence plot for mean work travel for examination
png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "results/model-results/gbm-travel-dependence-plot.png")
plot(gbm_fit_tuned,
     i.var = "mean_work_travel",
     n.trees = optimal_num_trees)
dev.off()





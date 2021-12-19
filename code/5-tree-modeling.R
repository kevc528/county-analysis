# load libraries
library(tidyverse)
library(rpart)            
library(rpart.plot)        

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

# create tree plot
png(width = 6, 
    height = 4,
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
  write_csv("results/model-results/tree-features-table.csv")

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

# create optimal tree plot
png(width = 7, 
    height = 4,
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
  write_csv("results/model-results/tree-optimal-features-table.csv")




library(tidyverse)
library(tidymodels)

starbucks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-21/starbucks.csv')
glimpse(starbucks)

starbucks %>%
  count(whip, sort = TRUE)

starbucks %>%
  ggplot(aes(trans_fat_g)) +
  geom_bar() +
  theme_minimal()



starbucks %>%
  ggplot(aes(fct_reorder(size, calories), calories, color = size)) +
  geom_boxplot() +
  geom_jitter(width = .2, alpha = 0.3) +
  theme_minimal() +
  labs(x = "Size") +
  theme(legend.position = "none",
        panel.grid = element_blank())

starbucks %>%
  ggplot(aes(fct_reorder(size, sugar_g), sugar_g, color = size)) +
  geom_boxplot() +
  geom_jitter(width = .2, alpha = 0.3) +
  theme_minimal() +
  labs(x = "Size") +
  theme(legend.position = "none",
        panel.grid = element_blank())

starbucks %>%
  ggplot(aes(fct_reorder(size, total_carbs_g), total_carbs_g, color = size)) +
  geom_boxplot() +
  geom_jitter(width = .2, alpha = 0.3) +
  theme_minimal() +
  labs(x = "Size") +
  theme(legend.position = "none",
        panel.grid = element_blank())

starbucks %>%
  ggplot(aes(fct_reorder(size, total_carbs_g), total_carbs_g, color = size)) +
  geom_boxplot() +
  geom_jitter(width = .2, alpha = 0.3) +
  theme_minimal() +
  labs(x = "Size") +
  theme(legend.position = "none",
        panel.grid = element_blank())

starbucks %>%
  ggplot(aes(sugar_g, calories)) +
  geom_point() +
  labs(x = "Sugar Grams") +
  facet_wrap(~size, scales = "free") +
  geom_smooth(method = 'lm') +
  theme_minimal()


starbucks %>%
  ggplot(aes(fct_reorder(as.character(milk), total_carbs_g), total_carbs_g, color = as.character(milk))) +
  geom_boxplot() +
  geom_jitter(width = .2, alpha = 0.3) +
  theme_minimal() +
  labs(x = "Milk") +
  theme(legend.position = "none",
        panel.grid = element_blank())

starbucks %>%
  ggplot(aes(fct_reorder(as.character(whip), total_carbs_g), total_carbs_g, color = as.character(whip))) +
  geom_boxplot() +
  geom_jitter(width = .2, alpha = 0.3) +
  theme_minimal() +
  labs(x = "Whip") +
  theme(legend.position = "none",
        panel.grid = element_blank())

starbucks %>%
  ggplot(aes(sodium_mg, calories, color = as.character(whip), group = as.character(whip))) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(color = "Whip?") +
  theme_minimal()


glimpse(starbucks)

starbucks %>%
  pivot_longer(-c(product_name, size, trans_fat_g, fiber_g)) %>%
  ggplot(aes(value)) +
  geom_histogram(bins = 25) +
  facet_wrap(~name, scales = "free") +
  theme_minimal()

starbucks %>%
  pivot_longer(c(product_name, size, trans_fat_g, fiber_g)) %>%
  ggplot(aes(value)) +
  geom_bar() +
  facet_wrap(~name)


# Xgboost to predict sugar grams

splits <- initial_split(starbucks, prop = .7)
train <- training(splits)
test <- testing(splits)


rec <- recipe(sugar_g ~ ., data = train) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_other(all_nominal_predictors(), threshold = .3)


train_df <- rec %>% prep(train) %>% juice()


rec %>%
  prep(train) %>%
  juice()

xgb_spec <- boost_tree(mode = 'regression',
                       engine = "xgboost", 
                       mtry = tune(), 
                       trees = tune(), 
                       learn_rate = tune(),
                       min_n = tune())

xg_wf <- workflow(rec, xgb_spec)

folds <- vfold_cv(train)

xgb_tune <- xg_wf %>%
  tune_grid(folds,
            metrics = metric_set(rmse, mae),
            control = control_grid(verbose = TRUE))

autoplot(xgb_tune)

xgb_best <- xg_wf %>%
  finalize_workflow(select_best(xgb_tune))

xgb_best_fit <- xgb_best %>%
  fit(train)


xgb_best_fit %>%
  augment(test) %>% 
  ggplot(aes(sugar_g, .pred)) +
  geom_point() +
  labs(x = "sugar_g actual",
       y = "sugar_g predicted") +
  geom_smooth(method = 'lm', alpha = 0.4) +
  theme_minimal()

xgb_best_fit %>%
  extract_fit_engine() %>%
  vip::vip(geom = 'point', num_features = 10) +
  geom_point(aes(color = "red")) +
  theme_minimal() +
  labs(title = "Variable Importance XG-Boost") +
  theme(panel.grid = element_blank(),
        legend.position = 'none',
        plot.title = element_text(hjust = 0.5, face = 'bold', size = 17))
  




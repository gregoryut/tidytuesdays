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

















library(tidyverse)


food_consumption <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')

head(food_consumption)

# USA Pork
food_consumption %>%
  filter(country == "USA") %>%
  pivot_longer(-c(country, food_category)) %>%
  mutate(name = ifelse(name == "co2_emmission", "CO2 Emmission", "Consumption")) %>%
  ggplot(aes(fct_reorder(food_category, value), value)) +
  geom_col() +
  labs(x = NULL,
       title = "USA Food Consumption and Emmissions.",
       y = NULL) +
  coord_flip() +
  facet_wrap(~name,
             scales = "free") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, face = 'bold', size = 16))

# USA And Canada
food_consumption %>%
  filter(country %in% c("USA", "Canada")) %>%
  pivot_longer(-c(country, food_category)) %>%
  mutate(name = ifelse(name == "co2_emmission", "CO2 Emmission", "Consumption")) %>%
  ggplot(aes(fct_reorder(food_category, value), value)) +
  geom_col() +
  labs(x = NULL,
       title = "USA & Canada Food Consumption and Emmissions.",
       y = NULL) +
  coord_flip() +
  facet_wrap(country~name) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, face = 'bold', size = 16))

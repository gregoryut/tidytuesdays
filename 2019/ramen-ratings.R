library(tidyverse)
theme_set(theme_minimal())

ramen_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-04/ramen_ratings.csv")

glimpse(ramen_ratings)

ramen_ratings %>%
  mutate(country = ifelse(country == "United States", "USA", country)) %>%
  group_by(country) %>%
  summarise(avg_star = mean(stars, na.rm = TRUE),
            count = n()) %>%
  mutate(weight = count / sum(count),
         avg_star_w = (avg_star * weight)*5) %>%
  arrange(desc(avg_star)) %>%
  ggplot(aes(fct_reorder(country, avg_star), avg_star)) +
  geom_col() +
  coord_flip() +
  labs(x = "Country",
       y = "Average Stars",
       title = "Average Ramen Rating Stars by Country.",
       subtitle = "Not weighted Average.")

ramen_ratings %>%
  mutate(country = ifelse(country == "United States", "USA", country)) %>%
  group_by(country) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  ggplot(aes(fct_reorder(country, count), count)) +
  geom_col() +
  coord_flip() +
  labs(x = "Country",
       y = "Count",
       title = "Amount of Ratings by Country.")

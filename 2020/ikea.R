library(tidyverse)


ikea <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-03/ikea.csv')
glimpse(ikea)

ikea %>%
  group_by(designer) %>%
  summarise(median_price = median(price, na.rm = TRUE)) %>%
  arrange(desc(median_price)) %>%
  head(15) %>%
  ggplot(aes(fct_reorder(designer, median_price), median_price)) +
  geom_col() +
  coord_flip() +
  labs(x = "Designer",
       y = "Median Saudi Royals Price") +
  theme_minimal()


ikea %>%
  ggplot(aes(fct_reorder(category, price), price, color = category)) +
  geom_boxplot() +
  coord_flip() +
  theme_minimal() +
  labs(x = "Category") +
  theme(legend.position = 'none') 

library(tidyverse)

passwords <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv')
glimpse(passwords)


passwords %>%
  count(category, sort = TRUE)

passwords %>%
  drop_na(category) %>%
  ggplot(aes(fct_reorder(category, strength), strength, color = category)) +
  geom_boxplot() +
  labs(x = "Category",
       y = "Strength",
       title = "Password Strength by Cateogry.") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'none')


passwords %>%
  mutate(p_cat = case_when(
    str_detect(password, "^[a-z]+$") ~ "lowercase",
    str_detect(password, "^[0-9]+$") ~ "numeric",
    str_detect(password, "[:alnum:]") ~ "alphanumeric",
    str_detect(password, "[:punct:]") ~ "punctutation"
  )) %>%
  drop_na(p_cat) %>%
  ggplot(aes(p_cat, strength, color = p_cat)) +
  geom_violin() +
  labs(x = "Password Category",
       color = "Category",
       title = "Password Strength by Category") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

passwords %>%
  count(category) %>%
  drop_na() %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(fct_reorder(category, prop), prop, fill = category)) +
  geom_col() +
  labs(x = "category",
       y = NULL,
       title = "Proportion of passwords by category.") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = 'none',
        plot.title = element_text(hjust = 0.5, face = "bold", size = 19),
        panel.grid = element_blank())








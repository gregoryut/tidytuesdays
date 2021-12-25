library(tidyverse)


spiders <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-07/spiders.csv')
glimpse(spiders)

spiders %>%
  mutate(decade = round(year / 10) * 10,
         dcol = ifelse(decade == 2010, "salmon", "darkgrey")) %>%
  count(decade, dcol, sort = TRUE) %>%
  ggplot(aes(decade, n, fill = dcol)) +
  geom_col() +
  theme_minimal() +
  scale_fill_identity() +
  labs(x = "Decade",
       y = "Count of Spiders",
       title = "Total spiders by decade.") +
  scale_y_continuous(breaks = seq(0, 8000, 1000)) +
  scale_x_continuous(breaks = seq(1750, 2021, 20)) +
  annotate(geom = "curve",
           x = 1985, 
           y = 7700,
           xend = 2000,
           yend = 6500) +
  annotate(geom = "text", x = 1990,
           y = 8000, 
           label = "6900 spiders!",
           family = "sans",
           size = 5) +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, 
                                  face = "bold",
                                  size = 20))


spiders %>%
  mutate(decade = round(year / 10) * 10,
         dcol = ifelse(decade == 2010, "salmon", "darkgrey")) %>%
  group_by(decade) %>%
  summarise(n_count = n_distinct(genus)) %>%
  ggplot(aes(decade, n_count)) +
  geom_line() +
  labs(x = "Decade",
       y = "Count",
       title = "Count of Unique Genus.") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20))



spiders %>%
  mutate(decade = round(year / 10) * 10,
         dcol = ifelse(decade == 2010, "salmon", "darkgrey")) %>%
  group_by(decade) %>%
  summarise(n_count = n_distinct(family)) %>%
  ggplot(aes(decade, n_count)) +
  geom_line() +
  labs(x = "Decade",
       y = "Count",
       title = "Count of Unique Famlieis") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20))


spiders %>%
  count(family) %>%
  arrange(desc(n)) %>%
  head(15) %>%
  ggplot(aes(n, fct_reorder(family, n), fill = n)) +
  geom_col() +
  labs(x = NULL,
       y = "Family",
       title = "Counts of Spiders by Family",
       subtitle = "A lot of Salticidae & Linyphiidae!") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 17),
        legend.position = "none")

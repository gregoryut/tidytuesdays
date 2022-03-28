library(tidyverse)


babynames <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-22/babynames.csv')
glimpse(babynames)

babynames %>%
  filter(year %in% c(1960, 1980, 1990, 2000, 2005, 2010, 2013)) %>%
  group_by(year, sex, name) %>%
  summarise(avg_prop = mean(prop, na.rm = TRUE)) %>%
  arrange(desc(avg_prop)) %>%
  ungroup() %>%
  group_by(year, sex) %>%
  slice_head(n = 5) %>%
  ungroup() %>%
  ggplot(aes(fct_reorder(name, avg_prop), avg_prop, fill = sex)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~year, scales = "free") +
  labs(x = "Popular Name",
       y = "Average Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 70))

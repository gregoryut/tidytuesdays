library(tidyverse)
library(gghighlight)
theme_set(theme_minimal())

matches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-30/matches.csv')

glimpse(matches)

min(matches$match_date)
max(matches$match_date)

matches %>%
  separate(match_date, c("month_day", "year"), sep = ",") %>%
  mutate(year = str_trim(year)) %>%
  group_by(winner, year) %>%
  summarise(n = n()) %>%
  filter(winner %in% c("South Africa",
                       "Pakistan", 
                       "India", 
                       "Australia", 
                       "England",
                       "Sri Lanka",
                       "New Zealand",
                       "West Indies",
                       "Zimbabwe")) %>%
  ggplot(aes(year, n, group = winner, color = winner)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  gghighlight(winner == "Australia") +
  labs(title = "Total Wins By Country Over Years.",
       x = NULL,
       y = "Total Wins") +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16))

matches %>%
  group_by(winner) %>%
  summarise(n = n()) %>%
  top_n(10) %>%
  ggplot(aes(fct_reorder(winner, n), n)) +
  geom_col(alpha = 0.6) +
  labs(x = NULL,
       y = "Number of Wins",
       title = "Which Countries Have The Most Wins?") +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
  


matches %>%
  separate(match_date, c("month_day", "year"), sep = ",") %>%
  mutate(year = str_trim(year)) %>%
  group_by(winner, year) %>%
  summarise(n = n()) %>%
  filter(winner %in% c("South Africa",
                       "Pakistan", 
                       "India", 
                       "Australia", 
                       "England",
                       "Sri Lanka",
                       "New Zealand",
                       "West Indies",
                       "Zimbabwe")) %>%
  ggplot(aes(fct_reorder(winner, n), n, fill = winner)) +
  geom_col(alpha = 0.6) +
  facet_wrap(~year) +
  coord_flip() +
  labs(x = NULL,
       y = "Number of Wins",
       title = "Which Countries Have The Most Wins by Year?") +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

library(tidyverse)
library(lubridate)

ultra_rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/ultra_rankings.csv')
race <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/race.csv')

glimpse(ultra_rankings)
glimpse(race)

# check NA values by column
map(ultra_rankings, ~sum(is.na(.))) %>% unlist
map(race, ~sum(is.na(.))) %>% unlist

race %>%
  mutate(year = year(date)) %>%  
  group_by(year, country)
  

ultra_rankings %>%
  left_join(race, by = c("race_year_id" = "race_year_id")) %>%
  mutate(year = year(date)) %>%  
  group_by(year) %>%
  summarise(n = n()) %>%
  ggplot(aes(year, n)) +
  scale_x_continuous(breaks = seq(2012, 2021, 1)) +
  geom_line(color = "lightblue3", size = 2) +
  theme_minimal() +
  labs(title = "Total Runners By Year.",
       y = NULL,
       x = "Year") +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 17, family = "sans"))


ultra_rankings %>%
  left_join(race, by = c("race_year_id" = "race_year_id")) %>%
  mutate(year = year(date)) %>% 
  group_by(year, gender) %>%
  summarise(mean_dist = mean(distance, na.rm = TRUE)) %>%
  drop_na() %>%
  ggplot(aes(year, mean_dist, color = gender)) +
  geom_line() +
  theme_minimal() +
  labs(x = "Year",
       y = "Average Distance") + 
  theme(panel.grid = element_blank())



ultra_rankings %>%
  left_join(race, by = c("race_year_id" = "race_year_id")) %>%
  count(country) %>%
  mutate(prop = (n / sum(n))) %>%
  top_n(10) %>%
  ggplot(aes(fct_reorder(country, n), n, label = scales::percent(prop, accuracy = 0.1))) +
  geom_col() +
  geom_text(position = position_dodge(width = 0.9),
            hjust = -0.2) + 
  expand_limits(y = 55000) + 
  coord_flip() +
  labs(x = "Country",
       y = NULL,
       title = "Top countries with Participants.") + 
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, family = "sans", size = 17))



# by mean age/gender
ultra_rankings %>%
  left_join(race, by = c("race_year_id" = "race_year_id")) %>%
  mutate(year = year(date)) %>% 
  group_by(year, gender) %>%
  summarise(avg_age = mean(age, na.rm = TRUE)) %>%
  drop_na() %>%
  ggplot(aes(year, avg_age, color = gender)) +
  geom_line(size = 2) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(2012, 2021, 1)) + 
  labs(x = "Year",
       y = "Average Age",
       title = "Average Age of Runners by Gender Over Time.") +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, family = 'sans', size = 17))



ultra_rankings %>%
  left_join(race, by = c("race_year_id" = "race_year_id")) %>%
  mutate(year = year(date),
         time = hms(as.character(time), quiet = TRUE),
         duration = hour(time) + minute(time)/60 + second(time)/3600,
         speed = distance/duration) %>%
  group_by(year, country) %>%
  summarise(avg_speed = mean(speed, na.rm = TRUE), .groups = "drop",
            n = n()) %>%
  drop_na() %>%
  filter(avg_speed > 0) %>%
  ggplot(aes(year, avg_speed)) +
  geom_line() +
  geom_point() +
  facet_wrap(~country, scales = "free_y") +
  theme_minimal() +
  labs(x = NULL,
       y = "Average Speed",
       title = "Average Speed of runners by Country.") +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18))








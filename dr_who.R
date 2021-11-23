library(tidyverse)
library(lubridate)
theme_set(theme_minimal())

directors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/directors.csv')
episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/episodes.csv')
writers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/writers.csv')
imdb <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/imdb.csv')

glimpse(directors)
glimpse(episodes)
glimpse(writers)
glimpse(imdb)



# boxplot Viewers by season
episodes %>%
  drop_na(season_number, uk_viewers) %>%
  mutate(season_number = factor(season_number)) %>%
  ggplot(aes(uk_viewers, season_number)) +
  geom_boxplot(aes(color = season_number)) +
  labs(title = 'UK Viewers In Millions by Season',
       x = 'UK Viewers in Millions',
       y = 'Season Number') +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16))

# bar plot of Viewers by season
episodes %>%
  drop_na(season_number) %>%
  filter(episode_number == 1) %>%
  mutate(season_number = factor(season_number),
         season_number = fct_reorder(season_number, uk_viewers)) %>%
  ggplot(aes(uk_viewers, season_number, fill = uk_viewers)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Uk Viewes Millions",
       y = "Season Number",
       title = 'UK Viewers In Millions by Season') +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16))

# line plot of viewers by episode number and season
episodes %>%
  filter(season_number != 13) %>%
  ggplot(aes(episode_number, uk_viewers, color = factor(season_number))) +
  geom_line() +
  facet_wrap(~season_number) +
  labs(title = "UK Viewers by Season over Episode Number",
       x = "Episode Number",
       y = "UK Viewers in Millions") +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        legend.position = "none")

# barplot of mean rating by year
imdb %>%
  mutate(air_date = dmy(air_date),
         year = year(air_date)) %>%
  group_by(year) %>%
  summarize(mean_rate = mean(rating, na.rm = TRUE)) %>%
  mutate(year = as.character(year),
         mean_rate = round(mean_rate, 1)) %>%
  ggplot(aes(mean_rate, year)) +
  geom_bar(stat = "identity",
           position = "dodge",
           fill = "lightblue3") +
  geom_text(aes(label = mean_rate), 
            position = position_dodge(width = .9),
            hjust = -0.15,
            size = 4.17) +
  labs(title = "Average Rating By Year") +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank()) 


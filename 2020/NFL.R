library(tidyverse)



attendance <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/attendance.csv')
standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/standings.csv')
games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/games.csv')

glimpse(attendance)
glimpse(standings)
glimpse(games)


attendance %>%
  group_by(year) %>%
  summarise(weekly_attendance = sum(weekly_attendance, na.rm = TRUE)) %>%
  ggplot(aes(year, weekly_attendance)) +
  geom_line() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(2000, 2019, 2)) +
  theme_minimal() +
  labs(x = NULL,
       y = "Attendance",
       title = "Yearly Attendance By Year.") +
  ggeasy::easy_center_title()


attendance %>%
  group_by(year, team_name) %>%
  summarise(weekly_attendance = sum(weekly_attendance, na.rm = TRUE)) %>%
  ggplot(aes(year, weekly_attendance, color = team_name)) +
  geom_line() +
  facet_wrap(~team_name, scales = "free") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(2000, 2019, 6)) +
  theme_minimal() +
  labs(x = NULL,
       y = "Attendance",
       title = "Attendance by Team Through Time.") +
  theme(panel.grid = element_blank(),
        legend.position = 'none') +
  ggeasy::easy_center_title()

standings %>%
  group_by(team_name) %>%
  summarise(tot_wins = sum(wins, na.rm = TRUE)) %>%
  arrange(desc(tot_wins)) %>%
  head(20) %>%
  ggplot(aes(fct_reorder(team_name, tot_wins), tot_wins)) +
  geom_col() +
  coord_flip() +
  labs(x = "Team Name",
       y = "Total Wins",
       title = "Total Wins by Team.") +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  ggeasy::easy_center_title()





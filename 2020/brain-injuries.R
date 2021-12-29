library(tidyverse)
options(scipen = 13515)


tbi_age <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_age.csv')
tbi_year <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_year.csv')
tbi_military <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_military.csv')

head(tbi_age)
head(tbi_military)
head(tbi_year)

tbi_age %>%
  ggplot(aes(fct_reorder(age_group, rate_est), rate_est, color = age_group)) +
  geom_boxplot() +
  #scale_y_log10() +
  theme_minimal() +
  labs(x = "Age Group",
       y = "Rate",
       title = "Estimated Rate Per 100k, 2014.") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 17),
        panel.grid = element_blank(),
        legend.position = 'none')


tbi_age %>% 
  ggplot(aes(fct_reorder(injury_mechanism, rate_est), rate_est, color = injury_mechanism)) +
  geom_boxplot() +
  scale_y_log10() +
  theme_minimal() +
  labs(x = "Injury Mechanism",
       y = "Rate",
       title = "Logged Estimated Rate Per 100k, 2014.") +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 17),
        panel.grid = element_blank(),
        legend.position = 'none')

tbi_age %>%
  group_by(age_group) %>%
  summarise(rate_est = median(rate_est, na.rm = TRUE)) %>%
  ggplot(aes(fct_reorder(age_group, rate_est), rate_est)) +
  geom_col() +
  labs(x = "Age Group",
       y = "Rate",
       title = "Median Estimated Rate Per 100k, 2014.") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 17),
        panel.grid = element_blank())

tbi_military %>%
  group_by(year, service) %>%
  summarise(tot_diagnosed = sum(diagnosed, na.rm=TRUE)) %>%
  ggplot(aes(year, tot_diagnosed, color = service)) +
  geom_line() +
  labs(y = "Total Diagnosed",
       x = "Year",
       title = "Diagnosed with Brain Injury by Service Type.") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, face = 'bold', size = 17))


tbi_military %>%
  group_by(year, component) %>%
  summarise(tot_diagnosed = sum(diagnosed, na.rm=TRUE)) %>%
  ggplot(aes(year, tot_diagnosed, color = component)) +
  geom_line() +
  labs(y = "Total Diagnosed",
       x = "Year",
       title = "Diagnosed with Brain Injury by Component Type.") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, face = 'bold', size = 17))

tbi_military %>%
  group_by(year, severity) %>%
  summarise(tot_diagnosed = sum(diagnosed, na.rm=TRUE)) %>%
  ggplot(aes(year, tot_diagnosed, color = severity)) +
  geom_line() +
  labs(y = "Total Diagnosed",
       x = "Year",
       title = "Diagnosed with Brain Injury by Severity.") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, face = 'bold', size = 17))

tbi_year %>%
  group_by(year, type) %>%
  summarise(number_est = sum(number_est, na.rm = TRUE)) %>%
  ggplot(aes(year, number_est, color = type)) +
  geom_line() +
  facet_wrap(~type, scales = "free") +
  scale_x_continuous(breaks = seq(2006, 2014, 4)) +
  theme_minimal() +
  labs(title = "Brain Injuries by Type Over Time.") +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18, face = 'bold'))

tbi_year %>%
  group_by(year, injury_mechanism) %>%
  summarise(number_est = sum(number_est, na.rm = TRUE)) %>%
  filter(injury_mechanism != "Total") %>%
  ggplot(aes(year, number_est, color = injury_mechanism)) +
  geom_line() +
  facet_wrap(~injury_mechanism, scales = "free") +
  scale_x_continuous(breaks = seq(2006, 2014, 4)) +
  theme_minimal() +
  labs(x = "Year",
       y = "Estimated Number",
       title = "Brain Injuries by injury_mechanism Over Time.") +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18, face = 'bold'))

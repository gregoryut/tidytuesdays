library(tidyverse)
library(sf)


colony <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/colony.csv')
stressor <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/stressor.csv')

glimpse(colony)
glimpse(stressor)

stressor %>%
  group_by(year, stressor) %>%
  summarise(stress_pct = mean(stress_pct, na.rm = TRUE)) %>%
  ggplot(aes(year, stress_pct, color = stressor)) +
  geom_line(size = 1.5) +
  theme_minimal()


stressor %>%
  group_by(year, state, stressor) %>%
  summarise(stress_pct = mean(stress_pct, na.rm = TRUE)) %>%
  pivot_longer(-c(state, stressor, year)) %>%
  filter(state != "United States") %>%
  ggplot(aes(year, value, color = stressor)) +
  geom_line() +
  facet_wrap(~state, scales = "free") +
  theme_minimal() +
  theme(panel.grid = element_blank())

colony %>%
  group_by(year) %>%
  summarise(colony_n = sum(colony_n, na.rm = TRUE)) %>%
  ggplot(aes(year, colony_n)) +
  geom_line()

colony %>%
  group_by(year, state) %>%
  summarise(colony_n = sum(colony_n, na.rm = TRUE)) %>%
  pivot_longer(-c(state, year)) %>%
  filter(state != "United States") %>%
  ggplot(aes(year, value)) +
  geom_line() +
  facet_wrap(~state, scales = "free") +
  theme_minimal() +
  theme(panel.grid = element_blank())

colony %>%
  group_by(year, state) %>%
  summarise(colony_lost = sum(colony_lost, na.rm = TRUE)) %>%
  pivot_longer(-c(state, year)) %>%
  filter(state != "United States") %>%
  ggplot(aes(year, value)) +
  geom_line() +
  facet_wrap(~state, scales = "free") +
  theme_minimal() +
  theme(panel.grid = element_blank())

colony %>%
  group_by(year, state) %>%
  summarise(colony_reno_pct = mean(colony_reno_pct, na.rm = TRUE)) %>%
  pivot_longer(-c(state, year)) %>%
  filter(state != "United States") %>%
  ggplot(aes(year, value)) +
  geom_line() +
  facet_wrap(~state, scales = "free") +
  theme_minimal() +
  theme(panel.grid = element_blank())

colony %>%
  group_by(year) %>%
  summarise(colony_lost_pct = mean(colony_lost_pct, na.rm = TRUE)) %>%
  mutate(pct_change = (colony_lost_pct / lag(colony_lost_pct) - 1) * 100) %>%
  ggplot(aes(year, colony_lost_pct)) +
  geom_col(alpha = 0.7) +
  geom_text(aes(label = paste(format(round(pct_change, 2), nsmall = 2), "%")), vjust = 1.5, color = "white") +
  theme_minimal() +
  labs(x = NULL,
       y = "Colony Lost %",
       title = "Percent % Colonies Lost Over Time.") +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, face = 'bold', size = 15))


colony %>%
  select(months, starts_with("colony"), -contains("pct")) %>%
  pivot_longer(-c(months)) %>% 
  group_by(months, name) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  drop_na() %>%
  ggplot(aes(fct_reorder(months, value), value)) +
  geom_col() +
  facet_wrap(~name, scales = 'free') +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  coord_flip() +
  theme(axis.text.x = element_text(angle = -40, hjust = 0))

colony %>%
  group_by(year, state) %>%
  summarise(colony_n = sum(colony_n, na.rm = TRUE)) %>%
  filter(state != "United States") %>%
  ggplot(aes(year, colony_n)) +
  geom_line() +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~state, scales = 'free') +
  labs(title = "Bee Colony Counts by State.") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = 'bold'))


### map
states <- tigris::states(cb = TRUE)


sts <- states %>% 
  st_transform(., 5070)
  
cc <- colony %>%
  group_by(year, state) %>%
  summarise(colony_n = mean(colony_n, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = year,
              values_from = colony_n) %>%
  janitor::clean_names() %>%
  mutate(diff = x2020 - x2015) %>%
  mutate(diff = replace_na(diff, 0)) %>%
  select(state, diff)

sts %>%
  inner_join(cc, by = c("NAME" = "state")) %>%
  filter(NAME != "Hawaii") %>% # for nicer visual
  ggplot(aes(fill = diff)) +
  geom_sf(lwd = 0.3) +
  scale_fill_gradient2(labels = scales::comma) +
  theme_minimal() +
  labs(title = "Average Loss/Gain of Bee Colonies From 2020 to 2015.") +
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 15, face = 'bold'))

# same thing but for colonies lost
cc2 <- colony %>%
  group_by(year, state) %>%
  summarise(colony_lost_pct = mean(colony_lost_pct, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = year,
              values_from = colony_lost_pct) %>%
  janitor::clean_names() %>%
  mutate(diff = x2020 - x2015) %>%
  mutate(diff = replace_na(diff, 0)) %>%
  select(state, diff)

sts %>%
  inner_join(cc2, by = c("NAME" = "state")) %>%
  filter(NAME != "Hawaii") %>% # for nicer visual
  ggplot(aes(fill = diff)) +
  geom_sf(lwd = 0.3) +
  scale_fill_gradient2(labels = scales::comma) +
  theme_minimal() +
  labs(title = "Average Percent % Loss/Gain Bee Colonies From 2020 to 2015.") +
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 15, face = 'bold'))

# same thing but for 
ss <- stressor %>%
  group_by(year, state, stressor) %>%
  summarise(stress_pct = mean(stress_pct, na.rm = TRUE), .groups = "drop") %>%
  filter(stressor == "Pesticides") %>%
  pivot_wider(names_from = year,
              values_from = stress_pct) %>%
  janitor::clean_names() %>%
  mutate(diff = x2020 - x2015) %>%
  mutate(diff = replace_na(diff, 0)) %>%
  select(state, diff)

sts %>%
  inner_join(ss, by = c("NAME" = "state")) %>%
  filter(NAME != "Hawaii") %>% # for nicer visual
  ggplot(aes(fill = diff)) +
  geom_sf(lwd = 0.3) +
  scale_fill_gradient2(labels = scales::comma) +
  theme_minimal() +
  labs(title = "Average Percent % Of Stressor from Pesticides \nOn Bee Colonies From 2020 to 2015.") +
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 15, face = 'bold'))

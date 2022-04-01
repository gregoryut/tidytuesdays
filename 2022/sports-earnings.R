library(tidyverse)


sports <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-29/sports.csv')
glimpse(sports)

sports %>%
  count(sports, sort = TRUE)

sports %>%
  filter(year == 2019) %>%
  filter(sports %in% c("Basketball", "Soccer", "Baseball", "Volleyball",
                       "Tennis", "All Track Combined")) %>%
  pivot_longer(c(rev_men, rev_women)) %>% 
  group_by(name, sports) %>%
  summarise(tot_rev = sum(value, na.rm = TRUE)) %>%
  ggplot(aes(sports, tot_rev, fill = name)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::dollar) +
  theme_minimal()

sports %>%
  filter(year == 2019) %>%
  group_by(classification_name) %>%
  summarise(tot_rev = sum(total_rev_menwomen, na.rm = TRUE)) %>%
  arrange(desc(tot_rev)) %>%
  gt::gt()

sports %>%
  filter(sports %in% c("Basketball", 
                       "Soccer",
                       "Volleyball",
                       "Softball",
                       "Tennis",
                       "Football")) %>% 
  pivot_longer(c(rev_men, rev_women)) %>%
  mutate(gender = ifelse(name %in% c("rev_men"), "men", "women")) %>%
  group_by(year, sports, gender) %>%
  summarise(tot_val = sum(value, na.rm = TRUE)) %>%
  ggplot(aes(year, tot_val, group = gender, color = gender)) +
  geom_line(size = 1.5) +
  theme_minimal() +
  labs(x = NULL,
       y = "Total Revenue.") +
  scale_y_continuous(labels = scales::dollar) +
  scale_color_manual(values = c("#23ccff", "#ffc800")) +
  theme(panel.grid = element_blank()) +
  facet_wrap(~sports, scales = "free")

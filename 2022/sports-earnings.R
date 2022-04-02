library(tidyverse)
library(gt)

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
  group_by(classification_name, year) %>%
  summarise(tot_rev = sum(total_rev_menwomen, na.rm = TRUE), 
            .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = tot_rev, names_prefix = "rev_") %>%
  arrange(desc(rev_2019)) %>%
  gt(rowname_col = "classification_name") %>%
  fmt_currency(columns = starts_with("rev")) %>%
  cols_label(
    classification_name = "League Name",
    rev_2015 = "Revenue 2015",
    rev_2016 = "Revenue 2016",
    rev_2017 = "Revenue 2017",
    rev_2018 = "Revenue 2018",
    rev_2019 = "Revenue 2019"
  ) %>%
  tab_header(title = "League Revenue from 2015-2019") %>%
  opt_align_table_header(align = "left") %>%
  opt_horizontal_padding(scale = 2.5) %>%
  opt_vertical_padding(scale = 0.5) %>%
  opt_table_font(font = google_font(name = "Karla")) 

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
  summarise(tot_val = sum(value, na.rm = TRUE),
            .groups = "drop") %>%
  ggplot(aes(year, tot_val, group = gender, color = gender)) +
  geom_line(size = 1.5) +
  theme_minimal() +
  labs(x = NULL,
       y = "Total Revenue.",
       title = "Total Revenue Growth by Sport in USA.") +
  scale_y_continuous(labels = scales::dollar) +
  scale_color_manual(values = c("#23ccff", "#ffc800")) +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18, face = 'bold')) +
  facet_wrap(~sports, scales = "free")

library(tidyverse)
library(here)
theme_set(ggthemes::theme_hc())

## trying to recreate The Guardian plots
# https://www.theguardian.com/world/2021/jan/08/animal-rescues-london-fire-brigade-rise-2020-pandemic-year

animal_rescues <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-29/animal_rescues.csv'
  )


animal_rescues %>%
  filter(cal_year != 2021) %>%
  group_by(cal_year) %>%
  summarise(n = n()) %>%
  ggplot(aes(cal_year, n)) +
  geom_point(aes(size = n)) +
  scale_x_continuous(breaks = seq(2009,2020, 1)) + 
  geom_line() +
  labs(x = "",
       y = "",
       title = "Count of animal rescues by year.") +
  theme_light()


animal_rescues %>%
  group_by(cal_year) %>%
  summarise(n = n()) %>%
  filter(cal_year != 2021) %>%
  mutate(color2020 = if_else(cal_year == 2020, "salmon", "grey")) %>%
  ggplot(aes(cal_year, n, fill = color2020)) +
  geom_col() +
  scale_fill_identity() +
  scale_x_continuous(breaks = c(2009:2020)) +
  expand_limits(y = 800) + 
  labs(x = "",
       y = "",
       title = "London firefighters attended 755 animal rescues across the capital in 2020.") +
  theme(plot.title = element_text(family = "sans", hjust = 0.5, face = "bold", size = 14),
        panel.grid.major.y = element_line(color = "grey", size = 0.2),
        panel.background = element_blank(),
        axis.text = element_text(family = "sans", size = 10))
  

#ggsave('bar_plot_counts.png', dpi = 400)


animal_rescues %>%
  filter(cal_year %in% c(2019, 2020)) %>%
  mutate(
    animal_group_parent = str_to_title(animal_group_parent),
    animal_group_parent = if_else(
      animal_group_parent %in% c(
        "Unknown - Domestic Animal Or Pet",
        "Unknown - Wild Animal",
        "Squirrel",
        "Rabbit",
        "Snake",
        "Unknown - Heavy Livestock Animal",
        "Ferret",
        "Hamster",
        "Cow",
        "Goat",
        "Hedgehog",
        "Sheep",
        "Unknown - Animal Rescue From Below Ground - Farm Animal",
        "Unknown - Animal Rescue From Water - Farm Animal"
      ),
      "Other",
      animal_group_parent
    )
  ) %>%
  group_by(cal_year, animal_group_parent) %>%
  summarise(n = n()) %>%
  ggplot(aes(fct_reorder(animal_group_parent, n), n, fill = factor(cal_year))) +
  geom_bar(stat = "identity",
           position = "dodge",
           width = 0.5) +
  scale_x_discrete(limits = c("Other", "Horse", "Deer", "Fox", "Dog", "Bird", "Cat")) +
  scale_y_continuous(breaks = seq(0, 300, 50)) +
  coord_flip() +
  scale_fill_manual(values = c("#b3b3b4", "#cc0a14")) +
  labs(
    fill = "Year",
    y = "",
    x = "",
    title = "Cats accounted for 45% of London fire brigade animal rescues, but the\n biggest proportional increases were among birds and foxes.",
    caption = "\nGuardian graphic recreation by greg_ut | Data Source: London fire brigade"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      family = "sans",
      hjust = 0.5,
      face = "bold",
      size = 14
    ),
    axis.title = element_text(family = "sans", face = "bold"),
    axis.text = element_text(family = "sans", size = 12),
    legend.position = "top",
    legend.justification = "left",
    plot.caption = element_text(hjust = 0, color = "darkgrey"),
    plot.caption.position = "plot",
    aspect.ratio = 0.4,
    panel.grid.major.y = element_blank()
  )

#ggsave("animal_bars.png", dpi = 400)

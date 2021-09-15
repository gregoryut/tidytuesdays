library(tidyverse)


birds_raw <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-31/bird_baths.csv')

glimpse(birds_raw)

birds_raw %>%
  count(bioregions, bird_count, sort = TRUE)



birds_raw %>%
  group_by(survey_year, bioregions, urban_rural) %>%
  summarise(birds = sum(bird_count, na.rm = TRUE)) %>%
  group_by(bioregions, urban_rural) %>%
  filter(n() != 1) %>%
  ungroup() %>%
  ggplot(aes(survey_year, birds)) +
  geom_point(aes(color = urban_rural)) + 
  geom_line(aes(color = urban_rural)) +
  facet_wrap( ~ bioregions,
              nrow = 1,
              labeller = label_wrap_gen(width = 18, multi_line = TRUE)) +
  theme_minimal() +
  scale_x_continuous(breaks = c(min(birds_raw$survey_year, na.rm = TRUE),
                                max(birds_raw$survey_year, na.rm = TRUE))) +
  labs(
    x = "Survey Year",
    y = "Number of Species Observed",
    color = "Urban/Rural",
    title = "Count of bird baths by bioregion and area."
  ) +
  theme(
    plot.title = element_text(family = "sans", size = 15), 
    legend.position = "top",
    panel.spacing = unit(1, units = "lines"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = "white")
  ) +
  ggeasy::easy_center_title()

ggsave("birdbaths.png", width = 8, height = 5)




























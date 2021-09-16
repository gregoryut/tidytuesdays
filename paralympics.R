library(tidyverse)

athletes <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-03/athletes.csv'
  )

athletes %>%
  count(event, sort = TRUE)

athletes %>%
  count(type, sort = TRUE)

athletes %>%
  count(gender, sort = TRUE)


athletes %>%
  count(gender, medal, abb) %>%
  arrange(desc(n)) %>%
  top_n(25) %>%
  ggplot(aes(gender, n, fill = medal)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(y = "",
       x = "",
       title = "Medal Count by Gender",
       fill = "") +
  theme(
    legend.position = "top",
    legend.box.background = element_rect(color = "black", size = 1),
    axis.title.x = element_text(size = 20, color = "blue")
  ) +
  ggeasy::easy_center_title()


athletes %>%
  group_by(year, medal) %>%
  summarise(n = n()) %>%
  ggplot(aes(year, n, color = medal, group = medal)) +
  geom_point(aes(size = n)) +
  geom_line() +
  labs(x = "",
       y = "",
       title = "Is there less participants?") +
  scale_x_continuous(breaks = seq(1980, 2016, 4)) +
  theme(
    plot.background = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "top",
    panel.background = element_blank(),
    axis.text = element_text(
      family = "sans",
      face = "bold",
      size = 12
    ),
    axis.line = element_line(color = "grey1"),
    plot.title = element_text(
      family = "sans",
      size = 15,
      hjust = 0.5,
      color = "black"
    ),
    axis.ticks.y = element_line(lineend = "round"),
    axis.ticks.x = element_line(lineend = "round")
  )

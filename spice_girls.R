library(tidyverse)
options(scipen = 12345)

studio_album_tracks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-14/studio_album_tracks.csv')
artists <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-14/related_artists.csv")

glimpse(studio_album_tracks)


studio_album_tracks %>%
  count(album_name)

studio_album_tracks %>%
  group_by(album_name) %>%
  summarise(across(c("loudness",
                     "speechiness",
                     "instrumentalness",
                     "liveness",
                     "valence",
                     "tempo"), function(x) {
                       median(x)
                     }, .names = "median_{col}")) %>%
  pivot_longer(-album_name, names_to = "variable", values_to = "value") %>%
  mutate(color = ifelse(value < 0, 'red1', 'lightblue3')) %>%
  ggplot(aes(variable, value, fill = color)) +
  geom_col() +
  scale_fill_identity() + 
  geom_hline(yintercept = 0, color = "lightgrey") +
  facet_wrap(~album_name, scales = "free_x") +
  coord_flip() +
  labs(title = "Median characteristic by album.",
       y = NULL,
       x = NULL) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 19),
        strip.text = element_text(face = "bold", size = 11, colour = "grey"))


artists %>%
  distinct(popularity, followers_total) %>%
  ggplot(aes(popularity, followers_total, color = popularity)) +
  geom_point(size = 2.6) +
  geom_smooth(aes(group = 1), 
              method = "lm", 
              se = FALSE, 
              color = 'black') +
  theme_minimal() +
  labs(y = "followers #",
       title = "As Popularity Increases so is Followership.") +
  theme(panel.grid = element_blank(),
        plot.title = element_text(face = "bold", size = 19, hjust = 0.5),
        legend.position = 'none')
  
  











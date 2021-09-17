library(tidyverse)
library(lubridate)
library(tidymodels)
theme_set(theme_minimal())


billboard <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-14/billboard.csv')
audio <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-14/audio_features.csv')

glimpse(billboard)
glimpse(audio)

dfm <- billboard %>%
  left_join(audio, by = c("song_id", "performer", "song")) %>%
  mutate(week_id = lubridate::mdy(week_id),
         year = lubridate::year(week_id))



dfm %>%
  filter(week_position == 1) %>%
  count(song, performer, sort = TRUE) %>%
  top_n(15) %>%
  ggplot(aes(fct_reorder(performer, n), n, fill = performer)) +
  geom_col() +
  scale_fill_viridis_d() + 
  theme(legend.position = "None") +
  coord_flip()


billboard %>%
  group_by(song_id) %>%
  summarise(weeks_on_chart = max(weeks_on_chart), .groups = "drop") %>%
  arrange(desc(weeks_on_chart))

dfm %>%
  group_by(year) %>%
  summarise(tempo = median(tempo, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = tempo)) +
  geom_line() +
  labs(title = "Tempo throughout years") +
  ggeasy::easy_center_title()

df_tidy <- dfm %>%
  filter(year > 2000) %>%
  select(performer, week_id, spotify_genre, danceability:tempo,-mode) %>%
  mutate(
    genre = str_remove_all(spotify_genre, "\\['|'\\]"),
    # got to clean genre
    genre = replace_na(genre, "unknown")
  ) %>%
  select(genre)

rec <- recipe(~., data = df_tidy) %>%
  update_role(spotify_genre, performer, week_id, new_role = "id") %>%
  step_normalize(all_predictors()) %>%
  step_impute_median(all_predictors()) %>%
  step_pca(all_numeric_predictors(), num_comp = 4)


pca_prep <- prep(rec)

pca_tidied <- tidy(pca_prep, 3)

pca_tidied %>%
  count(component)

pca_tidied %>%
  filter(component %in% paste0("PC", 1:4)) %>%
  mutate(component = fct_inorder(component)) %>%
  ggplot(aes(value, terms, fill = terms)) +
  scale_fill_viridis_d() + 
  geom_col(show.legend = FALSE) +
  facet_wrap(~component, nrow = 1) + 
  labs(y = NULL)
  

pca_tidied %>%
  filter(component %in% paste0("PC", 1:4)) %>%
  group_by(component) %>%
  top_n(10, abs(value)) %>%
  ungroup() %>%
  mutate(terms = fct_reorder(terms, abs(value))) %>%
  ggplot(aes(abs(value), terms, fill = value > 0)) +
  geom_col() +
  facet_wrap(~component, scales = "free_y") +
 
  labs(
    x = "Absolute value of contribution",
    y = NULL, 
    fill = "Positive?"
  )

juice(pca_prep)



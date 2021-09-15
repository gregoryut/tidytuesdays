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
  group_by(year) %>%
  summarise(tempo = median(tempo, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = tempo)) +
  geom_line() +
  labs(title = "Tempo throughout years") +
  ggeasy::easy_center_title()

df_tidy <- dfm %>%
  filter(year > 2000) %>%
  select(performer, spotify_genre, danceability:tempo, -mode) #%>%
  mutate(genre = gsub("\\[|\\]|\'|\'", "", spotify_genre), # got to clean genre
         genre = replace_na(genre, "unknown"))

rec <- recipe(~., data = df_tidy) %>%
  update_role(spotify_genre, performer, new_role = "id") %>%
  step_normalize(all_predictors()) %>%
  step_impute_median(all_predictors()) %>%
  step_pca(all_predictors())


pca_prep <- prep(rec)

pca_tidied <- tidy(pca_prep, 3)


pca_tidied %>%
  filter(component %in% paste0("PC", 1:5)) %>%
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

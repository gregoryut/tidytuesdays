library(tidyverse)


measles <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-25/measles.csv'
  )
glimpse(measles)

# barplot of measles cases by school type
measles %>%
  count(type) %>%
  ggplot(aes(type, n, fill = type)) +
  geom_col() +
  labs(title = "Total Cases by School Type.") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 17),
    legend.position = 'none'
  )



grouped_tbl <- measles %>%
  group_by(state) %>%
  summarise(
    mean_mmr = mean(mmr, na.mr = TRUE),
    over_all_mean = mean(overall, na.rm = TRUE),
    xrel = mean(xrel, na.rm = TRUE),
    xmed = mean(xmed, na.rm = TRUE),
    xper = mean(xper, na.rm = TRUE)
  )

measles %>%
  filter(mmr != -1) %>%
  ggplot(aes(state, mmr, color = state)) +
  geom_boxplot(outlier.alpha = 0.5) +
  coord_flip() +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    legend.position = 'none'
  )




states <- tigris::states()


states %>%
  filter(
    !NAME %in% c(
      "Alaska",
      "Hawaii",
      "Commonwealth of the Northern Mariana Islands",
      "United States Virgin Islands",
      "Guam",
      "Puerto Rico",
      "American Samoa"
    )
  ) %>%
  left_join(grouped_tbl, by = c("NAME" = "state")) %>%
  mutate(mean_mmr = ifelse(mean_mmr == -1, NA_real_, mean_mmr)) %>%
  ggplot(aes(fill = mean_mmr)) +
  geom_sf() +
  theme_minimal() +
  labs(title = "Average MMR Rate.",
       fill = "MMR Rate") +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    plot.title = element_text(hjust = 0.5, face = 'bold', size = 19)
  )


hot_spots <- measles %>%
  filter(mmr < 50, mmr != -1)

states %>%
  filter(
    !NAME %in% c(
      "Alaska",
      "Hawaii",
      "Commonwealth of the Northern Mariana Islands",
      "United States Virgin Islands",
      "Guam",
      "Puerto Rico",
      "American Samoa"
    )
  ) %>%
  ggplot() +
  geom_sf(fill = "lightgrey", alpha = 0.4) +
  geom_point(data = hot_spots, aes(x = lng, y = lat, alpha = 0.6,
                                    color = state)) +
  theme_minimal() +
  labs(x = NULL,
       y = NULL,
       title = "Hot Spots (MMR < 50%).") + 
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = 'none',
        plot.title = element_text(hjust = 0.5, size = 17, face = 'bold'))



measles %>%
  filter(mmr != -1) %>%
  group_by(name, year) %>%
  summarise(mmr = mean(mmr)) %>%
  arrange(mmr) %>%
  filter(mmr < 15) %>%
  ggplot(aes(fct_reorder(name, mmr), mmr, fill = -mmr)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL,
       title = "Lowest MMR Schools.") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 16),
        legend.position = 'none')

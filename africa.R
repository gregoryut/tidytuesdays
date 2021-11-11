#remotes::install_github("afrimapr/afrilearndata")

library(afrilearndata)
library(tidyverse)
library(patchwork)


africapitals
afriairports


africa_pop <- afripop2020 %>%
  as.data.frame(xy = TRUE) %>%
  drop_na() %>%
  mutate(pop = ifelse(ppp_2020_1km_Aggregated < 500,
                      ppp_2020_1km_Aggregated, 600))



pop2020 <- ggplot() +
  geom_tile(data = africa_pop, aes(x, y, fill = pop)) +
  scale_fill_gradient2() +
  theme_minimal() +
  labs(fill = "Population") +
  geom_sf(
    data = africapitals,
    alpha = 0.2,
    color = "darkorange",
    aes(size = pop)
  ) +
  labs(x = NULL,
       y = NULL,
       size = "City Population",
       title = "Africa population 2020.") +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 15),
    plot.background = element_rect(fill = 'cadetblue2')
  )



ggplot(africa_pop, aes(x, y, fill = pop)) +
  geom_raster() +
  theme_minimal() +
  theme(panel.grid = element_blank())

pop2000 <- afripop2000 %>%
  as.data.frame(xy = TRUE) %>%
  drop_na() %>%
  mutate(pop = ifelse(ppp_2000_1km_Aggregated < 500,
                      ppp_2000_1km_Aggregated, 600)) %>%
  ggplot() +
  geom_tile(aes(x, y, fill = pop)) +
  scale_fill_gradient2() +
  theme_minimal() +
  labs(fill = "Population") +
  geom_sf(
    data = africapitals,
    alpha = 0.3,
    color = "darkorange",
    aes(size = pop)
  ) +
  #geom_text(data = africapitals, aes(x, y, label = capitalname)) + 
  labs(x = NULL,
       y = NULL,
       size = "City Population",
       title = "Africa Population 2000.") +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 15),
    plot.background = element_rect(fill = 'cadetblue2')
  )


pop2000 | pop2020


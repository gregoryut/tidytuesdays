#remotes::install_github("afrimapr/afrilearndata")
#devtools::install_github("yutannihilation/ggsflabel")
library(afrilearndata)
library(tidyverse)
library(patchwork)
library(ggsflabel)
library(scico)
options(scipen = 10000)

africa_pop <- afripop2020 %>%
  as.data.frame(xy = TRUE) %>%
  drop_na(ppp_2020_1km_Aggregated) %>%
  rename(pop = ppp_2020_1km_Aggregated)



pop2020 <- ggplot() +
  geom_tile(data = africa_pop, aes(x, y, fill = pop)) +
  scale_fill_scico(direction = -1, 
                   trans = "pseudo_log",
                   palette = "lajolla", 
                   breaks = c(0, 100, 1000, 10000, 20000)) +
  theme_minimal() +
  labs(fill = "Population") +
  geom_sf(
    data = africapitals,
    alpha = 0.5,
    color = "white",
    aes(size = (pop))
  ) +
  #geom_sf_label_repel(data = africapitals, aes(label = capitalname), 
  #                    color = "grey10", family = "prata", size = 2.5, nudge_x = -1.5,
  #                    segment.color = "transparent", segment.size = 1) + 
  labs(x = NULL,
       y = NULL,
       size = "City Population",
       title = "Africa population 2020.") +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 15),
    plot.background = element_rect(fill = 'cadetblue2')
  )


pop2020



pop2000 <- afripop2000 %>%
  as.data.frame(xy = TRUE) %>% 
  drop_na(ppp_2000_1km_Aggregated) %>%
  rename(pop = ppp_2000_1km_Aggregated) %>%
  ggplot() +
  geom_tile(aes(x, y, fill = pop)) +
  scale_fill_scico(direction = -1, 
                   trans = "pseudo_log",
                   palette = "lajolla", 
                   breaks = c(0, 100, 1000, 5000, 10000)) +
  theme_minimal() +
  labs(fill = "Population") +
  geom_sf(
    data = africapitals,
    alpha = 0.5,
    color = "white",
    aes(size = pop)
  ) +
  #geom_sf_label_repel(data = africapitals, aes(label = capitalname), 
  #                    color = "grey10", family = "prata", size = 2.5, nudge_x = -0.5,
  #                    segment.color = "white", segment.size = 0.5) + 
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


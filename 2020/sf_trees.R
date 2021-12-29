library(tidyverse)
library(tigris)
library(sf)

sf_trees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-28/sf_trees.csv')
glimpse(sf_trees)




tracts <- tracts(state = "CA", county = "San Francisco")


# function to erase water
st_erase <- function(x, y) {
  st_difference(x, st_union(y))
}

# get water data
sf_water <- area_water("CA", "San Francisco", class = "sf") 


# spatial join
## prepare data
trees <- sf_trees %>%
  filter(latitude < 46 & longitude < 125,
         longitude < 37.55) %>%
  drop_na(longitude, latitude) %>%
  st_as_sf(., coords = c("longitude", "latitude")) %>%
  st_set_crs(st_crs(tracts))

# join
sft <- tracts %>%
  st_join(trees, 
          join = st_contains_properly) %>%
  group_by(GEOID) %>%
  summarise(n_trees = n())

sft_erase <- st_erase(sft, sf_water)

# plot of trees
sft_erase %>%
  st_crop(st_bbox(trees)) %>%
  ggplot(aes(fill = n_trees)) +
  geom_sf(lwd = 0.1) +
  scale_fill_gradient2() + 
  theme_minimal() +
  labs(title = "Number Of Trees In Each Tract (San Francisco).",
       fill = "# of trees") +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())









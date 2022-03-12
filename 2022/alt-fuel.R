library(tidyverse)
library(sf)
library(tmap)
sf_use_s2(FALSE)


stations <- 
  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-01/stations.csv')

glimpse(stations)


cnts <- tigris::counties(cb = TRUE, year = 2019) %>%
  mutate(county = as.numeric(STATEFP)) %>%
  filter(county < 60,
         !county %in% c(15, 02))

# convert stations file to spatial format
stations_sp <- stations %>%
  st_as_sf(., coords = c("X", "Y"), crs = st_crs(cnts))

# spatial join points, sum all stations in each county
stations_cnts <- cnts %>%
  st_join(stations_sp,
          join = st_contains) %>%
  group_by(COUNTYFP) %>%
  summarise(count = n())

# plot
stations_cnts %>%
  ggplot(aes(fill = count)) +
  geom_sf(lwd = 0.1) +
  #scale_fill_viridis_c(option = "H") +
  scale_fill_gradient(low = "white", high = "blue") +
  coord_sf(crs = 5070) +
  labs(title = "Alternative Fuel Stations") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, face = 'bold', size = 18)) 


tm_shape(stations_cnts) +
  tm_fill(col = "count", 
          palette = "BuPu", 
          n = 6, 
          style = "jenks") +
  tm_borders(lwd = 0.2)


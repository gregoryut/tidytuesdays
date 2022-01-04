library(tidyverse)
library(sf)
library(raster)

wind_turbine <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-27/wind-turbine.csv')

glimpse(wind_turbine)


# top manufacturers
wind_turbine %>%
  count(manufacturer, sort = TRUE) %>%
  head(10) %>%
  ggplot(aes(fct_reorder(manufacturer, -n), n)) +
  geom_col() +
  theme_minimal() +
  labs(x = "Manufacturer")


provinces <- getData(country = "Canada", level = 1)

provinces <- st_as_sf(provinces)

provinces <- st_simplify(provinces, dTolerance = 10000)

ggplot(provinces) +
  geom_sf()

wind_sf <- wind_turbine %>%
  st_as_sf(., coords = c("longitude", "latitude"), 
           crs = st_crs(provinces))

provinces %>%
  ggplot() +
  geom_sf(alpha = 0.2) +
  geom_sf(data = wind_sf, color = "pink") +
  theme_void()

df_join <- provinces %>%
  st_join(wind_sf, 
          join = st_contains) %>%
  group_by(NAME_1) %>%
  summarise(wind_t = n())

df_join %>%
  ggplot(aes(fill = wind_t)) +
  geom_sf() +
  scale_fill_gradient2() +
  theme_void() +
  labs(title = "Number of Wind Turbines in Provinces.",
       fill = "Num. of Wind Turbines") +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 17))










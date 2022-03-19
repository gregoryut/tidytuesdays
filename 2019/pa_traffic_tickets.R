library(tidyverse)
library(sf)
library(tigris)
theme_set(theme_minimal())


tickets <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-03/tickets.csv")

glimpse(tickets)

pa <- tracts(state = "PA", cb = TRUE)
 
sf_tickets <- tickets %>%
  st_as_sf(., coords = c("lon", "lat"), crs = st_crs(pa)) %>%
  mutate(weekday = lubridate::wday(issue_datetime))

# spatial join
sp_tickets <- pa %>%
  st_join(sf_tickets,
          join = st_contains) %>%
  group_by(NAME) %>%
  summarise(tot_fine = sum(fine, na.rm = TRUE))


sp_tickets %>%
  ggplot(aes(fill = tot_fine)) +
  geom_sf(lwd = 0.04) +
  scale_fill_gradient2()

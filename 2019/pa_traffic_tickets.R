library(tidyverse)
library(sf)
library(tigris)
theme_set(theme_minimal())
sf_use_s2(FALSE)

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

pa <- sp_tickets %>%
  st_union()

ggplot() +
  geom_sf(data = sp_tickets, aes(fill = tot_fine), lwd = 0.04) +
  geom_sf(data = pa, alpha = 0) + 
  scale_fill_gradient2() +
  labs(title = glue::glue("Traffic Tickets Value USD From \n{max(sf_tickets$issue_datetime, na.rm = TRUE)} to  {min(sf_tickets$issue_datetime, na.rm = TRUE)}"),
       fill = "Total USD") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = 'bold'),
        panel.grid = element_blank())


sp_tickets %>% 
  st_crop(. , xmin = -75.3, xmax = -75, ymin = 39.6, ymax = 40.1) %>%
  ggplot() +
  geom_sf(aes(fill = tot_fine), lwd = 0.04) +
  scale_fill_gradientn(colors = c("#f1eef6", "#d4b9da", "#c994c7", "#df65b0", "#dd1c77", "#980043")) +
  labs(title = glue::glue("Traffic Tickets Value USD From \n{max(sf_tickets$issue_datetime, na.rm = TRUE)} to  {min(sf_tickets$issue_datetime, na.rm = TRUE)}"),
       fill = "Total USD")+
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = 'bold'),
        panel.grid = element_blank())

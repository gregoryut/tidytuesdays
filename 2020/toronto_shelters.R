library(tidyverse)
library(ggTimeSeries)

shelters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-01/shelters.csv')

glimpse(shelters)

max(shelters$occupancy_date)
min(shelters$occupancy_date)

shelters %>% count(facility_name, sort = TRUE)

shelters %>%
  filter(capacity != 0) %>%
  #mutate(capacity = ifelse(capacity == 0, 1, capacity)) %>%
  mutate(date = lubridate::ymd(occupancy_date),
         year = lubridate::year(date),
         month = lubridate::month(date),
         rate = occupancy / capacity) %>% 
  group_by(occupancy_date, year) %>%
  summarise(occ = mean(rate, na.rm = TRUE)) %>%
  ggplot_calendar_heatmap(cDateColumnName = 'occupancy_date',
                          cValueColumnName = 'occ',
                          dayBorderSize = .5, 
                          monthBorderSize = 1) +
  scale_fill_viridis_c(labels = scales::percent) +
  facet_wrap(~year, ncol = 1) +
  theme_minimal() +
  labs(title = "Heatmap of Shelter Occupancy Rate Across Time.",
       fill = "Occupancy Rate") +
  theme(panel.grid = element_blank(),
        legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5, size = 18, face = 'bold'),
        legend.key.width = unit(1, "cm"))


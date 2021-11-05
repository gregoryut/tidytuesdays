library(tidyverse)
library(sf)
library(tigris)
library(tmap)


cov_raw <- read_csv("https://raw.githubusercontent.com/nychealth/covid-vaccine-data/main/people/coverage-by-modzcta-allages.csv")
glimpse(cov_raw)

zcta_ny <- tigris::zctas("NY")
zcta_ny

vax_sp <- zcta_ny %>%
  inner_join(cov_raw %>% mutate(MODZCTA = as.character(MODZCTA)), 
             by = c("ZCTA5CE10" = "MODZCTA")) %>%
  mutate(vax_full = round((COUNT_FULLY_CUMULATIVE / POP_DENOMINATOR)*100, 2),
         vax_one = round((COUNT_1PLUS_CUMULATIVE / POP_DENOMINATOR)*100, 2),
         vax_one = case_when(vax_one < 40 ~ "40%",
                             vax_one < 50  ~ "50%",
                             vax_one < 60 ~ "60%",
                             vax_one < 70 ~ "70%",
                             vax_one < 80 ~ "80%",
                             TRUE ~ "99%"))

glimpse(vax_sp)

tm_shape(vax_sp) +
  tm_fill(col = "vax_one", 
          n = 5,
          style = "quantile", 
          legend.is.portrait = FALSE, 
          palette = "YlGnBu", 
          labels = c("40%", "50%", "60%", "70%", "80%", "90%", "99%"),
          interval.closure = "left",
          title = "NYC Residents With At Least 1 Dose") +
  tm_layout(legend.outside = TRUE,
            legend.outside.position = "top",
            legend.outside.size = 0.20) +
  tm_borders(lwd = 0.7, col = "lightgrey")








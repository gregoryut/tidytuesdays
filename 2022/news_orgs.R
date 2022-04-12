library(tidyverse)
library(gt)

news_orgs <-
  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-05/news_orgs.csv')

glimpse(news_orgs)

news_orgs %>%
  count(publication_name, sort = TRUE)

news_state_df <- news_orgs %>%
  group_by(state) %>%
  summarise(n = n())


states <- tigris::states(cb = TRUE, year = 2019)

states %>%
  left_join(news_state_df, by = c("STUSPS" = "state")) %>%
  filter(STUSPS %in% state.abb,
         !STUSPS %in% c("AK", "HI")) %>%
  ggplot(aes(fill = n)) +
  geom_sf(lwd = 0.1) +
  scale_fill_gradient2(low = "#4d3839", mid = "#4d3839", high = "#b533ff") +
  theme_void()

news_orgs %>%
  count(publication_name, budget_percent_administration, budget_percent_editorial, budget_percent_product_technology, budget_percent_revenue_generation) %>%
  arrange(desc(n)) %>%
  drop_na() %>%
  gt()




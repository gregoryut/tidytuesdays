library(tidyverse)

pumpkins <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-19/pumpkins.csv'
  )

glimpse(pumpkins)



# top 100 competitors and their weights by year
top100 <- pumpkins %>%
  filter(!place %in% c("EXH", "DMG")) %>%
  separate(id, into = c("year", "type"), sep = "-") %>%
  filter(!type %in% c("T", "L", "W")) %>%
  drop_na(place) %>%
  mutate(across(c(weight_lbs, place), parse_number)) %>%
  filter(place <= 100)

top100 %>%
  ggplot(aes(year, weight_lbs, color = type)) +
  geom_jitter() +
  theme_minimal() +
  labs(title = "Top 100 pumpkin competitors.",
       y = "Weight in Pounds",
       x = "Year") +
  theme(plot.title = element_text(
    hjust = 0.5,
    family = "sans",
    size = 17
  ),
  panel.grid = element_blank())




## Where most US competitors are from?
clean_pump <- pumpkins %>%
  filter(!place %in% c("EXH", "DMG")) %>%
  separate(id, into = c("year", "type"), sep = "-") %>%
  filter(!type %in% c("T", "L", "W")) %>%
  drop_na(place) %>%
  mutate(weight_lbs = parse_number(weight_lbs),
         place = parse_number(place)) %>%
  filter(country == "United States")

usa <- tigris::states(cb = TRUE)

usa %>%
  filter(NAME %in% clean_pump$state_prov) %>%
  filter(NAME != "Alaska") %>%
  left_join(
    clean_pump %>% filter(country == "United States") %>%
      count(state_prov, sort = TRUE),
    by = c("NAME" = "state_prov")
  ) %>%
  ggplot(aes(fill = n)) +
  geom_sf() +
  scale_fill_gradient2() +
  theme_void() +
  labs(title = "Where Do Most Competitors Come From?")


### WIP
clean_pump2 <- pumpkins %>%
  filter(!place %in% c("EXH", "DMG")) %>%
  separate(id, into = c("year", "type"), sep = "-") %>%
  filter(!type %in% c("T", "L", "W")) %>%
  drop_na(place) %>%
  mutate(weight_lbs = parse_number(weight_lbs),
         place = parse_number(place))


# still thinking ...
clean_pump2 %>%
  count(country, place, sort = TRUE) %>%
  filter(place %in% 1:5) %>%
  ggplot(aes(fct_reorder(country, n), n)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() + 
  facet_wrap(~place) +
  theme_minimal()


pumpkins %>%
  filter(!place %in% c("EXH", "DMG")) %>%
  separate(id, into = c("year", "type"), sep = "-") %>%
  filter(type == "P") %>%
  mutate(country = fct_lump(country, 10)) %>%
  mutate(across(c(weight_lbs, place), parse_number)) %>%
  ggplot(aes(fct_reorder(country, weight_lbs), weight_lbs, color = country)) +
  geom_boxplot(outlier.color = NA) +
  geom_jitter(alpha = 0.1, width = 0.15) + 
  theme_minimal() +
  labs(x = NULL, 
       y = "Weight (Pounds)") +
  theme(legend.position = "None")


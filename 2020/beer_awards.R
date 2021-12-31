library(tidyverse)

beer_awards <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-20/beer_awards.csv')

head(beer_awards)


beer_awards %>%
  count(state, sort = TRUE) %>%
  head(20) %>%
  mutate(ny = ifelse(state == "NY", "salmon1", "grey")) %>%
  ggplot(aes(fct_reorder(state, n), n, fill = ny)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  scale_fill_identity() +
  labs(x = "State",
       y = "Medals",
       title = "States with Most Models Won.",
       subtitle = "NY is v low :(") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = 'bold'),
        panel.grid = element_blank(),
        plot.subtitle = element_text(hjust = 0.4, face = 'bold')) 
  

beer_awards %>%
  count(year) %>%
  ggplot(aes(year, n)) +
  geom_line() +
  theme_minimal()

beer_awards %>%
  filter(state %in% c("NY", "CA", "CO", "OR", "TX", "PA", "WI", "IL", "VA", "WA")) %>%
  count(year, state) %>%
  ggplot(aes(year, n, color = state, group = state)) +
  geom_line() +
  theme_minimal() +
  labs(x = NULL,
       y = "Medals",
       title = "Total Medals Won by State") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = 'bold'))


beer_awards %>%
  count(state, medal) %>%
  filter(state %in% c("NY", "CA", "CO", "OR", "TX", "PA", "WI", "IL", "VA", "WA")) %>%
  ggplot(aes(fct_reorder(state, -n), n, fill = medal)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("tan3", "gold2", "grey87")) +
  theme_minimal() +
  labs(title = "Medal Counts by State",
       fill = "Medal",
       x = NULL,
       y = NULL) +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 17, face = 'bold')) 
           

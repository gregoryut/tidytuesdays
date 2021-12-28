library(tidyverse)
library(lubridate)

big_mac <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-22/big-mac.csv')
glimpse(big_mac)

max(big_mac$date)

map_chr(big_mac, function(x) {sum(is.na(x))})


big_mac %>%
  select(dollar_price) %>%
  mutate(percent = ntile(dollar_price, 100)) %>%
  ggplot(aes(dollar_price, percent)) +
  geom_step()



big_mac %>%
  mutate(date = ymd(date),
         month = month(date),
         year = year(date)) %>%
  group_by(year, name) %>%
  summarize(adj_price = median(adj_price, na.rm = TRUE)) %>%
  filter(name %in% c("United States", "Japan", "Russia", 
                     "Canada", "China", "Switzerland"),
         year > 2010) %>%
  ggplot(aes(year, adj_price, color = name)) +
  geom_line(size = 1.5) +
  facet_wrap(~name, scales = "free") +
  theme_minimal() +
  expand_limits(y = c(2, 6)) + 
  labs(title = "Big Mac Price $$$",
       subtitle = "GDP adjusted price",
       x = "Year",
       y = "Adjusted Price $") +
  scale_x_continuous(breaks = seq(2010, 2020, 2)) +
  theme(axis.text.y = element_text(face = "bold", size = 12),
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        panel.grid = element_blank())

  
big_mac %>%
  mutate(date = ymd(date),
         month = month(date),
         year = year(date)) %>%
  filter(name %in% "United States",
         year > 2015) %>%
  ggplot(aes(year, local_price)) +
  geom_line(color = "#AD3AED") +
  theme_minimal() + 
  labs(title = "Local Price of Big Mac in United States",
       x = NULL,
       y = "$$$") +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.text.y = element_text(face = "bold", size = 12),
        axis.text.x = element_text(face = "bold", size = 12))


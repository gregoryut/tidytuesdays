library(tidyverse)
library(kableExtra)

chocolate <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv')

glimpse(chocolate)

chocolate %>%
  group_by(review_date, cocoa_percent) %>%
  summarise(mean_rate = mean(rating)) %>%
  arrange(desc(mean_rate))  %>%
  ggplot(aes(review_date, mean_rate, group = cocoa_percent, color = cocoa_percent)) +
  geom_line()


chocolate %>%
  group_by(review_date) %>%
  summarise(mean_rate = mean(rating)) %>%
  ggplot(aes(review_date, mean_rate)) +
  geom_line() +
  theme_minimal()


chocolate %>% 
  group_by(company_location, review_date) %>%
  summarise(mean_rating = mean(rating, na.rm = TRUE)) %>%
  filter(str_detect(company_location, "U.S.A.|Canada|Brazil|Belgium"),
         !review_date %in% 2006:2008) %>%
  ggplot(aes(as.character(review_date), mean_rating, group = company_location, color = company_location)) +
  geom_line() +
  labs(title = "mmm 2010") +
  theme_minimal()


chocolate %>%
  separate(most_memorable_characteristics, into = c("taste1", "taste2", "taste3"), sep = ",", extra = "drop") %>% 
  group_by(company_location, taste1) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  filter(company_location %in% c("Chile", "U.A.E.", "Poland", "Vietnam", 
                                 "Australia", "Guatemala", "Norway", "U.S.A.",
                                 "Switzerland", "Canada"),
         taste1 %in% c("creamy","roasty")) %>%
  ggplot(aes(company_location, n, fill = taste1)) +
  geom_bar(stat = "identity", position = "dodge")
  

chocolate %>%
  filter(company_location == "U.S.A.") %>%
  group_by(company_manufacturer, review_date) %>%
  summarise(n = n(),
            mean_rate = mean(rating)) %>%
  arrange(desc(mean_rate)) %>%
  head(40) %>%
  kbl(caption = "U.S.A. Manufacturers With Highest Ratings") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

# does the count chocolates relate to rating
chocolate %>%
  group_by(company_manufacturer, review_date) %>%
  summarise(n = n(),
            mean_rate = mean(rating)) %>%
  ggplot(aes(as.character(n), mean_rate, color = as.character(n))) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Count of Chocolate types",
       y = "Mean Rating",
       title = "Count of Chocolate and Average Rating.") +
  theme(legend.position = 'none') +
  ggeasy::easy_center_title()

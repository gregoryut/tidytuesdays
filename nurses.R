library(tidyverse)
library(janitor)
theme_set(theme_minimal())

# read in the data 
nurses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-05/nurses.csv')

nurses <- nurses %>%
  clean_names()

glimpse(nurses)

nurses %>%
  group_by(year) %>%
  summarise(tot_emp = sum(total_employed_rn)) %>%
  filter(!is.na(tot_emp)) %>%
  ggplot(aes(year, tot_emp)) +
  geom_point(alpha = 0.3) +
  geom_line(color = "midnightblue") +
  scale_x_continuous(breaks = seq(2000, 2020, 2)) +
  labs(title = "How many Nurses are emplyed over the years?",
       x = "Year",
       y = "Total Employed") +
  theme(plot.title = element_text(hjust = .5))


nurses %>%
  group_by(year) %>%
  summarise(med_wage = mean(hourly_wage_median)) %>%
  filter(!is.na(med_wage)) %>%
  ggplot(aes(year, med_wage)) +
  geom_point(alpha = 0.3) +
  geom_line(color = "midnightblue") +
  scale_x_continuous(breaks = seq(2000, 2020, 2)) +
  scale_y_continuous(labels = scales::dollar) + 
  labs(title = "Median hourly salary of Nurses over the years?",
       x = "Year",
       y = "Median Hourly Salary") +
  theme(plot.title = element_text(hjust = .5)) 

nurses %>%
  filter(state %in% c("New York", "California", "Texas", "Georgia", "Florida")) %>%
  ggplot(aes(year, total_employed_rn, color = state)) +
  geom_line() +
  expand_limits(y = 0) +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(x = "Year",
       y = "# nurses employed")

nurses %>%
  filter(state %in% c("New York", "California", "Texas", "Georgia", "Florida")) %>%
  ggplot(aes(year, hourly_wage_median, color = state)) +
  geom_line() +
  expand_limits(y = 0) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(x = "Year",
       y = "Median Wage")


nurses %>%
  filter(state %in% c("New York", "California", "Texas", "Georgia", "Florida")) %>%
  ggplot(aes(year, annual_salary_median, color = state)) +
  geom_line() +
  expand_limits(y = 0) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(x = "Year",
       y = "Median Salary")


# make a map 
nurses %>%
  filter(year == 2020) %>%
  mutate(state = str_to_lower(state)) %>%
  inner_join(map_data("state"), by = c(state = "region")) %>%
  ggplot(aes(long, lat, group = annual_salary_median, fill = annual_salary_median)) +
  geom_polygon() +
  coord_map() + 
  scale_fill_continuous(labels = scales::dollar_format(), type = "viridis") +
  labs(title = "Median Annual Salary of Registered Nurses USA",
       fill = "Annual Salary",
       subtitle = "Salary in 2020") +
  theme_void() + 
  theme(plot.title = element_text(hjust = 0.5))
  
  



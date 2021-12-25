library(tidyverse)
library(janitor)
theme_set(theme_minimal())


# read in the data
nurses <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-05/nurses.csv'
  )

nurses <- nurses %>%
  clean_names()

glimpse(nurses)

nurses %>%
  filter(state %in% c("New York", "California", "Texas", "Georgia", "Florida")) %>%
  group_by(year, state) %>%
  summarise(tot_emp = sum(total_employed_rn), .groups = "drop") %>%
  filter(!is.na(tot_emp)) %>%
  ggplot(aes(year, tot_emp, color = state, group = state)) +
  geom_point(alpha = 0.3) +
  geom_line() +
  scale_x_continuous(breaks = seq(2000, 2020, 2)) +
  scale_y_continuous(labels = scales::comma) +
  geom_vline(xintercept = 2008,
             color = "grey",
             alpha = 0.5) +
  geom_text(
    aes(x = 2008, label = "Global Financial Crisis", y = 300000),
    colour = "grey",
    angle = 1,
    size = 4
  ) +
  labs(
    title = "How many Nurses are emplyed over the years?",
    x = "Year",
    y = "Total Employed",
    subtitle = "vertical line is financial crisis."
  ) +
  theme(plot.title = element_text(hjust = .5))


nurses %>%
  filter(state %in% c("New York", "California", "Texas", "Georgia", "Florida")) %>%
  group_by(year, state) %>%
  summarise(med_wage = mean(hourly_wage_median), .groups = "drop") %>%
  filter(!is.na(med_wage)) %>%
  ggplot(aes(year, med_wage, color = state, group = state)) +
  geom_point(alpha = 0.3) +
  geom_line() +
  scale_x_continuous(breaks = seq(2000, 2020, 2)) +
  scale_y_continuous(labels = scales::dollar) +
  geom_vline(xintercept = 2008,
             color = "grey",
             alpha = 0.5) + 
  geom_text(
    aes(x = 2008, label = "Global Financial Crisis", y = 50),
    colour = "grey",
    angle = 1,
    size = 4
  ) +
  labs(title = "Median hourly salary of Nurses over the years?",
       x = "Year",
       y = "Median Hourly Salary") +
  theme(plot.title = element_text(hjust = .5))



# not sure if im doing the error ribbon right...
nurses %>%
  mutate(std = wage_salary_standard_error_percent / 100) %>%
  filter(state %in% c("New York", "California", "Texas", "Georgia", "Florida")) %>%
  ggplot(aes(year, annual_salary_median, color = state)) +
  geom_line() +
  #geom_ribbon(aes(y = annual_salary_median, ymin = annual_salary_median - (annual_salary_median * std) , ymax = annual_salary_median + (annual_salary_median * std), fill = state), alpha = .2) +
  expand_limits(y = 0) +
  scale_y_continuous(labels = scales::dollar_format()) +
  geom_vline(xintercept = 2008,
             color = "grey",
             alpha = 0.5) + 
  geom_text(
    aes(x = 2008, label = "Global Financial Crisis", y = 100000),
    colour = "grey",
    angle = 1,
    size = 4
  )
  labs(x = "Year",
       y = "Median Salary")


# make a map
nurses %>%
  filter(year == 2020) %>%
  mutate(state = str_to_lower(state)) %>%
  inner_join(map_data("state"), by = c("state" = "region")) %>%
  ggplot(aes(long, lat, group = annual_salary_median, fill = annual_salary_median)) +
  geom_polygon() +
  coord_map() +
  scale_fill_continuous(labels = scales::dollar_format(), type = "viridis") +
  labs(title = "Median Annual Salary of Registered Nurses USA",
       fill = "Annual Salary",
       subtitle = "Salary in 2020") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))



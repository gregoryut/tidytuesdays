library(tidyverse)

freedom <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-22/freedom.csv')

glimpse(freedom)


freedom %>%
  filter(country %in% c("Afghanistan",
                        "Poland",
                        "Ukraine", 
                        "Bulgaria",
                        "Belarus",
                        "Russian Federation")) %>%
  pivot_longer(c(CL, PR)) %>%
  mutate(name = case_when(name %in% "CL" ~ "Civil Liberites",
                          name %in% "PR" ~ "Political Rights",
                          TRUE ~ "ERROR")) %>%
  ggplot(aes(year, value, color = country)) +
  geom_line(size = 1.2) +
  facet_wrap(country~name, scales = "free") +
  scale_color_manual(values = c("#3900b3",
                                "#714dbf",
                                "#9e6b90",
                                "#b17777", 
                                "#cf9270",
                                "#ebb698")) +
  theme_minimal() +
  labs(title = "Political Rights & Civil Liberites in Picked Countries.") +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        legend.position = 'none')


freedom %>%
  filter(country %in% c("Afghanistan",
                        "Poland",
                        "Ukraine", 
                        "Bulgaria",
                        "Belarus",
                        "Russian Federation")) %>%
  pivot_longer(c(CL, PR)) %>%
  mutate(name = case_when(name %in% "CL" ~ "Civil Liberites",
                          name %in% "PR" ~ "Political Rights",
                          TRUE ~ "ERROR")) %>%
  group_by(country, name) %>%
  summarise(mean_value = mean(value, na.rm = TRUE)) %>%
  ggplot(aes(country, mean_value, fill = name)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#0040ff", "#00ffff")) +
  theme_minimal() +
  labs(title = "Average Political Rights & Civil Liberites by Countries.") +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        legend.position = "top")




freedom %>%
  filter(country %in% c("Afghanistan",
                        "Poland",
                        "Ukraine", 
                        "Bulgaria",
                        "Belarus",
                        "Russian Federation")) %>%
  pivot_longer(c(CL, PR)) %>%
  mutate(name = case_when(name %in% "CL" ~ "Civil Liberites",
                          name %in% "PR" ~ "Political Rights",
                          TRUE ~ "ERROR")) %>%
  ggplot(aes(country, value, color = name)) +
  geom_boxplot() +
  scale_color_manual(values = c("#23ccff", "#ffc800")) +
  theme_minimal() +
  labs(title = "Dist. of Political Rights & Civil Liberites Values by Country.",
       x = NULL) +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        legend.position = "top")


freedom %>%
  filter(country %in% c("Afghanistan",
                        "Poland",
                        "Ukraine", 
                        "Bulgaria",
                        "Belarus",
                        "Russian Federation")) %>%
  count(country, Status) %>%
  mutate(Status = case_when(Status %in% c("F") ~ "Free",
                            Status %in% c("NF") ~ "Not Free",
                            Status %in% c("PF") ~ "Partially Free")) %>%
  ggplot(aes(country, n, fill = Status)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#f36f20", "#ffd8bf", "#8e499b")) +
  theme_minimal() +
  labs(title = "Historic Count of Statuses by Country",
       x = NULL) +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        legend.position = "top")



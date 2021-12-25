library(tidyverse)


# get data ----------------------------------------------------------------

farmed <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/aquaculture-farmed-fish-production.csv'
  )
captured_vs_farmed <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/capture-fisheries-vs-aquaculture.csv'
  )
captured <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/capture-fishery-production.csv'
  )
consumption <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/fish-and-seafood-consumption-per-capita.csv'
  )
stock <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/fish-stocks-within-sustainable-levels.csv'
  )
fishery <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/global-fishery-catch-by-sector.csv'
  )
production <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/seafood-and-fish-production-thousand-tonnes.csv'
  )

glimpse(farmed)
glimpse(captured_vs_farmed)
glimpse(captured)
glimpse(consumption)
glimpse(stock)
glimpse(fishery)
glimpse(production)


# fish stock sus
stock %>%
  janitor::clean_names() %>%
  filter(entity == "World") %>%
  pivot_longer(starts_with("share_of")) %>%
  mutate(
    value2 = ifelse(
      name == "share_of_fish_stocks_within_biologically_sustainable_levels_fao_2020",-1 * value,
      value
    )
  ) %>%
  ggplot(aes(x = year, y = value2 / 100, fill = name)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(
    limits = c(-1, .50),
    expand = c(0.1, 0.1),
    breaks = seq(-1, 0.50, .25),
    labels = c("100%", "75%", "50%", "25%", "0%", "25%", "50%")
  ) +
  scale_x_continuous(breaks = c(1974, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2017)) +
  scale_alpha_identity() +
  scale_fill_manual(values = c("salmon", "midnightblue")) +
  geom_hline(yintercept = 0, color = "grey") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(
          hjust = .5,
          size = 18,
          family = "sans"
        )) +
  labs(
    x = "Year",
    y = "Percent Overexploited/Sustainable",
    title = "History of Seafood Stock that is Sustainable or NOT!",
    caption = "Tidytuesday week 42. \n https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-10-12/readme.md"
  ) +
  annotate(
    geom = "text",
    x = 1990,
    y = -.95,
    label = "Sustainable Share",
    color = "blue2",
    size = 6,
    alpha = .8,
    family = "Sans",
    hjust = 0
  ) +
  annotate(
    geom = "text",
    x = 1990,
    y = .5,
    label = "Overexplioted Share",
    color = "red2",
    size = 6,
    alpha = .8,
    family = "Sans",
    hjust = 0
  )



# Consumption by country plot
consumption %>%
  janitor::clean_names() %>%
  filter(
    entity %in% c(
      "United States",
      "France",
      "Spain",
      "Greece",
      "Russia",
      "Germany",
      "Australia",
      "China",
      "India"
    )
  ) %>%
  ggplot(
    aes(
      year,
      fish_seafood_food_supply_quantity_kg_capita_yr_fao_2020,
      color = entity,
      group = entity
    )
  ) +
  geom_line() +
  theme_minimal() +
  scale_y_continuous(
    breaks = seq(10, 50, 10),
    labels = c("10 kg", "20 kg", "30 kg", "40 kg", "50 kg"),
    expand = c(0.1, 0.1)
  ) +
  labs(
    x = "Year",
    y = NULL,
    title = "Seafood Consumption By Country.",
    color = "Country",
    subtitle = "Food supply in seafood in kg/capita/year."
  ) +
  theme(
    plot.title = element_text(
      family = "Sans",
      size = 16,
      hjust = 0.5
    ),
    legend.key.size = unit(1, 'cm')
  )



# farmed plot
farmed %>%
  janitor::clean_names() %>%
  filter(
    entity %in% c(
      "United States",
      "France",
      "Spain",
      "Greece",
      "Japan",
      "Germany",
      "Australia",
      "China",
      "India"
    )
  ) %>%
  ggplot(aes(
    year,
    aquaculture_production_metric_tons,
    color = entity,
    group = entity
  )) +
  geom_line() +
  scale_y_continuous(labels = scales::comma,
                     expand = c(0.1, 0.1)) +
  theme_minimal() +
  labs(
    y = NULL,
    x = "Year",
    title = "Aquaculture production (metric tons)",
    color = "Country",
    subtitle = "OMG China and India!"
  ) +
  theme(plot.title = element_text(hjust = .5))


# V2 without China
farmed %>%
  janitor::clean_names() %>%
  filter(
    entity %in% c(
      "United States",
      "France",
      "Spain",
      "Greece",
      "Japan",
      "Germany",
      "Australia"
    )
  ) %>%
  ggplot(aes(
    year,
    aquaculture_production_metric_tons,
    color = entity,
    group = entity
  )) +
  geom_line() +
  scale_y_continuous(labels = scales::comma,
                     expand = c(0.1, 0.1)) +
  theme_minimal() +
  labs(
    y = NULL,
    x = "Year",
    title = "Aquaculture production (metric tons)",
    color = "Country",
    subtitle = "Without China and India."
  ) +
  theme(plot.title = element_text(hjust = .5))


# us captured vs farmed
captured_vs_farmed %>%
  janitor::clean_names() %>%
  filter(entity %in% c(
    "United States",
    "Spain",
    "Japan",
    "China",
    "India",
    "United Kingdom"
  )) %>%
  pivot_longer(aquaculture_production_metric_tons:capture_fisheries_production_metric_tons) %>%
  ggplot(aes(year, value, color = name, group = name)) +
  geom_line() +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap( ~ entity, scales = "free") +
  labs(
    color = "Type",
    x = "Year",
    y = "Tons",
    title = "Production vs. Capture Aquaculture by Country."
  ) +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, size = 16),
    panel.grid = element_blank(),
    strip.text = element_text(size = 13)
  )




# fisheries
fishery %>%
  janitor::clean_names() %>%
  pivot_longer(artisanal_small_scale_commercial:subsistence) %>%
  ggplot(aes(year, value, color = name, group = name)) +
  geom_line() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Fishery Catches by Sector.",
    x = "Year",
    y = "Tons?",
    subtitle = "Looks like small scale commercial fishing is picking up."
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = .5, size = 16))


# production
production %>%
  janitor::clean_names() %>%
  filter(entity %in% c("United States", "Spain", "Italy", "China", "India", "Japan")) %>%
  pivot_longer(
    commodity_balances_livestock_and_fish_primary_equivalent_cephalopods_2766_production_5510_tonnes:commodity_balances_livestock_and_fish_primary_equivalent_marine_fish_other_2764_production_5510_tonnes
  ) %>%
  mutate(
    name = case_when(
      name == "commodity_balances_livestock_and_fish_primary_equivalent_cephalopods_2766_production_5510_tonnes" ~ "cephalopods",
      name == "commodity_balances_livestock_and_fish_primary_equivalent_demersal_fish_2762_production_5510_tonnes" ~ "demersal fish",
      name == "commodity_balances_livestock_and_fish_primary_equivalent_freshwater_fish_2761_production_5510_tonnes" ~ "freshwater fish",
      name == "commodity_balances_livestock_and_fish_primary_equivalent_marine_fish_other_2764_production_5510_tonnes" ~ "marine fish / other",
      name == "commodity_balances_livestock_and_fish_primary_equivalent_molluscs_other_2767_production_5510_tonnes" ~ "molluscs other"
    )
  ) %>%
  ggplot(aes(year, value, color = entity)) +
  geom_line() +
  facet_wrap( ~ name, scales = "free") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    x = "Year",
    y = "Tons",
    title = "Types of Fish Produced by Country",
    subtitle = "Looks like China is producing everything."
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5))

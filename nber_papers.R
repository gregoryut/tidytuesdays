library(tidyverse)
library(gghighlight)
library(kableExtra)
theme_set(theme_minimal())


papers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/papers.csv')
authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/authors.csv')
programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/programs.csv')
paper_authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_authors.csv')
paper_programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_programs.csv')

glimpse(papers)
glimpse(authors)
glimpse(programs)
glimpse(paper_authors)
glimpse(paper_programs)

full_df <- papers %>%
  left_join(paper_authors) %>% 
  left_join(authors) %>%
  left_join(paper_programs) %>%
  left_join(programs) %>%
  mutate(
    catalogue_group = str_sub(paper, 1, 1),
    catalogue_group = case_when(
      catalogue_group == "h" ~ "Historical",
      catalogue_group == "t" ~ "Technical",
      catalogue_group == "w" ~ "General"
    ),
    program = case_when(program == "LS" ~ "Labor Studies (LS)",
                        program == "PE" ~ "Public Economics (PE)",
                        program == "EFG" ~ "Economic Fluctuations & Growth (EFG)",
                        program == "IFM" ~ "International Finance and Macroeconomics (IFM)",
                        program == "ITI" ~ "International Trade and Investment (ITI)",
                        program == "ME" ~ "Monetary Economics (ME)",
                        program == "AP" ~ "Asset Pricing (AP)",
                        program == "HE" ~ "Health Economics (HE)",
                        program == "CF" ~ "Corporate Finace (CF)",
                        program == "PR" ~ "Productivity, Innovation, and Entrepreneurship (PR)",
                        program == "HC" ~ "Health Care (HC)",
                        program == "CH" ~ "Children (CH)", 
                        program == "IO" ~ "Industrial Organization (IO)",
                        program == "ED" ~ "Economics of Education (ED)",
                        program == "AG" ~ "Economics of Aging (AG)", 
                        program == "DEV" ~ "Development Economics (DEV)", 
                        program == "POL" ~ "Political Economics (POL)",
                        program == "DAE" ~ "Development of the American Economy (DAE)",
                        program == "EEE" ~ "Environment and Energy Economics (EEE)",
                        program == "LE" ~ "Law and Economics (LE)",
                        program == "TWP" ~ "Technical Working Papers (TWP)",
                        is.na(program) ~ "None",
                        TRUE ~ program),
    decade = round(year / 10) * 10,
    .after = paper
  )

full_df %>%
  count(year, program, sort = TRUE) %>%
  filter(year != 2021) %>%
  mutate(program = fct_lump(program, 15)) %>%
  ggplot(aes(fct_reorder(program, n), n, fill = program)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL,
       y = NULL) + 
  theme(legend.position = "none")




mydf <- full_df %>%
  filter(decade != 1970)

my_tbl <- round(prop.table(table(mydf$program, mydf$decade), 1) * 100)
my_tbl

my_tbl %>%
  kbl() %>%
  kable_paper("hover", full_width = F)


full_df %>%
  filter(year != 2021) %>%
  filter(!is.na(program_category)) %>%
  count(year, program_category, sort = TRUE) %>%
  ggplot(aes(year, n, color = program_category)) +
  geom_line() +
  scale_x_continuous(breaks = seq(1973, 2020, 2)) +
  labs(subtitle = "2020-2021 saw a year of spike in papers!",
       title = "NBER papers historical counts.") +
  theme(plot.title = element_text(family = "sans", face = "bold", hjust = 0.5),
        legend.position = "bottom")



full_df %>%
  filter(year != 2021) %>%
  count(year, program, sort = TRUE) %>%
  ggplot(aes(year, n, color = program)) +
  geom_line() +
  gghighlight(n > 400,
              label_params = list(size = 4.5, max.overlaps = 20),
              unhighlighted_params = list(size = 0.5, alpha = 0.5)) +
  scale_x_continuous(breaks = seq(1973, 2020, 2))










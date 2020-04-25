## Packages and Themes
library(tidyverse)
library(naniar)

theme_clean <- theme_bw(base_family="Calibri") + 
  theme(legend.position = "top",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b=12)),
        plot.caption = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())
theme_set(theme_clean)

## Import data
covid_df <- read_csv(file = "https://covid.ourworldindata.org/data/ecdc/full_data.csv")

## Tidy the data
covid_tidy_df <- covid_df %>%
  group_by(location) %>%
  mutate(death_doub_7days = 7*log(2)/log(total_deaths/lag(total_deaths,7))) %>%
  replace_with_na(replace = list(death_doub_7days = c("NaN", 0, "Inf"))) %>%
  mutate(td_exceeds_100 = as.numeric(total_deaths >= 100),
         td_date = case_when(td_exceeds_100 == 0 ~ as.Date(NA),
                             td_exceeds_100 == cumsum(td_exceeds_100) ~ date,
                             td_exceeds_100 < cumsum(td_exceeds_100) ~ as.Date(NA))) %>%
  fill(td_date) %>%
  mutate(days_since_100_deaths = as.numeric(difftime(date, td_date, units = "days")))

covid_select_df <- covid_tidy_df %>%
  filter(location %in% c("United Kingdom", "Italy", "Spain", "Germany", "United States"),
         days_since_100_deaths >= 0)

## Make graphs
covid_select_gg <- covid_select_df %>%
  filter(location %in% c("United Kingdom", "Italy", "Spain", "Germany", "United States")) %>%
  ggplot(aes(x = days_since_100_deaths,
             y = death_doub_7days,
             group = location)) +
  geom_line(aes(color = location), size = 1.5) +
  labs(title = "Italy's doubling time for COVID-19 confirmed deaths has increased to over three weeks",
       subtitle = "Doubling time in days (calculated over seven days), by days since 100 confirmed COVID-19 deaths. There may be different reporting practices in each country.",
       caption = "Data: Author's calculations based on European Centre for Disease Prevention and Control (Our World in Data CSV).",
       x = "Days since 100 recorded COVID-19 deaths (Day 0 is the day the total reached 100 deaths)",
       y = "Doubling Time (calculated over seven days)")
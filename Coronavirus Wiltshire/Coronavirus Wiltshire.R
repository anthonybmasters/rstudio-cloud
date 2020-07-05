## Packages and themes
library(tidyverse)
library(readxl)
library(janitor)

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

## Import and tidy data
coronavirus_cases_df <- read_csv(file = "https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv") %>%
  clean_names() %>%
  select("area_name", "area_code", "area_type", "specimen_date",
         "daily_lab_confirmed_cases", "cumulative_lab_confirmed_cases",
         "cumulative_lab_confirmed_cases_rate")

wiltshire_cases_df <- coronavirus_cases_df %>%
  filter(area_name == "Wiltshire",
         area_type == "Upper tier local authority")

## Make the graph
wiltshire_cases_gg <- wiltshire_cases_df %>%
  ggplot(aes(x = specimen_date,
             y = daily_lab_confirmed_cases)) +
  geom_col(fill = "#0b4838") +
  scale_x_date(date_breaks = "2 weeks",
               date_labels = "%d-%b-%Y",
               expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(title = "Confirmed COVID-19 cases in Wiltshire peaked on 11th May.",
       subtitle = "Lab-confirmed cases from SARS-CoV-2 testing (pillars 1 and 2) by specimen date. The provisional figures are for Wiltshire (UTLA), and subject to revision.",
       caption = paste("Data: Public Health England Coronavirus Data Dashboard. ",
                       "Latest reporting date: ",
                       max(wiltshire_cases_df$specimen_date)),
       x = "Specimen date",
       y = "")
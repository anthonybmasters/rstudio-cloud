## Packages and Themes
library(tidyverse)
library(readxl)

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
week_number_min <- 27

phe_figure4table_df <- read_excel("Coronavirus Wiltshire/PHE National COVID-19 Surveillance Data Report - Figure 4 Table - 2020-09-25.xlsx", 
                                        sheet = "DATA",
                                        col_types = c("numeric", "date", "date",
                                                      "numeric", "numeric", "numeric",
                                                      "numeric", "numeric", "numeric",
                                                      "numeric", "numeric", "numeric", "numeric"))

phe_caserates_agegroup_df <- phe_figure4table_df %>%
  pivot_longer(cols = 4:13,
               names_to = "age_group",
               values_to = "case_rate") %>%
  filter(week_number >= week_number_min)

week_end_breaks <- phe_figure4table_df %>%
  filter(week_number >= week_number_min) %>%
  pull(week_end_date)

phe_caserates_agegroup_df$age_group <- factor(phe_caserates_agegroup_df$age_group,
                                              levels = c("0 to 4", "5 to 9", "10 to 19",
                                                         "20 to 29", "30 to 39", "40 to 49",
                                                         "50 to 59", "60 to 69", "70 to 79", "80 or over"))

## Make the graph
phe_caserates_agegroup_gg <- ggplot(data = phe_caserates_agegroup_df,
                                    mapping = aes(x = week_end_date,
                                                  y = age_group,
                                                  fill = case_rate)) +
  geom_raster() +
  scale_fill_gradient(name = "",
                      low = "#FFFFFF",
                      high = "#d1112e",
                      guide = FALSE) +
  geom_text(aes(label = round(case_rate))) +
  scale_x_datetime(expand = c(0,0),
                    breaks = week_end_breaks,
                    date_labels = "%d-%b") + 
  labs(title = "In England, viral resurgence does not remain contained within one age group.",
       subtitle = "Weekly COVID-19 lab-confirmed cases (pillar 1 and 2) in England per 100,000 people based on ONS population estimates, by age group. The latest statistics are provisional.",
       x = "Week End Date (Sunday)",
       y = "",
       caption = "Data: Public Health England: National COVID-19 surveillance data report: 25 September 2020 (week 39), Figure 4.")
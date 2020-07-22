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
ph_ons_deaths_df <- read_excel("Coronavirus Deaths by Source/PHE and ONS Comparisons - 2020-07-22.xlsx",
                               sheet = "DATA",
                               col_types = c("date", "text", "numeric", "numeric")) %>%
  pivot_longer(cols = 3:4,
               names_to = "data_source",
               values_to = "new_deaths",
               values_drop_na = TRUE)

## Make the graphs
phe_ons_england_gg <- ph_ons_deaths_df %>%
  filter(area_name == "England") %>%
  ggplot(aes(x = date_of_death,
             y = new_deaths,
             group = data_source)) +
  geom_line(aes(color = data_source),
            size = 1.5) +
  scale_x_datetime(date_labels = "%d-%b-%Y",
                   expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 1300),
                     expand = c(0,0)) +
  labs(title = "Since the end of May, PHE confirmed deaths outnumbered ONS COVID-19 certificate mentions in England.",
       subtitle = "Deaths involving COVID-19 in England, by date of death. These figures are provisional, and could be revised.",
       caption = "Author's calculations. Data: ONS (Comparison of weekly death occurrences in England and Wales: up to week ending 10 July 2020); Public Health England Coronavirus Staging Dashboard.",
       x = "Date of Death",
       y = "") +
  scale_color_manual(name = "",
                    breaks = c("ons_deaths", "ph_new_deaths"),
                    labels = c("Office for National Statistics (Death certificate mentions - registered to 18th July)",
                               "Public Health England (Deaths with a positive test - up to 20th July)"),
                    values = c("#e5942e","#04867b")) +
  annotate("text",
           x = as.POSIXct(as.Date("2020-05-01")),
           y = 1100,
           label = "The ONS counts death certificates that mention COVID-19:\nincluding suspected deaths that do not have lab-confirmation.",
           hjust = -0.05, size = 5) +
  annotate("curve",
           x = as.POSIXct(as.Date("2020-04-30")),
           xend = as.POSIXct(as.Date("2020-04-20")),
           y = 1100, yend = 1000,
           curvature = 0.3, arrow = arrow(length = unit(2, "mm")),
           color = "#e5942e") +
  annotate("text",
           x = as.POSIXct(as.Date("2020-06-08")),
           y = 500,
           label = "PHE deaths are defined as:\nany death with a positive test\nresult for SARS-CoV-2.",
           hjust = -0.05, size = 5) +
  annotate("curve",
           x = as.POSIXct(as.Date("2020-06-07")),
           xend = as.POSIXct(as.Date("2020-06-02")),
           y = 500, yend= 250,
           curvature = 0.2, arrow = arrow(length = unit(2, "mm")),
           color = "#04867b")

phw_ons_wales_gg <- ph_ons_deaths_df %>%
  filter(area_name == "Wales") %>%
  ggplot(aes(x = date_of_death,
             y = new_deaths,
             group = data_source)) +
  geom_line(aes(color = data_source),
            size = 1.5) +
  scale_x_datetime(date_labels = "%d-%b-%Y",
                   expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 80),
                     expand = c(0,0)) +
  labs(title = "Public Health Wales deaths in hospitals and care homes require a positive test, and clinical suspicion that COVID-19 was a causative factor.",
       subtitle = "Deaths involving COVID-19 in Wales, by date of death. These figures are provisional, and could be revised.",
       caption = "Author's calculations. Data: ONS (Comparison of weekly death occurrences in England and Wales: up to week ending 10 July 2020); Public Health England Coronavirus Staging Dashboard.",
       x = "Date of Death",
       y = "") +
  scale_color_manual(name = "",
                     breaks = c("ons_deaths", "ph_new_deaths"),
                     labels = c("Office for National Statistics (Death certificate mentions - registered to 18th July)",
                                "Public Health Wales (up to 20th July)"),
                     values = c("#e5942e","#04867b"))
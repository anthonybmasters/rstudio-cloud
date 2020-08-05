## Packages and Themes
library(tidyverse)
library(readxl)
library(lubridate)

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

## Draw and tidy data
coronavirus_gb_df <- read_excel("Coronavirus Deaths by Source/Coronavirus Deaths by Data Source - 2020-08-05.xlsx",
                                    sheet = "DATA",
                                    col_types = c("date", "text", "text", "text", "numeric"))

phs_scotland_df <- read_csv(url("https://www.opendata.nhs.scot/dataset/b318bddf-a4dc-4262-971f-0ba329e09b87/resource/287fc645-4352-4477-9c8c-55bc054b7e76/download/daily_cuml_scot_20200730.csv"))

phs_scotland_df <- phs_scotland_df %>%
  mutate(date_of_death = as_date(parse_date_time(Date, orders = "ymd")),
         new_deaths = Deaths - lag(Deaths, 1),
         new_deaths = replace_na(new_deaths, 0),
         area_name = "Scotland",
         data_source = "Public Health Scotland",
         source_type = "Public Health Agency") %>%
  select(date_of_death, area_name, data_source, source_type, new_deaths)

coronavirus_bysource_df <- bind_rows(coronavirus_gb_df, phs_scotland_df)

coronavirus_bysource_df %>% group_by(area_name, source_type) %>%
  summarise(deaths = sum(new_deaths)) %>%
  pivot_wider(names_from = source_type,
              values_from = deaths) %>%
  mutate(Ratio = `Public Health Agency` / `Statistics Office`)

## Make graphs
coronavirus_england_gg <- coronavirus_bysource_df %>%
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
       caption = "Author's calculations. Data: ONS (Deaths registered weekly in England and Wales, provisional: week ending 24 July 2020); Public Health England Coronavirus Staging Dashboard.",
       x = "Date of Death",
       y = "") +
  scale_color_manual(name = "",
                     breaks = c("Office for National Statistics", "Public Health England"),
                     labels = c("Office for National Statistics (Death certificate mentions - registered to 1st August)",
                                "Public Health England (Deaths with a positive test - up to 4th August)"),
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

coronavirus_wales_gg <- coronavirus_bysource_df %>%
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
  labs(title = "PHW deaths in hospitals and care homes need a positive test, and clinical suspicion that COVID-19 was a causative factor.",
       subtitle = "Deaths involving COVID-19 in Wales, by date of death. These figures are provisional, and could be revised.",
       caption = "Author's calculations. Data: ONS (Deaths registered weekly in England and Wales, provisional: week ending 24 July 2020); Public Health England Coronavirus Staging Dashboard.",
       x = "Date of Death",
       y = "") +
  scale_color_manual(name = "",
                     breaks = c("Office for National Statistics", "Public Health Wales"),
                     labels = c("Office for National Statistics (Death certificate mentions - registered to 1st August)",
                                "Public Health Wales (recorded up to 4th August)"),
                     values = c("#e5942e","#04867b"))

coronavirus_scotland_gg <- coronavirus_bysource_df %>%
  filter(area_name == "Scotland",
         date_of_death >= "2020-03-12",
         date_of_death <= "2020-07-26") %>%
  ggplot(aes(x = date_of_death,
             y = new_deaths,
             group = data_source)) +
  geom_line(aes(color = data_source),
            size = 1.5) +
  scale_x_datetime(date_labels = "%d-%b-%Y",
                   expand = expansion(mult = c(0,0))) +
  scale_y_continuous(limits = c(0, 120),
                     expand = c(0,0)) +
  labs(title = "National Records of Scotland has registered over 4,200 deaths in Scotland involving COVID-19.",
       subtitle = "Deaths involving COVID-19 in Scotland, by date of death. These figures are provisional, and subject to change.",
       caption = "Author's calculations. Data: National Records Scotland (Deaths involving coronavirus in Scotland); Public Health Scotland Open Data",
       x = "Date of Death",
       y = "") +
  scale_color_manual(name = "",
                     breaks = c("National Records of Scotland", "Public Health Scotland"),
                     labels = c("National Records of Scotland (Death certificate mentions - registered up to 2nd August)",
                                "Public Health Scotland (Deaths with a positive test - up to 5th August)"),
                     values = c("#e5942e","#04867b")) +
  annotate("text",
           x = as.POSIXct(as.Date("2020-05-01")),
           y = 100,
           label = "The NRS counts death certificates that mention COVID-19:\nincluding suspected deaths that do not have lab-confirmation.",
           hjust = 0, size = 5) +
  annotate("curve", x = as.POSIXct(as.Date("2020-04-30")),
           xend = as.POSIXct(as.Date("2020-04-27")),
           y = 100, yend = 90,
           curvature = 0.3, arrow = arrow(length = unit(2, "mm")),
           color = "#e5942e") +
  annotate("text",
           x = as.POSIXct(as.Date("2020-04-10")),
           y = 20,
           label = "PHS deaths are defined as:\na death within 28 days of their\nfirst positive test result for SARS-CoV-2.",
           hjust = 0, size = 5) +
  annotate("curve",
           x = as.POSIXct(as.Date("2020-05-11")),
           xend = as.POSIXct(as.Date("2020-05-13")),
           y = 20, yend= 30,
           curvature = 0.3, arrow = arrow(length = unit(2, "mm")),
           color = "#04867b")
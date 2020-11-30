## Load packages and set themes
  # First, we load our packages
remotes::install_github("publichealthengland/coronavirus-dashboard-api-R-sdk")
library(tidyverse)
library(lubridate)
library(ukcovid19)

  # Next, we set the plotting theme for the graphs
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

## Set queries and structures
  # These lines set the queries and structures of the requested statistics
covid19_query_filters <- c('areaType=nation',
                           'areaName=England')

covid19_structure <- list(
  date = "date",
  areaName = "areaName",
  areaCode = "areaCode",
  newCasesByPublishDate = "newCasesByPublishDate")

  # A list of valid measures is on the UK COVID-19 dashboard website:
  # https://coronavirus.data.gov.uk/details/developers-guide#structure-metrics

## Getting the data
  # We then use the query and structure to draw the data, with the function
  # The mutation is necessary as the drawn dates are characters
covid19_eng_data <- get_data(
  filters = covid19_query_filters, 
  structure = covid19_structure) %>%
  mutate(date = lubridate::as_date(date),
         weeklyCasesByPublishDateChange = newCasesByPublishDate/lead(newCasesByPublishDate, 7) - 1)

  # This function allows us to see when the last update was
covid19_eng_timestamp <- last_update(
  filters = covid19_query_filters, 
  structure = covid19_structure)

## Making a graph
  # First, we set our labels
covid19_change <- 100*covid19_eng_data$weeklyCasesByPublishDateChange[1]

covid19_eng_latest <- case_when(
  covid19_change < -1 ~ paste0(round(-covid19_change, 0), "% lower than"),
  covid19_change > 1 ~ paste0(round(covid19_change, 0), "% higher than"),
  TRUE ~ "about the same as")

covid19_eng_title <- paste0(
  "On ",
  max(covid19_eng_data$date) %>% format("%a %d %B"),
  ", there were ",
  covid19_eng_data$newCasesByPublishDate[1] %>% format(big.mark = ","),
  " lab-confirmed cases in England. This figure is ",
  covid19_eng_latest,
  " a week ago.")

covid19_eng_subtitle <- "The number of people with at least one positive COVID-19 test result in England, by date reported."

covid19_eng_caption <- paste0(
  "Data: Public Health England UK COVID-19 Package (ukcovid19). Last updated: ",
  covid19_eng_timestamp %>% format("%A %d %B %Y"),
  ".")

# Next, we make the graph
covid19_eng_gg <- covid19_eng_data %>%
  filter(date >= "2020-03-01") %>%
  ggplot(aes(x = date,
             y = newCasesByPublishDate)) +
  geom_line(color = "#008080",
            size = 1.5) +
  labs(title = covid19_eng_title,
       subtitle = covid19_eng_subtitle,
       x = "Reporting date",
       y = "",
       caption = covid19_eng_caption) +
  scale_x_date(expand = c(0,0),
               date_labels = "%d-%b-%Y") +
  scale_y_continuous(expand = c(0,0)) +
  geom_vline(xintercept = as_date("2020-07-02"),
             linetype = "dashed") +
  annotate("text",
           x = as_date("2020-05-14"),
           y = 7000,
           label = "Before 2nd July, these figures only\nincluded pillar 1 (NHS and PHE lab) tests.",
           color = "red")
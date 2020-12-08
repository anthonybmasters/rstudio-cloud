## Packages and themes
  # First, we load the packages we need
library(tidyverse)
library(readxl)
library(gganimate)
library(transformr)
library(gifski)

  # Next, we set the plotting theme for the animated graph
theme_clean <- theme_bw(base_family="Calibri") + 
  theme(legend.position = "top",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b=12)),
        plot.caption = element_text(size = 10),
        plot.margin = unit(c(.5,.5,.5,.5), "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())
theme_set(theme_clean)

## Draw the data
  # We draw the data from a prepared file
ons_model_df <- read_excel("ONS COVID-19 Model Estimates/ONS Modelled England Incidence Rate Statistics - 2020-12-08.xlsx",
                           sheet = "DATA",
                           col_types = c("date", "date", "numeric", "numeric", "numeric"))

## Create the animated graph
  # We code the animated graph
ons_model_agg <- ons_model_df %>%
  ggplot(aes(x = modelled_date)) +
  geom_line(aes(y = incidence_rate_estimate),
            color = "#008080",
            size = 1.2) +
  geom_ribbon(aes(ymin = incidence_rate_lower,
                  ymax = incidence_rate_upper),
              fill = "#008080",
              alpha = 0.2) +
  labs(title = "Each week, the COVID-19 incidence model in England is smoothed and revised.",
       subtitle = "Estimated number of daily new infections per 10,000 people in England (with 95% credible intervals).
       Publication date: {closest_state}",
       caption = "Source: ONS COVID-19 Infection Surveys, 30th October to 4th December 2020.",
       x = "Date",
       y = "") +
  scale_x_datetime(date_labels = "%d-%b",
                   expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, NA)) +
  theme(axis.text = element_text(size = 12)) +
  transition_states(publication_date)

ons_model_gif <- animate(ons_model_agg, height = 650, width = 1000)
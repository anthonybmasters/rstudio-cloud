## Install Packages
library(tidyverse)
library(readxl)
library(lubridate)
library(scales)

## Set the theme
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

## Import and tidy the data
BARB_week_df <- read_excel("BARB Weekly Viewing/BARB Weekly Viewing Summary by Channel - 2020-01-04.xlsx",
                           sheet = "DATA") %>%
  rename(Avg_WeekView_wf = Avg_Weekly_Viewing) %>%
  mutate(Week_EndDate = as_date(Week_EndDate),
         Avg_Weekly_Viewing = make_datetime(min = hour(Avg_WeekView_wf),
                                             sec = minute(Avg_WeekView_wf)))

## Create the graphs
BARB_reach_gg <- BARB_week_df %>% ggplot(aes(x = Week_EndDate,
                                             y = Wk_Reach_Sh,
                                             group = Channel)) +
  geom_line(aes(color = Channel), size = 1.2) +
  scale_x_date(breaks = date_breaks("4 weeks"),
               labels = date_format("%d-%b")) +
  labs(title = "The BBC's estimated reach was largely unchanged in December",
       subtitle = "Estimated weekly reach [%], by channel.",
       caption = "Source: BARB, Weekly Viewing Summary.",
       x = "Week (Sunday Date)",
       y = "")
BARB_share_gg <- BARB_week_df %>% ggplot(aes(x = Week_EndDate,
                                             y = Avg_Week_View_Sh,
                                             group = Channel)) +
  geom_line(aes(color = Channel), size = 1.2) +
  scale_x_date(breaks = date_breaks("4 weeks"),
               labels = date_format("%d-%b")) +
  labs(title = "BBC1 estimated viewing share returns to the mean",
       subtitle = "Estimated average weekly viewing share [%], by channel.",
       caption = "Source: BARB, Weekly Viewing Summary.",
       x = "Week (Sunday Date)",
       y = "")
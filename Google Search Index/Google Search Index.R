## Install packages
library(tidyverse)
library(readxl)

## Set theme
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
Google_Search_Index_df <- read_excel(path = "Google Search Index - 2020-06-11.xlsx", 
                                     sheet = "DATA",
                                     col_types = c("date", "numeric", "numeric", "numeric"))
google_search_1_df <- Google_Search_Index_df %>%
  select(1:3) %>%
  rename("Cream tea" = "Cream tea delivery (relative)",
         "McDonald's" = "McDonalds delivery") %>%
  pivot_longer(cols = 2:3,
               names_to = "Term",
               values_to = "Index",
               values_drop_na = TRUE)

google_search_2_df <- Google_Search_Index_df %>%
  select(1, 3, 4) %>%
  rename("McDonald's" = "McDonalds delivery",
         "Cream tea" = "Cream tea delivery") %>%
  pivot_longer(cols = 2:3,
               names_to = "Term",
               values_to = "Index",
               values_drop_na = TRUE)

## Making graphs
google_search_1_gg <- ggplot(data = google_search_1_df,
                             aes(x = Day,
                                 y = Index,
                                 group = Term)) +
  geom_line(aes(color = Term), size = 1.5) +
  scale_x_datetime(date_labels = "%d-%b-%Y",
                   expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 100),
                     expand = c(0,0)) +
  labs(title = "Searching on Google for McDonald's delivery is more common than cream tea.",
       subtitle = "Rounded index of UK Google 'delivery' search volumes. 100 is the 'McDonald's delivery' volume on 3rd June 2020.",
       caption = "Data: Google Trends (United Kingdom).",
       x = "Date",
       y = "")

google_search_2_gg <- ggplot(data = google_search_2_df,
                             aes(x = Day,
                                 y = Index,
                                 group = Term)) +
  geom_line(aes(color = Term), size = 1.5) +
  scale_x_datetime(date_labels = "%d-%b-%Y",
                   expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 100),
                     expand = c(0,0)) +
  labs(title = "There was a spike in 'Cream tea delivery' Google searches when the BBC published their article.",
       subtitle = "Rounded index of UK Google 'delivery' search volumes. 100 is the highest daily value for each term.",
       caption = "Data: Google Trends (United Kingdom).",
       x = "Date",
       y = "")
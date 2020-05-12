## Install packages
library(tidyverse)

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

## Import and tidy data
death_regs_EW_df <- read_excel("Weekly Death Registrations in EW - 2020-05-12.xlsx",
                                                         sheet = "DATA-E&W-Tidy",
                                                         col_types = c("numeric", "numeric", "date", "numeric",
                                                                       "numeric", "numeric", "numeric", "numeric")) %>%
  mutate(Deaths_45_over = Deaths_45_64 + Deaths_65_74 + Deaths_75_84 + Deaths_85_over)

death_regs_time_df <- death_regs_EW_df %>%
  select(3:8) %>%
  rename("Total Deaths" = Total_Deaths,
         "Aged 45 to 64" = Deaths_45_64,
         "Aged 65 to 74" = Deaths_65_74,
         "Aged 75 to 84" = Deaths_75_84,
         "Aged 85 and over" = Deaths_85_over) %>%
  pivot_longer(cols = 2:6,
               names_to = "Age_Group",
               values_to = "Deaths")

death_regs_time_df$Age_Group <- factor(death_regs_time_df$Age_Group,
                                       levels = c("Total Deaths", "Aged 45 to 64",
                                                  "Aged 65 to 74", "Aged 75 to 84", "Aged 85 and over"))

death_w41to52_df <- death_regs_EW_df %>%
  filter(Week_No >= 41 & Week_No <= 52) %>%
  select(Week_No, Year, Total_Deaths, Deaths_45_over) %>%
  group_by(Year) %>%
  summarise("Total Deaths" = sum(Total_Deaths),
            "Aged 45 and over" = sum(Deaths_45_over)) %>%
  pivot_longer(cols = 2:3,
               names_to = "Age_Group",
               values_to = "Deaths")

death_w41to52_df$Age_Group <- factor(death_w41to52_df$Age_Group,
                                      levels = c("Total Deaths", "Aged 45 and over"))

death_regs_range_df <- death_regs_EW_df %>%
  group_by(Week_No) %>%
  filter(Year <= 2018,
         Week_No <= 52) %>%
  summarise(min_2010_2018 = min(Deaths_45_over),
            max_2010_2018 = max(Deaths_45_over))

death_regs_2019_df <- death_regs_EW_df %>%
  filter(Year == 2019) %>%
  select(Week_No, Deaths_45_over) %>%
  rename(Y2019 = Deaths_45_over)

death_regs_tidy_df <- full_join(death_regs_range_df,
                                 death_regs_2019_df,
                                 by = "Week_No")

## Make graphs
death_time_gg <- death_regs_time_df %>%
  ggplot(aes(x = Week_End, y = Deaths, group = Age_Group)) +
  geom_line(aes(color = Age_Group),
            size = 1.5) +
  scale_x_datetime(date_breaks = "52 weeks",
                   date_labels = "%d-%b-%Y",
                   expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 18000),
                     expand = c(0,0)) +
  labs(title = "There is a seasonal cycle in death registrations in England and Wales",
       subtitle = "Weekly death registrations in age groups. These statistics are by registration date, for England and Wales.",
       caption = "Data: ONS: Deaths registered weekly in England and Wales, 2010 to 2019.",
       x = "Registration Week Number",
       y = "",
       color = "")

death_w41to52_gg <- death_w41to52_df %>%
  ggplot(aes(x = factor(Year), y = Deaths, group = Age_Group)) +
  geom_line(aes(color = Age_Group),
            size = 1.5) +
  geom_point(aes(color = Age_Group),
             fill = "white",
             shape = 21,
             size = 3) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 130000),
                     expand = c(0,0)) +
  labs(title = "In weeks 41 to 52, there is a general trend of increased English and Welsh death registrations",
       subtitle = "Weekly death registrations in age groups (summed for 45 and over). These statistics are for registration weeks 41 to 52, for England and Wales.",
       caption = "Author's calculations. Based on ONS: Deaths registered weekly in England and Wales, 2010 to 2019. Registrations missing a coded age are not included in age groups.",
       x = "Year",
       y = "",
       color = "")

death_regs_gg <- death_regs_tidy_df %>%
  ggplot() +
  geom_line(aes(x = Week_No, y = Y2019),
            size = 1.5,
            color = "blue") +
  geom_ribbon(aes(x = Week_No,
                  ymin = min_2010_2018,
                  ymax = max_2010_2018),
                  fill = "grey12",
              alpha = 0.2) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 16000)) +
  annotate('text', x = 20, y = 14000,
           label = "Death registrations (aged 45 and over) in 2019", col = "blue",
           size = 5) +
  annotate('text', x = 20, y = 6000,
           label = "Range for 2010 to 2018", col = "grey12",
           alpha = 0.5, size = 5) +
  labs(title = "In 2019, deaths for those aged 45 and over were moderately elevated compared to previous years",
       subtitle = "Weekly death registrations for those aged 45 and over (summed). These statistics are by registration date, for England and Wales.",
       caption = "Author's calculations. Based on ONS: Deaths registered weekly in England and Wales, 2010 to 2019. Registrations missing a coded age are not included in age groups.",
       x = "Registration Week Number",
       y = "")
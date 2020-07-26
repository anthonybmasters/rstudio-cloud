## Packages and Themes
library(tidyverse)
library(readxl)
library(forecast)

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
hesa_degree_df <- read_excel("A Degree of Extrapolation/HESA Class of Degree by First Degree Graduates - 2019-12-30.xlsx",
                             sheet = "DATA") %>%
  dplyr::rename(ThirdPass = "Third/Pass") %>%
  dplyr::mutate(Classified = First + Upper_Second + Lower_Second + ThirdPass,
                First_Share = (First / Classified)* 100)

hesa_class_df <- hesa_degree_df %>%
  select(1:5) %>%
  pivot_longer(cols = 2:5,
               names_to = "Class",
               values_to = "Count")

hesa_class_df$Class <- factor(hesa_class_df$Class,
                              levels = c("First", "Upper_Second", "Lower_Second", "ThirdPass"))

## Draw the graphs
hesa_class_gg <- ggplot(data = hesa_class_df,
                        aes(x = AcademicYear_EY,
                            y = Count,
                            group = Class)) +
  geom_line(size = 1.2,
            aes(color = Class)) +
  scale_color_discrete(name = "Classification",
                       labels = c("First", "Upper Second", "Lower Second", "Third/Pass")) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "First and Upper Second degrees have generally risen in volume",
       subtitle = "Rounded classifications of first degree graduates, in each academic year at UK institutions.",
       caption = "Source: HESA Enrolment and Qualification reports; HESA Higher Education Student Statistics.",
       x = "Academic Year (End Year)",
       y = "")

hesa_firstshare_gg <- ggplot(data = hesa_degree_df,
                             aes(x = AcademicYear_EY,
                                 y = First_Share)) +
  geom_line(size = 1.2) +
  geom_point(fill = "white", size = 3, shape = 21) +
  geom_text(aes(label = round(First_Share)), nudge_y = 2) +
  ylim(c(0,30)) +
  labs(title = "28% of first-time graduates received a First-class degree in 2017/18",
       subtitle = "Proportion of Firsts among first degree graduates with classified degrees at UK institutions [%].",
       caption = "Source: HESA Enrolment and Qualification reports; HESA Higher Education Student Statistics.",
       x = "Academic Year (End Year)",
       y = "")

## Forecasting
hesa_firstshare_df <- hesa_degree_df %>%
  dplyr::select("AcademicYear_EY", "First_Share")

hesa_firstshare_ts <- ts(data = hesa_firstshare_df$First_Share,
                         start = 2004,
                         frequency = 1)

forecast(hesa_firstshare_ts, h = 12)

forecast(hesa_firstshare_ts, h = 12)$model

hesa_tspred_gg <- forecast(hesa_firstshare_ts, h = 12) %>%
  autoplot() +
  labs(title = "These forecasts have wide prediction intervals by 2029/30",
       subtitle = "Forecasts from ETS(M,A,N) of the proportion of Firsts achieved by first-time graduates at UK institutions [%]. (80% and 95% prediction intervals shown.)",
       caption = "Sources: HESA Higher Education Statistics; Forecast function in R.",
       x = "Academic Year (End Year)", y = "") +
  ylim(c(0,100))

## Forecasting - Logit Transformation
hesa_fslogit_df <- hesa_degree_df %>%
  dplyr::mutate(First_Share_Logit = log(First_Share/(100-First_Share))) %>%
  dplyr::select("AcademicYear_EY", "First_Share_Logit")

hesa_fslogit_ts <- ts(data = hesa_fslogit_df$First_Share_Logit,
                         start = 2004,
                         frequency = 1)

hesa_fslogit_fc <- forecast(hesa_fslogit_ts, h = 12)

hesa_fslogit_fc$model

hesa_fslogit_fc$mean <- 100*exp(hesa_fslogit_fc$mean)/(1+exp(hesa_fslogit_fc$mean))
hesa_fslogit_fc$lower <- 100*exp(hesa_fslogit_fc$lower)/(1+exp(hesa_fslogit_fc$lower))
hesa_fslogit_fc$upper <- 100*exp(hesa_fslogit_fc$upper)/(1+exp(hesa_fslogit_fc$upper))
hesa_fslogit_fc$x <- 100*exp(hesa_fslogit_fc$x)/(1+exp(hesa_fslogit_fc$x))

hesa_fslogit_fc

hesa_fslogit_gg <- hesa_fslogit_fc %>%
  autoplot() +
  labs(title = "For 2029/30, the central forecast is 58% (with the 80% prediction interval from 45% to 70%).",
       subtitle = "ETS(A,A,N) forecast of the logit-transformed Firsts proportion achieved by first-time graduates at UK institutions [%] (80% and 95% prediction intervals shown).",
       caption = "Sources: HESA Higher Education Statistics; Forecast function in R.",
       x = "Academic Year (End Year)", y = "") +
  scale_y_continuous(limits = c(0,100),
                     expand = c(0,0))
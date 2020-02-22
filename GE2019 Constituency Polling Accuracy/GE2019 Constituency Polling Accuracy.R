## Install packages
library(tidyverse)
library(readxl)
library(lubridate)

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
polls_estimate_df <- read_excel("GE2019 Constituency Polling Accuracy - 2020-02-20.xlsx",
                                sheet = "DATA") %>%
  dplyr::select("company", "ons_id", "constituency_name",
                "fw_start", "fw_end", "sample_size",
                "con", "lab", "ldem", "grn", "bxp", "oth",
                "est_1st_party", "est_2nd_party")

ge2019_results_df <- read_excel("GE2019 Constituency Polling Accuracy - 2020-02-20.xlsx",
                                sheet = "GE2019-RESULTS") %>%
  mutate(con_vs = con / valid_votes,
         lab_vs = lab / valid_votes,
         ldem_vs = ld / valid_votes,
         grn_vs = green / valid_votes,
         bxp_vs = brexit / valid_votes,
         oth_vs = other / valid_votes) %>%
  dplyr::select("ons_id", "region_name",
                "result", "first_party", "second_party",
                "con_vs", "lab_vs", "ldem_vs", "grn_vs", "bxp_vs", "oth_vs")

## Join and create the tables
polls_innerjoin_df <- inner_join(polls_estimate_df, ge2019_results_df,
                                 by = "ons_id") %>%
  mutate(days_to = as.numeric(as_date(fw_end) - as_date("2019-12-12")),
         CON = ifelse(con_vs == 0, NA, con - con_vs),
         LAB = ifelse(lab_vs == 0, NA, lab - lab_vs),
         LDEM = ifelse(ldem_vs == 0, NA, ldem - ldem_vs),
         GRN = ifelse(grn_vs == 0, NA, grn - grn_vs),
         BXP = ifelse(bxp_vs == 0, NA, bxp - bxp_vs),
         OTH = ifelse(oth_vs == 0, NA, oth - oth_vs),
         CONBXP = (con + bxp) - (con_vs + bxp_vs),
         party_mae = (1/3)*(abs(CON) + abs(LAB) + abs(LDEM)))

polls_error_df <- polls_innerjoin_df %>%
  dplyr::select("company", "ons_id", "constituency_name", "days_to",
                "CON", "LAB", "LDEM", "GRN", "BXP", "OTH") %>%
  pivot_longer(cols = 5:10,
               names_to = "Party",
               values_to = "Error",
               values_drop_na = TRUE)
polls_error_df$Party = factor(polls_error_df$Party,
                              levels = c("CON", "LAB", "LDEM", "GRN", "BXP", "OTH"))

polls_call_df <- polls_innerjoin_df %>%
  dplyr::filter(days_to > -21) %>%
  dplyr::select("company", "constituency_name",
                "fw_start", "fw_end", "days_to",
                "est_1st_party", "est_2nd_party",
                "first_party", "second_party",
                "CON", "LAB", "LDEM", "GRN", "BXP", "OTH", "CONBXP", "party_mae")

polls_call_df %>%
  select("CON", "LAB", "LDEM", "GRN", "BXP", "OTH", "CONBXP", "party_mae") %>%
  summarise_all(list(mean = mean, sd = sd), na.rm = TRUE) %>% #create columns called PARTY_mean and PARTY_sd
  pivot_longer(everything(), names_to = c("PARTY", "measure"),
               names_pattern = "(.*)_(.*)") %>% #identifies PARTY and measure using regex
  pivot_wider(names_from = "measure", values_from = "value") %>% #then we spread the columns
  mutate(mean = 100 * mean, sd = 100 * sd) #multiply by 100 to give the values in percentage points

## Create the graphs
polls_error_gg <- ggplot(polls_error_df,
                         aes(x = days_to, y = Error*100)) +
  geom_point(aes(color = company), na.rm = TRUE,
             size = 3) +
  facet_wrap(~Party) +
  geom_hline(aes(yintercept = -5), linetype = "dashed") +
  geom_hline(aes(yintercept = 5), linetype = "dashed") +
  scale_color_manual(name = "Company",
                     values = c("#BDBDBD", "#424242")) +
  labs(title = "As expected, errors reduced as constituency polls were conducted closer to the election.",
       subtitle = "Error (rounded survey estimate minus actual) in percentage points, for constituency polls. Error bounds of five points are shown.",
       x = "Days to Election (fieldwork end-date)",
       y = "Error [pp]",
       caption = "Source: Author's calculations based on Deltapoll and Survation polling archives (Oct-Dec 2019) and House of Commons Library CBP-8749.")

polls_3pae_gg <- ggplot(polls_innerjoin_df,
                       aes(x = days_to, y = party_mae*100)) +
  geom_point(aes(color = company), na.rm = TRUE, size = 3) +
  geom_smooth(method = "loess", na.rm = TRUE,
              color = "#1D1D1D", fill = "#F5F5F5") +
  geom_hline(aes(yintercept = 4), linetype = "dashed") +
  annotate("text", x = -60, y = 3.5, label = "3PAE = 4 points") +
  scale_color_manual(name = "Company",
                     values = c("#BDBDBD", "#424242")) +
  labs(title = "Absolute errors were centred around 3.7 points for polls conducted after 21st November.",
       subtitle = "Three party absolute errors for the Conservative, Labour and Liberal Democrat estimates. Loess regression curve (with 95% confidence intervals).",
       x = "Days to Election (fieldwork end-date)",
       y = "3PAE [pp]",
       fill = "Company",
       caption = "Source: Author's calculations based on Deltapoll and Survation polling archives and House of Commons Library CBP-8749.")
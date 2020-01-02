#Load packages
library(tidyverse)
library(readxl)
library(gganimate)
library(gifski)
library(png)

#Set the theme and colours
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
gb_party_colours <- c("#0087DC", "#DC241f", "#FAA61A", "#12B6CF", "#6AB023")

#Import and tidy the data
HoC_GE2017_results_df <- read_excel("HoC-GE2017-constituency-results.xlsx")
HoC_GE2019_results_df <- read_excel("HoC-GE2019-by-constituency.xlsx")
GE19_res_df <- HoC_GE2019_results_df %>%
  filter(country_name != "Northern Ireland") %>%
  select(ons_id, constituency_name, declaration_time, result, valid_votes, con, lab, ld, brexit, green) %>%
  rename(votes19 = valid_votes, con19 = con, lab19 = lab, ldem19 = ld, bxp19 = brexit, grn19 = green) %>%
  arrange(declaration_time)
GE17_res_df <- HoC_GE2017_results_df %>%
  filter(country_name != "Northern Ireland") %>%
  select(ons_id, valid_votes, con, lab, ld, green) %>%
  rename(votes17 = valid_votes, con17 = con, lab17 = lab, ldem17 = ld, grn17 = green)
vote_share_change_df <- full_join(GE19_res_df, GE17_res_df, by = "ons_id") %>%
  mutate(con19csh = 100*cumsum(con19)/cumsum(votes19),
         lab19csh = 100*cumsum(lab19)/cumsum(votes19),
         ldem19csh = 100*cumsum(ldem19)/cumsum(votes19),
         bxp19csh = 100*cumsum(bxp19)/cumsum(votes19),
         grn19csh = 100*cumsum(grn19)/cumsum(votes19),
         con17csh = 100*cumsum(con17)/cumsum(votes17),
         lab17csh = 100*cumsum(lab17)/cumsum(votes17),
         ldem17csh = 100*cumsum(ldem17)/cumsum(votes17),
         grn17csh = 100*cumsum(grn17)/cumsum(votes17),
         con_change = con19csh - con17csh,
         lab_change = lab19csh - lab17csh,
         ldem_change = ldem19csh - ldem17csh,
         bxp_change = bxp19csh - 0,
         grn_change = grn19csh - grn17csh)
GB_change_df <- vote_share_change_df %>%
  group_by(declaration_time) %>%
  summarise(CON = last(con_change),
            LAB = last(lab_change),
            LDEM = last(ldem_change),
            BXP = last(bxp_change),
            GRN = last(grn_change)) %>%
  pivot_longer(cols = 2:6,
               names_to = "Party",
               values_to = "Change")
GB_change_df$Party <- factor(GB_change_df$Party,
                             levels = c("CON", "LAB", "LDEM", "BXP", "GRN"))

#Create the graphs
GB_change_gg <- GB_change_df %>%
  filter(declaration_time <= "2019-12-13 06:00:00") %>%
  ggplot(aes(x = declaration_time, y = Change, group = Party)) +
  geom_step(aes(color = Party), size = 1.2) +
  ylim(c(-15, 15)) +
  scale_color_manual(values = gb_party_colours) +
  labs(title = "Votes share changes for the main parties settled after about 4am on election night",
       subtitle = "Cumulative aggregate GB vote share changes between 2017 and 2019, in percentage points, by declaration time.",
       caption = "Source: Author's calculations, based on House of Commons Library CBP-8749 and CBP-7979.",
       x = "Declaration Time (on 12-13th December 2019)",
       y = "")
GB_change_gif <- GB_change_df %>%
  filter(declaration_time <= "2019-12-13 06:00:00") %>%
  ggplot(aes(x = declaration_time, y = Change, group = Party)) +
  geom_step(aes(color = Party), size = 1.2) +
  geom_point(aes(color = Party), size = 1.4) +
  ylim(c(-15, 15)) +
  scale_color_manual(values = gb_party_colours) +
  labs(title = "Declared vote share changes settled after about 4am",
       subtitle = "Cumulative aggregate GB vote share changes between 2017 and 2019 [pp]",
       caption = "Source: Author's calculations, based on House of Commons Library CBP-8749 and CBP-7979.",
       x = "Declaration Time (on 12-13th December 2019)",
       y = "") +
  transition_reveal(declaration_time)
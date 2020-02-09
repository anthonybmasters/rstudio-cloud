#Load the packages
library(tidyverse)
library(readxl)
library(stringr)

#Set the theme
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

#Import the data
vote_eng_df <- read_excel("GE Vote Shares 1918 to 2019/UK General Election Vote Shares (1918 - 2019) - 2019-12-29.xlsx",
                          sheet = "DATA-Eng")
vote_scot_df <- read_excel("GE Vote Shares 1918 to 2019/UK General Election Vote Shares (1918 - 2019) - 2019-12-29.xlsx",
                            sheet = "DATA-Scot")
vote_wales_df <- read_excel("GE Vote Shares 1918 to 2019/UK General Election Vote Shares (1918 - 2019) - 2019-12-29.xlsx",
                            sheet = "DATA-Wales")

#Tidy the data. Scot and Wales vote counts are in thousands
thou_to_mill <- function(x, na.rm = FALSE)(x/1000)

vote_eng_df <- vote_eng_df %>%
  rename_at(2:6, list(~ str_replace(., "$", "_Eng")))
vote_scot_df <- vote_scot_df %>%
  rename_at(2:7, list(~ str_replace(., "$", "_Scot"))) %>%
  mutate_at(2:7, thou_to_mill)
vote_wales_df <- vote_wales_df %>%
  rename_at(2:7, list(~ str_replace(., "$", "_Wales"))) %>%
  mutate_at(2:7, thou_to_mill)

vote_join_df <- full_join(vote_eng_df, vote_scot_df, by = "General_Election") %>%
  full_join(., vote_wales_df, by = "General_Election")
vote_join_df$General_Election[16] <- "1974f"
vote_join_df$General_Election[17] <- "1974o"

voteshare_eng_df <- vote_join_df %>%
  select("General_Election", ends_with("_Eng")) %>%
  mutate(CON = CON_Eng*100 / TOTAL_Eng,
         LAB = LAB_Eng*100 / TOTAL_Eng,
         LDEM = LDEM_Eng*100 / TOTAL_Eng,
         OTH = OTH_Eng*100 / TOTAL_Eng) %>%
  select("General_Election", 7:10) %>%
  pivot_longer(cols = 2:5,
               names_to = "Party",
               values_to = "Share",
               values_drop_na = TRUE)

voteshare_scot_df <- vote_join_df %>%
  select("General_Election", ends_with("_Scot")) %>%
  mutate(CON = CON_Scot*100 / TOTAL_Scot,
         LAB = LAB_Scot*100 / TOTAL_Scot,
         LDEM = LDEM_Scot*100 / TOTAL_Scot,
         SNP = SNP_Scot*100 / TOTAL_Scot,
         OTH = OTH_Scot*100 / TOTAL_Scot) %>%
  select("General_Election", 8:12) %>%
  pivot_longer(cols = 2:6,
               names_to = "Party",
               values_to = "Share",
               values_drop_na = TRUE)

voteshare_wales_df <- vote_join_df %>%
  select("General_Election", ends_with("_Wales")) %>%
  mutate(CON = CON_Wales*100 / TOTAL_Wales,
         LAB = LAB_Wales*100 / TOTAL_Wales,
         LDEM = LDEM_Wales*100 / TOTAL_Wales,
         PCY = PCY_Wales*100 / TOTAL_Wales,
         OTH = OTH_Wales*100 / TOTAL_Wales) %>%
  select("General_Election", 8:12) %>%
  pivot_longer(cols = 2:6,
               names_to = "Party",
               values_to = "Share",
               values_drop_na = TRUE)
voteshare_engwales_df <- vote_join_df %>%
  select(-ends_with("_Scot")) %>% #as we need the first column, plus Eng and Wales
  mutate(TOTAL = TOTAL_Eng + TOTAL_Wales,
         CON = (CON_Eng + CON_Wales)*100 / TOTAL,
         LAB = (LAB_Eng + LAB_Wales)*100 / TOTAL,
         LDEM = (LDEM_Eng + LDEM_Wales)*100 / TOTAL,
         PCY = PCY_Wales*100 / TOTAL,
         OTH = (OTH_Eng + OTH_Wales)*100 / TOTAL) %>%
  select("General_Election", 14:18) %>%
  pivot_longer(cols = 2:6,
               names_to = "Party",
               values_to = "Share",
               values_drop_na = TRUE)

#Set factors
voteshare_eng_df$Party <- factor(voteshare_eng_df$Party,
                                  levels = c("CON", "LAB", "LDEM", "OTH"))
voteshare_scot_df$Party <- factor(voteshare_scot_df$Party,
                                 levels = c("CON", "LAB", "LDEM", "SNP", "OTH"))
voteshare_wales_df$Party <- factor(voteshare_wales_df$Party,
                                  levels = c("CON", "LAB", "LDEM", "PCY", "OTH"))
voteshare_engwales_df$Party <- factor(voteshare_engwales_df$Party,
                                   levels = c("CON", "LAB", "LDEM", "PCY", "OTH"))

#Create the graphs
voteshare_eng_gg <- ggplot(data = voteshare_eng_df,
                           aes(x = General_Election,
                               y = Share, group = Party)) +
  geom_line(aes(colour = Party), size = 1.2) +
  scale_colour_manual(values = c("#0087DC", "#DC241f", "#FAA61A", "#a9a9a9")) +
  labs(title = "Labour's deficit widened in England, from 3.5 points in 2017 to 13.3 points in 2019.",
       subtitle = "General Election vote share in England, [%].",
       caption = "Source: Author's calculation based on House of Commons Library CBP-7529 and CBP-8749.",
       x = "General Election",
       y = "")

voteshare_scot_gg <- ggplot(data = voteshare_scot_df,
                            aes(x = General_Election,
                                y = Share, group = Party)) +
  geom_line(aes(colour = Party), size = 1.2) +
  scale_colour_manual(values = c("#0087DC", "#DC241f", "#FAA61A", "#FDF38E", "#a9a9a9")) +
  labs(title = "The Scottish National Party have been the largest party since the 2014 referendum.",
       subtitle = "General Election vote share in Scotland, [%].",
       caption = "Source: Author's calculation based on House of Commons Library CBP-7529 and CBP-8749.",
       x = "General Election",
       y = "")

voteshare_wales_gg <- ggplot(data = voteshare_wales_df,
                            aes(x = General_Election,
                                y = Share, group = Party)) +
  geom_line(aes(colour = Party), size = 1.2) +
  scale_colour_manual(values = c("#0087DC", "#DC241f", "#FAA61A", "#008142", "#a9a9a9")) +
  labs(title = "The Conservatives reached a record Welsh vote share in 2019.",
       subtitle = "General Election vote share in Wales, [%].",
       caption = "Source: Author's calculation based on House of Commons Library CBP-7529 and CBP-8749.",
       x = "General Election",
       y = "")

voteshare_engwales_gg <- ggplot(data = voteshare_engwales_df,
                               aes(x = General_Election,
                                   y = Share, group = Party)) +
  geom_line(aes(colour = Party), size = 1.2) +
  scale_colour_manual(values = c("#0087DC", "#DC241f", "#FAA61A", "#008142", "#a9a9a9")) +
  labs(title = "Labour had more votes than the Conservatives in multiple elections since 1950.",
       subtitle = "General Election vote share in England and Wales, [%].",
       caption = "Source: Author's calculation based on House of Commons Library CBP-7529 and CBP-8749.",
       x = "General Election",
       y = "")
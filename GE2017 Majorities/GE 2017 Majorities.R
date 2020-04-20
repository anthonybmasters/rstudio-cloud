## Packages and Themes
library(tidyverse)

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

## Import data
ge2017_constituency_df <- read_csv(file = "http://researchbriefings.files.parliament.uk/documents/CBP-7979/HoC-GE2017-constituency-results.csv")

## Tidy the data
ge2017_tidy_df <- ge2017_constituency_df %>%
  filter(region_name != "Northern Ireland") %>%
  select(constituency_name, region_name,
         result, first_party, second_party,
         electorate, valid_votes, invalid_votes, majority,
         con, lab, ld, ukip, green, snp, pc, other, other_winner) %>%
  mutate(switch_votes = ceiling(0.5*(majority + 1)),
         majority_point = 100*majority/valid_votes)

## Produce counts
ge2017_tidy_df %>% filter(first_party == "Con",
                          second_party == "Lab") %>%
  top_n(n = 7, wt = -majority) %>%
  summarise(sum_majority = sum(majority),
            sum_switch_votes = sum(switch_votes))

ge2017_tidy_df %>% filter(second_party == "Con") %>%
  top_n(n = 4, wt = -majority) %>%
  summarise(sum_majority = sum(majority),
            sum_switch_votes = sum(switch_votes))

ge2017_tidy_df %>% filter(second_party == "Con") %>%
  top_n(n = 9, wt = -majority) %>%
  summarise(sum_majority = sum(majority),
            sum_switch_votes = sum(switch_votes))

## Make graphs
ge2017_2ndplace_gg <- ge2017_tidy_df %>%
  ggplot(aes(x = fct_rev(fct_infreq(second_party)),
             fill = second_party)) +
  geom_bar() +
  coord_flip() +
  geom_text(stat = "count",
            aes(label = stat(count),
                hjust = -1)) +
  scale_x_discrete(labels = c("NHAP", "Independent", "Green",
                              "Plaid Cymru", "Scottish National Party",
                              "Liberal Democrat","Conservative", "Labour")) +
  scale_fill_manual(values = c("#0087DC", "#6AB023", "#DDDDDD", "#DC241f",
                               "#FAA61A", "#0071BB", "#008142", "#FDF38E")) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(title = "In 2017, Labour had 301 second places, compared to 264 seats for the Conservatives",
       subtitle = "Second place constituency counts for 2017 General Election constituency in Great Britain",
       x = "",
       caption = "Data: House of Commons Library CBP-7979") +
  theme(legend.position = "none")

ge2017_con2nd_gg <- ge2017_tidy_df %>%
  filter(second_party == "Con") %>%
  ggplot(aes(x = majority, fill = first_party)) +
  geom_dotplot(method = "histodot",
               binwidth = 1000,
               origin = 0,
               stackgroups = TRUE,
               binaxis = "x",
               dotsize = 0.8,
               right = FALSE) +
  scale_fill_manual(values = c("#DC241f", "#FAA61A", "#008142", "#FDF38E")) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(title = "In 2017: of the 19 seats with Conservatives second by less than 1,000 votes, 15 were won by Labour.",
       subtitle = "Dot histogram of General Election 2017 constituency majorities, in Great British seats with the Conservatives in second place. Binwidths equal to 1,000 votes.",
       caption = "Data: House of Commons Library CBP-7979.",
       x = "Constituency Majority (Number of winning candidate votes minus second-place votes)")

ge2017_lab2nd_gg <- ge2017_tidy_df %>%
  filter(second_party == "Lab") %>%
  ggplot(aes(x = majority, fill = first_party)) +
  geom_dotplot(method = "histodot",
               binwidth = 1000,
               origin = 0,
               stackgroups = TRUE,
               binaxis = "x",
               dotsize = 0.7,
               right = FALSE) +
  scale_fill_manual(values = c("#0087DC", "#6AB023", "#008142", "#FDF38E")) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(title = "In 2017: of the 19 seats with Conservatives second by less than 1,000 votes, 15 were won by Labour.",
       subtitle = "Dot histogram of General Election 2017 constituency majorities, in Great British seats with the Conservatives in second place. Binwidths equal to 1,000 votes.",
       caption = "Data: House of Commons Library CBP-7979.",
       x = "Constituency Majority (Number of winning candidate votes minus second-place votes)")

ge2017_conpoint_gg <- ge2017_tidy_df %>%
  filter(second_party == "Con") %>%
  ggplot(aes(x = majority_point, fill = first_party)) +
  geom_dotplot(method = "histodot",
               binwidth = 1,
               origin = 0,
               stackgroups = TRUE,
               binaxis = "x",
               dotsize = 0.8,
               right = FALSE) +
  scale_fill_manual(values = c("#DC241f", "#FAA61A", "#008142", "#FDF38E")) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(title = "In 2017: nine seats with Conservatives in second had a majority of less than 0.53 points.",
       subtitle = "Dot histogram of General Election 2017 constituency majorities [pp], in Great British seats with the Conservatives in second place. Binwidths equal to 1.0 point.",
       caption = "Data: House of Commons Library CBP-7979.",
       x = "Constituency Majority (percentage point)")

ge2017_labpoint_gg <- ge2017_tidy_df %>%
  filter(second_party == "Lab") %>%
  ggplot(aes(x = majority_point, fill = first_party)) +
  geom_dotplot(method = "histodot",
               binwidth = 1,
               origin = 0,
               stackgroups = TRUE,
               binaxis = "x",
               dotsize = 0.8,
               right = FALSE) +
  scale_fill_manual(values = c("#0087DC", "#6AB023", "#008142", "#FDF38E")) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(title = "In 2017: seven Conservative-held seats with Labour in second had a majority of less than 0.64 points.",
       subtitle = "Dot histogram of General Election 2017 constituency majorities [pp], in Great British seats with the Conservatives in second place. Binwidths equal to 1.0 point.",
       caption = "Data: House of Commons Library CBP-7979.",
       x = "Constituency Majority (percentage point)")
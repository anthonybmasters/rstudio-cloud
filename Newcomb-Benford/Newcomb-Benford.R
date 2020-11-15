## Packages and Themes
library(tidyverse)
library(stringr)
library(readxl)

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

## Draw and summarise data
# As shown in Matt Parker's video: https://www.youtube.com/watch?v=etx0k1nLn78
# We download the Chicago 2020 US Presidential election file from here: https://chicagoelections.gov/en/election-results-specifics.asp

chicago_2020_df <- read_excel("Newcomb-Benford/Chicago BEC 2020 US Presidential Election Results - 2020-11-13.xlsx",
                              sheet = "DATA")

extract_first_digit <- function(x)
{
  stringr::str_extract(x, "\\d{1}") %>%
    as.numeric()
}

chicago_2020_df %>%
  filter(valid_votes >= 100 & valid_votes < 1000) %>%
  tally()
  
chicago_2020_df %>%
  mutate(democrat_share = 100*biden_harris/valid_votes) %>%
  group_by(group = cut(democrat_share,
                       breaks = seq(0, 100, 10))) %>%
  summarise(count = n())

chicago_nb_df <- chicago_2020_df %>%
  dplyr::select(biden_harris, trump_pence) %>%
  dplyr::rename("Biden & Harris (Democrat)" = biden_harris,
                "Trump & Pence (Republican)" = trump_pence) %>%
  dplyr::mutate_all(extract_first_digit) %>%
  pivot_longer(cols = 1:2,
                      names_to = "ticket",
                      values_to = "first_digit") %>%
  group_by(ticket, first_digit) %>%
  summarise(count = n()) %>%
  mutate(share = 100*count/sum(count),
         nb_dist = 100*log10(1 + 1/first_digit))

## Make graphs
chicago_graph_caption <- "Author's calculations. Data: Chicago Board of Election Commissioners, 2020 US Presidential Election Results."

chicago_votebin_gg <- chicago_2020_df %>%
  ggplot(aes(x = valid_votes,
             fill = stat(x >= 100 & x < 1000))) +
  geom_histogram(binwidth = 10) +
  labs(title = "Over 97% of Chicago precincts counted between 100 and 1,000 votes in the 2020 US Presidential election.",
       subtitle = "Histogram of votes cast with bin widths of 10, counting Chicago precincts in the 2020 US Presidential election.",
       x = "Votes cast in precinct",
       y = "",
       caption = chicago_graph_caption) +
  theme(legend.position = "none")

chicago_scatter_gg <- chicago_2020_df %>%
  ggplot(aes(x = valid_votes,
             y = 100*biden_harris/valid_votes)) +
  geom_point(alpha = 0.2,
             color = "#0015BC") +
  labs(title = "In most Chicago precincts, the Democrat ticket had over 80% of cast votes.",
       subtitle = "For each Chicago precinct, the graph shows votes cast versus Democrat vote share [%] in the 2020 US Presidential election.",
       x = "Votes cast in precinct",
       y = "",
       caption = chicago_graph_caption) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 100),
                     expand = c(0, 0))

chicago_nb_gg <- chicago_nb_df %>%
  ggplot(aes(x = as.factor(first_digit),
             y = share,
             fill = ticket)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ticket) +
  geom_line(aes(x = first_digit,
                y = nb_dist)) +
  geom_point(aes(x = first_digit,
                 y = nb_dist),
             size = 2) +
  scale_y_continuous(expand = c(0,0),
                     limit = c(0, 50)) +
  scale_fill_manual(values = c("#0015BC", "#E9141D")) +
  labs(title = "Precinct vote counts are not compatible with Benford's Law.",
       subtitle = "The share [%] of Chicago precincts with leading digits for each party's vote counts. The black line and points represents the Newcomb-Benford distribution.",
       x = "Leading digit",
       y = "",
       caption = chicago_graph_caption) +
  theme(legend.position = "none")
## Load packages and set themes
  # First, we load the tidyverse set of packages
library(tidyverse)

  # Next, we set the plotting theme for the graphs
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

## Create the data frame
  # We want to look at the probability of many independent trials having at least one event
outcomes <- (50*49*48*47*46*20)/(5*4*3*2*1*1)
end_const <- 15
start_or_end_const <- 35
involving_const <- 105

  # Next, we create the tibble
sa_powerball_df <- tibble(draws = 1:10000) %>%
  mutate(
    "Ending with a PowerBall" = 1 - (1-end_const/outcomes)^draws,
    "Starting or ending with a PowerBall" = 1 - (1-start_or_end_const/outcomes)^draws,
    "Involving a PowerBall" = 1 - (1-involving_const/outcomes)^draws
    ) %>%
  pivot_longer(
    cols = 2:4,
    names_to = "Events",
    values_to = "Probability") %>%
  mutate(Events = fct_relevel(Events, "Involving a PowerBall", after = Inf))

## Make the graph
  # Finally, we create the graph
sa_powerball_gg <- ggplot(data = sa_powerball_df,
    aes(x = draws,
        y = 100*Probability,
        group = Events)) +
  geom_line(aes(color = Events),
            size = 1.5) +
  labs(title = "With more lottery draws, the probability of a numerical sequence increases.",
       subtitle = "The probability [%] of a successive numerical sequence in the South African PowerBall lottery draw. This is shown by number of draws.",
       caption = "Author's calculations. [The South African PowerBall lottery draws five balls from a pool of 50 (numbered 1 to 50), with a PowerBall pool of 20 (numbered 1 to 20).]",
       x = "Number of independent draws",
       y = "",
       color = "A numerical sequence...") +
  scale_x_continuous(expand = c(0,0),
                     limits = c(0, NA)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, NA))
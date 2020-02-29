## Install packages
library(tidyverse)
library(tidydice)
library(gganimate)
library(gifski)
library(png)

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

## Create table
dice_roll_df <- roll_dice(times = 100, seed = 1210) %>%
  mutate(c_result = cumsum(result), model = 3.5*nr)
dice_tidy_df <- dice_roll_df %>%
  select(nr, c_result, model) %>%
  pivot_longer(cols = 2:3,
               names_to = "Type",
               values_to = "Value")

cor(dice_roll_df$c_result, dice_roll_df$model)

## Create graphs
dice_roll_gg <- dice_roll_df %>% ggplot(aes(x = nr, y = factor(result))) +
  geom_point(aes(color = factor(result)), size = 2) +
  scale_fill_brewer(palette = "Spectral") +
  theme(legend.position = "none") +
  labs(title = "There is no pattern to these 100 dice rolls.",
       subtitle = "100 rolls in the tidydice package.",
       caption = "Source: tidydice package, seed set to 1210.",
       x = "Number of Rolls",
       y = "")

dice_model_gg <- dice_tidy_df %>%
  ggplot(aes(x = nr, y = Value, group = Type)) +
  geom_line(aes(colour = Type, linetype = Type), size = 1.2) +
  labs(title = "Summing dice roll results is close to expectation.",
       subtitle = "Cumulative sums of dice rolls, compared to modelled values (3.5 times total rolls).",
       x = "Number of Rolls",
       y = "",
       caption = "Source: tidydice package, with seed set to 1210.") 

dice_model_gif <- dice_tidy_df %>%
  ggplot(aes(x = nr, y = Value, group = Type)) +
  geom_line(aes(colour = Type, linetype = Type), size = 1.2) +
  geom_point(aes(colour = Type), size = 1.4) +
  labs(title = "Summing dice roll results is close to expectation.",
       subtitle = "Cumulative sums of dice rolls, compared to modelled values.",
       x = "Number of Rolls",
       y = "",
       caption = "Source: tidydice package (seed: 1210), with the model equal to 3.5 times the number of rolls.") +
  transition_reveal(nr)
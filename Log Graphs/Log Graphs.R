#Load packages
library(tidyverse)
library(gganimate)
library(gifski)
library(png)
library(magick)

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

## Make the table
doubling_df <- tibble::enframe(1:12, name = NULL,
                               value = "x") %>%
  mutate(y = 2^x)

## Make the GIFs
linear_agg <- doubling_df %>%
  ggplot(aes(x = x, y = y)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10, 12)) +
  scale_y_continuous(limits = c(1, 5000), trans = "identity") +
  labs(title = "This is exponential growth.",
       subtitle = "The series doubles each day, shown on a linear scale.",
       x = "Day",
       y = "") +
  transition_reveal(x)
linear_gif <- animate(linear_agg, height = 360, width = 360,
                      nframes = 48, fps = 8)

log_agg <- doubling_df %>%
  ggplot(aes(x = x, y = y)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10, 12)) + 
  scale_y_continuous(limits = c(1, 5000), trans = "log10") +
  labs(title = "On a log scale, the line is straight.",
       subtitle = "The series doubles each day, shown on a logarithmic scale.",
       x = "Day",
       y = "") +
  transition_reveal(x)
log_gif <- animate(log_agg, height = 360, width = 360,
                   nframes = 48, fps = 8)

linear_mgif <- image_read(linear_gif)
log_mgif <- image_read(log_gif)

new_gif <- image_append(c(linear_mgif[1], log_mgif[1]))
  for(i in 2:48){
        combined <- image_append(c(linear_mgif[i], log_mgif[i]))
        new_gif <- c(new_gif, combined)
  }
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

## Create data
fpr <- 0.01
fnr <- 0.3
population <- 10000

positivity_df <- tibble(prevalence = 0:1000,
                        true_positive = round(prevalence*(1-fnr)),
                        false_positive = round((population-prevalence)*fpr)) %>%
  pivot_longer(cols = 2:3,
               names_to = "status",
               values_to = "positive")

positivity_df$status = factor(positivity_df$status,
                              levels = c("true_positive", "false_positive"))

## Make the graph
positivity_subtitle = paste0("Central estimates of positivity [%] for a test with a false positive rate of ",
                             fpr*100, "% and a false negative rate of ",
                             fnr*100, "%.")
positivity_caption = paste0("Illustrative central estimates of a test with specificity ",
                            (1-fpr)*100,
                            "% (false positive rate: ", fpr*100,
                            "%) and sensitivity ", (1-fnr)*100,
                            "% (false negative rate: ", fnr*100, "%).")

positivity_gg <- ggplot(data = positivity_df,
                        aes(x = prevalence/100,
                            y = positive/100,
                            fill = status)) + 
  geom_area() + 
  labs(title = "As prevalence rises, average true positives increase and false positives reduce.",
       subtitle = positivity_subtitle,
       caption = positivity_caption,
       x = "Prevalence [%]",
       y = "") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,10)) +
  scale_fill_discrete(name = "Status",
                      labels = c("True positive", "False positive")) +
  annotate("text", x = 0.1, y = 5,
           label = "With low prevalence, false positives outnumber true ones.",
           hjust = -0.05, size = 5) +
  annotate("curve", x = 0.4, xend = 0.4,
           y = 4.8, yend = 1.5,
           curvature = 0.3, arrow = arrow(length = unit(2, "mm")),
           color = "#00BFC4") +
  annotate("text", x = 5, y = 8,
           label = "With higher prevlance, true positives increase\n and false positives decline.",
           hjust = -0.05, size = 5) +
  annotate("curve", x = 5.3, xend = 5.3,
           y = 7.5, yend = 5,
           curvature = 0.3, arrow = arrow(length = unit(2, "mm")),
           color = "#F8766D")
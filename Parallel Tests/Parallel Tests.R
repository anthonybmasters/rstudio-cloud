## Install packages
library(tidyverse)

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

## Create the table
n_A = 10000
n_B = 10000
pvalue_diff_df <- tibble(a = 1:100, b = 1:100) %>%
  expand(a, b) %>%
  mutate(a = 10*a,
         b = 10*b,
         prop_A = a/n_A,
         prop_B = b/n_B,
         ssd_A = sqrt(prop_A*(1-prop_A)*n_A/(n_A-1)),
         ssd_B = sqrt(prop_B*(1-prop_B)*n_B/(n_B-1)),
         se_A = sqrt(prop_A*(1-prop_A)/n_A),
         se_B = sqrt(prop_B*(1-prop_B)/n_B),
         t_pooledse = ((n_A-1)*ssd_A^2 + (n_B-1)*ssd_B^2)*(1/n_A + 1/n_B)/(n_A+n_B-2),
         t_value = (prop_B-prop_A)/sqrt(t_pooledse),
         t_pvalue = 2*pt(-abs(t_value), df = n_A + n_B - 2),
         lift_var = (prop_B/prop_A)^2 * (se_A^2 / prop_A^2 + se_B^2 / prop_B^2),
         lift_value = (prop_B/prop_A - 1)/sqrt(lift_var),
         lift_pvalue = 2*pnorm(-abs(lift_value)),
         diff_pvalue = t_pvalue - lift_pvalue)

## Create the graph

pvalue_diff_gg <- pvalue_diff_df %>%
  ggplot(aes(x = prop_A*100, y = prop_B*100)) +
  geom_raster(aes(fill = diff_pvalue), interpolate = TRUE) +
  scale_fill_gradient2(limits = c(-0.1, 0.1),
                       midpoint = 0, mid = "white", low = "red", high = "blue") +
  coord_fixed(ratio = 1) +
  theme(legend.text = element_text(size = 8),
        legend.title = element_blank()) +
  labs(title = "Parallel tests, parallel p-values, parallel conclusions.",
       subtitle = str_wrap("Difference between two-sided p-values of: t-test of difference of proportions, and Normal approximation to the ratio (or lift)."),
       x = "Proportion: A [%]",
       y = "Proportion: B [%]",
       caption = "Two-sided hypothesis tests of proportions, with 10,000 units in samples A and B.")